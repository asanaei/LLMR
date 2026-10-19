# log_read.R --------------------------------------------------------------------
# Reading an LLMR audit log (the JSONL written by llm_log_enable()) back into a
# structured object: the parsed records plus a per-record manifest that carries,
# for each call, a record hash (over the verbatim line, for tamper evidence) and
# a request hash (the call's identity, computed the same way llm_request_hash()
# computes it from a config, so a logged call and a live call agree). This is the
# generic reader; building a sealed replication archive on top of it is a
# separate, downstream concern.

# SHA-256 over the raw bytes of a string (a log line), without R serialization,
# so the tamper-evidence hash is stable across R versions. Distinct from
# llm_hash(), which canonicalizes an object first.
#' @keywords internal
#' @noRd
.llmr_line_hash <- function(x) {
  digest::digest(x, algo = "sha256", serialize = FALSE)
}

#' Read an LLMR audit log into records and a manifest
#'
#' Parses a JSONL audit log written by [llm_log_enable()] into its records and a
#' per-record manifest tibble. Each manifest row carries the call's metadata, a
#' `record_hash` over the verbatim log line (tamper evidence), and, where the
#' request body was logged, a `request_hash` identifying the call. The
#' `request_hash` is computed through [llm_request_hash()] over the call's
#' canonical turns and generation parameters, so a call read from a log and the
#' same call described by a config hash identically.
#'
#' Reading is strict: a line that is not a JSON object, or a record whose
#' metadata fields are not single values, stops with an error that names the
#' line of the file, the field, and the reason. Blank lines are ignored but
#' still counted, so the line numbers match the file.
#'
#' @param log Path to a JSONL audit log.
#' @return A list with `records` (a list of `list(raw=, rec=)`, one per
#'   nonblank line) and `manifest` (a tibble with `idx`, `ts`, `kind`,
#'   `provider`, `model`, `model_version`, `status`, `schema_version`,
#'   `has_payload`, `request_hash`, `record_hash`, and `line`, the record's
#'   line number in the file).
#' @seealso [llm_request_hash()], [llm_log_enable()]
#' @examples
#' # In practice the log comes from llm_log_enable("study.jsonl"); here one
#' # record written by hand.
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0(
#'   '{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0","kind":"call",',
#'   '"provider":"openai","model":"gpt-4o-mini","status":200,',
#'   '"request":{"messages":[{"role":"user","content":"Capital of France?"}],',
#'   '"temperature":0},"usage":{"sent":5,"rec":1},',
#'   '"response_id":"r-1","text":"Paris"}'), log)
#' read <- llm_log_read(log)
#' read$manifest
#' @export
llm_log_read <- function(log) {
  if (!is.character(log) || length(log) != 1L || is.na(log)) {
    rlang::abort("`log` must be a single file path.")
  }
  if (!file.exists(log)) {
    rlang::abort(sprintf("Log file not found: %s", log))
  }
  lines <- readLines(log, warn = FALSE)
  line_no <- which(nzchar(trimws(lines)))
  lines <- lines[line_no]
  if (!length(lines)) rlang::abort("The log contains no records.")

  records <- lapply(seq_along(lines), function(i) {
    ln <- lines[[i]]
    rec <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE),
                    error = function(e) e)
    if (inherits(rec, "error")) {
      rlang::abort(sprintf("Log line %d is not valid JSON; the file may be corrupted.",
                           line_no[[i]]), parent = rec)
    }
    .llmr_check_log_record(rec, line_no[[i]])
    list(raw = ln, rec = rec)
  })

  manifest <- do.call(rbind, lapply(seq_along(records), function(i) {
    r <- records[[i]]$rec
    request_hash <- if (is.null(r[["request"]])) NA_character_ else
      tryCatch(
        llm_request_hash(
          provider = r$provider, model = r$model,
          messages = .llmr_turns(provider = r$provider, request = r[["request"]]),
          extra = list(params = .llmr_body_params(r[["request"]]))),
        error = function(e) rlang::abort(
          sprintf("Log line %d: field `request` cannot be read.", line_no[[i]]),
          parent = e))
    tibble::tibble(
      idx            = i,
      ts             = as.character(r$ts %||% NA_character_),
      kind           = as.character(r$kind %||% NA_character_),
      provider       = as.character(r$provider %||% NA_character_),
      model          = as.character(r$model %||% NA_character_),
      model_version  = as.character(r$model_version %||% NA_character_),
      status         = as.integer(r$status %||% NA_integer_),
      schema_version = as.character(r$schema_version %||% NA_character_),
      has_payload    = !is.null(r[["request"]]) || !is.null(r$text),
      request_hash   = request_hash,
      record_hash    = .llmr_line_hash(records[[i]]$raw),
      line           = line_no[[i]]
    )
  }))

  sv <- unique(stats::na.omit(manifest$schema_version))
  if (length(sv) > 1L) {
    cli::cli_warn("This log mixes schema versions ({.val {sv}}); records were kept as written.")
  } else if (!length(sv)) {
    cli::cli_warn("No schema_version in these records (an older LLMR wrote them); reading as schema 'pre-1.0'.")
  }

  list(records = records, manifest = manifest)
}

#' Rebuild a callable request from a logged record
#'
#' Turns one parsed log record (the `rec` element of a [llm_log_read()] entry)
#' back into the `config` and `messages` an experiments row needs, so an archived
#' call can be re-issued live or matched against a config-driven call. The
#' provider-specific request body is canonicalized to provider-neutral
#' role/content turns and its generation parameters are recovered onto a
#' reconstructed [llm_config()].
#'
#' @param record A parsed log record: a list with at least `provider`, `model`,
#'   and `request` (as found in `llm_log_read(x)$records[[i]]$rec`).
#' @param on_unsupported What to do when the request body uses a shape this
#'   reconstructor does not cover, so the rebuilt messages would be empty or
#'   lack a turn's content (`complete = FALSE`) -- chiefly the OpenAI Responses
#'   API (`input`/`instructions`) and turns whose only content is a non-text
#'   block. `"error"` (default) aborts; `"warn"` warns and returns the
#'   best-effort result; `"quiet"` returns it silently. Failing loudly is the
#'   default because a silently empty replay is worse than none.
#' @return A list with `config` (an [llm_config()]), `messages`, `complete`,
#'   and `reissuable`; `NULL` when the record carries no request body.
#'   `messages` is a character vector of turns named by role, or, when a turn
#'   carries non-text content, a list of turns whose attachments are kept only
#'   as hashes. `complete` is `TRUE` when every turn was recovered with its
#'   text and attachment hashes, which is what [llm_request_hash()] needs.
#'   `reissuable` is `TRUE` when the rebuilt messages can also be sent again as
#'   the original request: it is `FALSE` when the record has attachments (only
#'   their hashes survive), tool calls or tool results (which are dropped), or a
#'   role mapped with loss (such as `developer` to `system`).
#' @seealso [llm_log_read()], [llm_request_hash()]
#' @examples
#' rec <- list(provider = "openai", model = "gpt-4o-mini",
#'             request = list(
#'               messages = list(list(role = "user", content = "Hi")),
#'               temperature = 0))
#' req <- llm_request_from_log(rec)
#' req$messages
#' @export
llm_request_from_log <- function(record,
                                 on_unsupported = c("error", "warn", "quiet")) {
  on_unsupported <- match.arg(on_unsupported)
  if (is.null(record[["request"]])) return(NULL)
  req <- record[["request"]]

  turns <- .llmr_turns(provider = record$provider, request = req)
  messages <- .llmr_messages_from_turns(turns)
  params <- .llmr_body_params(req)
  cfg <- llm_config(record$provider %||% "", record$model %||% "")
  cfg$model_params <- utils::modifyList(cfg$model_params %||% list(), params)

  # The request carries content this reconstructor cannot recover when it has a
  # known content field but yielded no turns (e.g. the OpenAI Responses API
  # input/instructions, which .llmr_turns does not read), or when a logged turn
  # contributed empty content (a non-text block such as an image or file).
  has_content <- !is.null(req$input) || !is.null(req$instructions) ||
    !is.null(req$messages) || !is.null(req$contents)
  empty_turn <- length(turns) > 0 &&
    any(vapply(turns, function(t) !nzchar(t$content %||% ""), logical(1)))
  complete <- !((has_content && !length(turns)) || empty_turn)

  if (!complete && !identical(on_unsupported, "quiet")) {
    msg <- paste0(
      "This logged request uses a shape llm_request_from_log() cannot fully ",
      "reconstruct (an OpenAI Responses-API body, or non-text content blocks ",
      "such as images/files); the rebuilt messages are empty or lossy, so a ",
      "replay or hash from this record would be wrong. ",
      "Pass on_unsupported = \"warn\" or \"quiet\" to proceed anyway.")
    if (identical(on_unsupported, "error")) rlang::abort(msg)
    cli::cli_warn(msg)
  }

  list(config = cfg, messages = messages, complete = complete,
       reissuable = complete && .llmr_request_reissuable(req, turns))
}

# Internal: can the turns rebuilt from a logged body be sent again as the
# original request? Not when a turn carries attachments (only their hashes
# survive), when the body holds tool calls or tool results (dropped by the
# turn canonicalization), or when a role was mapped with loss.
.llmr_request_reissuable <- function(req, turns) {
  if (any(vapply(turns, function(t) length(t$non_text %||% character()) > 0L, logical(1)))) {
    return(FALSE)
  }
  msgs <- c(req$messages %||% list(), req$contents %||% list())
  tool_types <- c("tool_use", "tool_result", "server_tool_use", "web_search_tool_result")
  for (m in msgs) {
    if (!is.list(m)) next
    role <- tolower(as.character(m$role %||% "user")[1])
    if (!role %in% c("user", "assistant", "model", "system")) return(FALSE)
    if (!is.null(m$tool_calls) || !is.null(m$tool_call_id) || !is.null(m$function_call)) {
      return(FALSE)
    }
    blocks <- m$content %||% m$parts
    if (is.list(blocks)) {
      for (b in blocks) {
        if (is.list(b) && (isTRUE(b$type %in% tool_types) ||
                           !is.null(b$functionCall) || !is.null(b$functionResponse))) {
          return(FALSE)
        }
      }
    }
  }
  TRUE
}

# A log record must be a JSON object whose metadata fields, when present, are
# single values, and whose request, when present, is an object. Anything else
# stops here with the file's line number, rather than failing later with an
# unrelated error or a manifest built from the wrong shape.
.llmr_check_log_record <- function(rec, line) {
  if (!is.list(rec) || (length(rec) && is.null(names(rec)))) {
    what <- if (is.list(rec)) "an array" else sprintf("a %s value", class(rec)[1])
    rlang::abort(sprintf("Log line %d is %s, not a JSON object.", line, what))
  }
  scalar <- c("ts", "kind", "provider", "model", "model_version",
              "schema_version", "status")
  for (f in intersect(scalar, names(rec))) {
    v <- rec[[f]]
    if (is.null(v)) next
    if (is.list(v) || length(v) != 1L) {
      rlang::abort(sprintf("Log line %d: field `%s` must be a single value, not %s.",
                           line, f, if (is.list(v)) "an object or array"
                                    else sprintf("%d values", length(v))))
    }
  }
  st <- rec[["status"]]
  if (!is.null(st) && is.na(suppressWarnings(as.integer(st))) && !is.na(st)) {
    rlang::abort(sprintf("Log line %d: field `status` must be an HTTP status number, not %s.",
                         line, encodeString(as.character(st), quote = '"')))
  }
  req <- rec[["request"]]
  if (!is.null(req) && (!is.list(req) || (length(req) && is.null(names(req))))) {
    rlang::abort(sprintf("Log line %d: field `request` must be a JSON object.", line))
  }
  invisible(TRUE)
}
