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
#' @param log Path to a JSONL audit log.
#' @return A list with `records` (a list of `list(raw=, rec=)`, one per line) and
#'   `manifest` (a tibble with `idx`, `ts`, `kind`, `provider`, `model`,
#'   `model_version`, `status`, `schema_version`, `has_payload`, `request_hash`,
#'   `record_hash`).
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
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) rlang::abort("The log contains no records.")

  records <- lapply(seq_along(lines), function(i) {
    ln <- lines[[i]]
    rec <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(rec)) {
      rlang::abort(sprintf("Log line %d is not valid JSON; the file may be corrupted.", i))
    }
    list(raw = ln, rec = rec)
  })

  manifest <- do.call(rbind, lapply(seq_along(records), function(i) {
    r <- records[[i]]$rec
    tibble::tibble(
      idx            = i,
      ts             = as.character(r$ts %||% NA_character_),
      kind           = as.character(r$kind %||% NA_character_),
      provider       = as.character(r$provider %||% NA_character_),
      model          = as.character(r$model %||% NA_character_),
      model_version  = as.character(r$model_version %||% NA_character_),
      status         = as.integer(r$status %||% NA_integer_),
      schema_version = as.character(r$schema_version %||% NA_character_),
      has_payload    = !is.null(r$request) || !is.null(r$text),
      request_hash   = if (is.null(r$request)) NA_character_ else
                         llm_request_hash(
                           provider = r$provider, model = r$model,
                           messages = .llmr_messages_from_turns(
                             .llmr_turns(provider = r$provider, request = r$request)),
                           extra = list(params = .llmr_body_params(r$request))),
      record_hash    = .llmr_line_hash(records[[i]]$raw)
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
#'   reconstructor does not cover (so the rebuilt messages would be empty or
#'   lossy) -- chiefly the OpenAI Responses API (`input`/`instructions`) and
#'   non-text content blocks (images, files). `"error"` (default) aborts;
#'   `"warn"` warns and returns the best-effort result; `"quiet"` returns it
#'   silently. Failing loudly is the default because a silently empty replay is
#'   worse than none.
#' @return A list with `config` (an [llm_config()]), `messages` (a named
#'   character vector of role/content turns), and `complete` (a logical: `TRUE`
#'   when the body was fully reconstructed, `FALSE` when content was dropped).
#'   `NULL` when the record carries no request body.
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
  if (is.null(record$request)) return(NULL)
  req <- record$request

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

  list(config = cfg, messages = messages, complete = complete)
}
