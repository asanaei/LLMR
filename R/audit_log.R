# audit_log.R ----------------------------------------------------------------
# A per-call audit trail for reproducible research.
#
# When enabled, every API call made through perform_request() appends one JSON
# record to a local JSONL file: timestamp, provider, model, the request body,
# the response text, token usage, identifiers, and timing. The format is one
# self-contained JSON object per line, so the file can be read back with
# jsonlite::stream_in(), inspected with command-line tools, or archived as
# supplementary material alongside a paper.

#' Record every LLM call in a local audit log
#'
#' `llm_log_enable()` turns on a session-wide audit log: each API call made
#' through LLMR (including those issued by [llm_fn()], [llm_mutate()],
#' [call_llm_par()], and [chat_session()]) appends one JSON object to `path`.
#' `llm_log_disable()` turns logging off. `llm_log_status()` reports the
#' current destination, if any.
#'
#' @details
#' Methodological guidance for LLM-assisted research asks authors to retain,
#' for every call: the model and provider, the full prompt, the inference
#' settings, the output, and identifiers that allow an exact lookup later.
#' The audit log records precisely that:
#'
#' - `ts`: ISO-8601 timestamp with timezone.
#' - `schema_version`: the version of this record layout (currently `"1.0"`),
#'   so downstream tools that parse the log can rely on a stable contract.
#' - `provider`, `model`: as configured; `model_version`: the identifier the
#'   server reports having served (when echoed), which catches silent model
#'   updates.
#' - `request`: the JSON body sent to the provider, including all sampling
#'   parameters and the rendered messages. Inline file data (base64) is
#'   replaced by a short placeholder so logs stay small.
#' - `text`, `finish_reason`, `usage`: the reply, why it stopped, and token
#'   counts (including cached tokens when reported).
#' - `response_id`, `status`, `duration_s`: provider request id, HTTP status,
#'   and wall-clock seconds.
#' - Failed calls are logged too (`kind = "error"`), with the provider's error
#'   message.
#'
#' Records are appended line by line. Under multisession parallel execution,
#' workers write per-process shard files and [llm_log_merge()] folds those
#' shards back into the main log after the parallel call completes. The log
#' contains your prompts and the model's replies in clear text. It never
#' contains API keys.
#'
#' Set `include_messages = FALSE` to omit request bodies and reply text
#' (keeping only metadata, parameters, usage, and identifiers), e.g. when
#' prompts contain confidential data.
#'
#' @param path File path for the log. For `llm_log_enable()`, created on first
#'   write and appended to if it exists; for `llm_log_merge()`, the base log
#'   path whose per-process shard files should be folded in.
#' @param include_messages Logical. If `TRUE` (default), the request body and
#'   the reply text are stored. If `FALSE`, only metadata is stored.
#' @return `llm_log_enable()` and `llm_log_disable()` return the previous log
#'   path invisibly. `llm_log_status()` returns the active path or `NULL`,
#'   invisibly, after printing a one-line status.
#' @seealso [llm_usage()] for token summaries, [llm_methods_text()] for a
#'   draft methods paragraph.
#' @export
#' @examples
#' \dontrun{
#' llm_log_enable("annotation_run.jsonl")
#' cfg <- llm_config("groq", "openai/gpt-oss-20b")
#' call_llm(cfg, "One word: capital of France?")
#' llm_log_disable()
#'
#' # Read the log back as a data frame
#' log_df <- jsonlite::stream_in(file("annotation_run.jsonl"), verbose = FALSE)
#' }
llm_log_enable <- function(path = "llmr_log.jsonl", include_messages = TRUE) {
  stopifnot(is.character(path), length(path) == 1L, nzchar(path))
  old <- getOption("llmr.log_file")
  options(
    llmr.log_file     = path,
    llmr.log_messages = isTRUE(include_messages)
  )
  invisible(old)
}

#' @rdname llm_log_enable
#' @export
llm_log_disable <- function() {
  old <- getOption("llmr.log_file")
  options(llmr.log_file = NULL)
  invisible(old)
}

#' @rdname llm_log_enable
#' @export
llm_log_status <- function() {
  p <- getOption("llmr.log_file")
  if (is.null(p)) {
    cat("LLMR audit log: disabled\n")
  } else {
    cat("LLMR audit log: writing to", p,
        if (isTRUE(getOption("llmr.log_messages", TRUE))) "(messages included)\n"
        else "(metadata only)\n")
  }
  invisible(p)
}

#' @rdname llm_log_enable
#' @export
#' @return `llm_log_active()` returns a list with `active` (logical), `path`
#'   (the destination or `NULL`), and `include_messages` (logical), for code that
#'   needs to read the logging state without printing.
llm_log_active <- function() {
  p <- getOption("llmr.log_file")
  list(
    active           = !is.null(p),
    path             = p,
    include_messages = isTRUE(getOption("llmr.log_messages", TRUE))
  )
}

#' @rdname llm_log_enable
#' @export
llm_log_merge <- function(path) {
  .llmr_log_merge(path)
}

# Escape a file basename so it can be matched literally inside list.files().
.llmr_re_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

.llmr_log_shard_path <- function(path, pid = Sys.getpid()) {
  paste0(path, ".", pid)
}

.llmr_log_path <- function(path) {
  if (isTRUE(getOption("llmr.log_parallel", FALSE))) {
    return(.llmr_log_shard_path(path))
  }
  path
}

.llmr_log_merge <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    rlang::abort("`path` must be a single non-empty file path.")
  }

  log_dir <- dirname(path)
  if (!dir.exists(log_dir)) return(invisible(character()))

  shard_pattern <- paste0("^", .llmr_re_escape(basename(path)), "\\.[0-9]+$")
  shard_names <- sort(list.files(log_dir, pattern = shard_pattern))
  if (!length(shard_names)) return(invisible(character()))

  # Rebuild the shard path exactly as the writer formed it (base path + suffix),
  # so the return value matches `.llmr_log_shard_path()` regardless of how
  # list.files() normalizes separators (Windows returns forward slashes).
  shards <- paste0(path, ".", sub("^.*\\.([0-9]+)$", "\\1", shard_names))
  read_from <- file.path(log_dir, shard_names)

  for (i in seq_along(shards)) {
    lines <- readLines(read_from[i], warn = FALSE)
    if (length(lines)) {
      con <- file(path, open = "a")
      tryCatch({
        writeLines(lines, con = con, useBytes = TRUE)
      }, finally = {
        close(con)
      })
    }
    unlink(read_from[i])
  }

  invisible(shards)
}

# Internal: replace long inline payloads (base64 file data) so logs stay small.
.llmr_log_scrub <- function(x) {
  if (is.character(x) && length(x) == 1L && nchar(x) > 4096L &&
      (grepl("^data:", x) || grepl("^[A-Za-z0-9+/=\r\n]+$", substr(x, 1, 512)))) {
    return(sprintf("<inline data omitted: %d chars>", nchar(x)))
  }
  if (is.list(x)) return(lapply(x, .llmr_log_scrub))
  x
}

# Internal: append one record to the audit log, if enabled. Never errors:
# logging must not be able to break a call that succeeded.
.llmr_log_event <- function(kind,
                            provider = NULL, model = NULL, status = NULL,
                            request = NULL, response = NULL,
                            error_message = NULL, duration_s = NULL) {
  path <- getOption("llmr.log_file")
  if (is.null(path)) return(invisible(NULL))
  path <- .llmr_log_path(path)
  ok <- try({
    include_msgs <- isTRUE(getOption("llmr.log_messages", TRUE))
    rec <- list(
      ts             = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      schema_version = "1.0",
      llmr_version   = as.character(utils::packageVersion("LLMR")),
      kind           = kind,
      provider     = provider,
      model        = model,
      status       = status
    )
    if (!is.null(request) && include_msgs) {
      body <- tryCatch(request$body$data, error = function(e) NULL)
      if (!is.null(body)) rec$request <- .llmr_log_scrub(body)
    }
    if (inherits(response, "llmr_response")) {
      rec$model_version <- response$model_version
      rec$finish_reason <- response$finish_reason
      rec$usage         <- response$usage
      rec$response_id   <- response$response_id
      rec$duration_s    <- response$duration_s
      if (include_msgs) rec$text <- response$text
    }
    if (!is.null(error_message)) rec$error <- error_message
    if (!is.null(duration_s) && is.null(rec$duration_s)) rec$duration_s <- duration_s
    line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", na = "null")
    cat(line, "\n", sep = "", file = path, append = TRUE)
  }, silent = TRUE)
  invisible(NULL)
}
