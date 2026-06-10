# batch_api.R -----------------------------------------------------------------
# Provider batch APIs: submit many requests as one asynchronous job, typically
# at half the per-token price, with results ready within minutes to 24 hours.
# This is DIFFERENT from row batching (.batch_size, several rows in one live
# request) and from call_llm_par() (many live requests in parallel): here the
# provider queues the job server-side and you fetch results later, so a session
# can be closed and resumed between submission and retrieval.
#
# Supported: OpenAI and Groq (the OpenAI Files+Batches protocol), Anthropic
# (Message Batches), and Gemini (batchGenerateContent with inline requests).

# ---- job object ---------------------------------------------------------------

.llmr_batch_job <- function(provider, config, batch_id, custom_ids,
                            extra = list()) {
  structure(
    c(list(provider = provider,
           config = config,
           batch_id = batch_id,
           custom_ids = custom_ids,
           n = length(custom_ids),
           submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
           llmr_version = as.character(utils::packageVersion("LLMR"))),
      extra),
    class = "llmr_batch_job"
  )
}

#' @export
print.llmr_batch_job <- function(x, ...) {
  cat(sprintf("<llmr_batch_job %s | %s | %d request(s) | submitted %s>\n",
              x$provider, x$batch_id, x$n, x$submitted_at))
  cat("  check:  llm_batch_status(job)\n  fetch:  llm_batch_fetch(job)\n")
  invisible(x)
}

# Persist/load: the job (including its config, whose api_key is an env-var
# handle, not a secret) round-trips through RDS.
.llmr_batch_save <- function(job, state_path) {
  if (!is.null(state_path)) {
    saveRDS(job, state_path)
  }
  job
}

.llmr_batch_load <- function(job) {
  if (inherits(job, "llmr_batch_job")) return(job)
  if (is.character(job) && length(job) == 1L && file.exists(job)) {
    out <- readRDS(job)
    if (!inherits(out, "llmr_batch_job")) stop("File does not contain an llmr_batch_job.", call. = FALSE)
    return(out)
  }
  stop("Pass an llmr_batch_job or the path to a saved one.", call. = FALSE)
}

# ---- shared helpers -----------------------------------------------------------

.batch_api_base <- function(config) {
  switch(config$provider,
    openai = "https://api.openai.com/v1",
    groq   = "https://api.groq.com/openai/v1",
    stop("Internal error: not an OpenAI-protocol batch provider.", call. = FALSE)
  )
}

.batch_api_auth <- function(config) {
  paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider,
                                   model = config$model))
}

.batch_api_perform <- function(req) {
  req <- httr2::req_timeout(req, getOption("llmr.timeout", 600))
  req <- httr2::req_error(req, is_error = \(resp) FALSE)
  resp <- httr2::req_perform(req)
  code <- httr2::resp_status(resp)
  if (code >= 400) {
    err <- try(httr2::resp_body_json(resp), silent = TRUE)
    msg <- if (!inherits(err, "try-error")) {
      err$error$message %||% err$error$type %||% "No message supplied"
    } else "No message supplied"
    .llmr_error(
      message = paste0("Batch API request failed (HTTP ", code, "): ", msg),
      category = if (code >= 500) "server" else if (code == 429) "rate_limit"
                 else if (code %in% c(401, 403)) "auth" else if (code == 400) "param" else "unknown",
      status_code = code
    )
  }
  resp
}

# normalize the per-row inputs into a list where each element is one request's
# worth of messages (a named character vector, a message list, or a plain string).
# An *unnamed* character vector becomes one element per string.
# A *named* character vector is treated as a single multi-role request.
.batch_inputs <- function(messages) {
  if (is.character(messages)) {
    messages <- if (is.null(names(messages))) as.list(messages) else list(messages)
  }
  if (!is.list(messages) || !length(messages)) {
    stop("`messages` must be a character vector or a non-empty list of message specs.",
         call. = FALSE)
  }
  messages
}

# Build one synthesized provider response into call_llm_par-style row fields.
.batch_row_from_content <- function(content) {
  tc <- .token_counts(content)
  list(
    response_text    = tryCatch(extract_text(content), error = function(e) NA_character_),
    finish_reason    = tryCatch(.std_finish_reason(content), error = function(e) NA_character_),
    sent_tokens      = as.integer(tc$sent %||% NA_integer_),
    rec_tokens       = as.integer(tc$rec %||% NA_integer_),
    total_tokens     = as.integer(
                         content$usage$total_tokens %||%
                         if (is.null(tc$sent) && is.null(tc$rec)) NA_integer_
                         else sum(c(tc$sent, tc$rec), na.rm = TRUE)),
    reasoning_tokens = as.integer(.reasoning_tokens_from(content) %||% NA_integer_),
    cached_tokens    = as.integer(.cached_tokens_from(content) %||% NA_integer_),
    response_id      = .response_id_from(content),
    raw_response_json = tryCatch(
      as.character(jsonlite::toJSON(content, auto_unbox = TRUE, null = "null")),
      error = function(e) NA_character_)
  )
}

.batch_empty_results <- function(n, custom_ids) {
  tibble::tibble(
    custom_id         = custom_ids,
    response_text     = rep(NA_character_, n),
    success           = rep(FALSE, n),
    error_message     = rep(NA_character_, n),
    finish_reason     = rep(NA_character_, n),
    sent_tokens       = rep(NA_integer_, n),
    rec_tokens        = rep(NA_integer_, n),
    total_tokens      = rep(NA_integer_, n),
    reasoning_tokens  = rep(NA_integer_, n),
    cached_tokens     = rep(NA_integer_, n),
    response_id       = rep(NA_character_, n),
    raw_response_json = rep(NA_character_, n)
  )
}

.batch_fill_row <- function(res, i, fields, success, error_message = NA_character_) {
  res$success[i]       <- success
  res$error_message[i] <- error_message
  for (nm in names(fields)) {
    v <- fields[[nm]]
    if (!is.null(v) && length(v) == 1L) res[[nm]][i] <- v
  }
  res
}

# ---- public API ----------------------------------------------------------------

#' Submit a batch job to a provider's batch API
#'
#' Provider batch APIs run large jobs asynchronously at a reduced price
#' (typically half) in exchange for delayed delivery (minutes up to 24 hours).
#' `llm_batch_submit()` packages one request per element of `messages` and
#' submits the job; [llm_batch_status()] polls it; [llm_batch_fetch()]
#' retrieves results as a tidy tibble aligned with the inputs.
#'
#' Supported providers: `"openai"` and `"groq"` (Files + Batches protocol),
#' `"anthropic"` (Message Batches), and `"gemini"` (`batchGenerateContent`
#' with inline requests; developer API, not Vertex). All request-shaping
#' features of [llm_config()] apply: sampling parameters, structured output,
#' tools, and hooks shape each request exactly as a live [call_llm()] would.
#'
#' The returned job object contains no secrets (the config stores an
#' environment-variable reference, not the key), so it can be saved to disk,
#' shared, and fetched later or from another machine with the same
#' environment variables set. Pass `state_path` to save it automatically.
#'
#' @param config An [llm_config] for a generative model.
#' @param messages An unnamed character vector (each element becomes one
#'   request's user message); a named character vector like
#'   `c(system = ..., user = ...)` (a single multi-role request); or a list
#'   with one element per request, where each element is any messages form
#'   accepted by [call_llm()]. Multimodal file parts are not supported in
#'   batch jobs.
#' @param state_path Optional file path; when given, the job object is also
#'   saved there as RDS (and the path is remembered for convenience).
#' @return An `llmr_batch_job` object.
#' @examples
#' \dontrun{
#' cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
#' job <- llm_batch_submit(cfg, c("2+2?", "Capital of Chile?"),
#'                         state_path = "my_batch.rds")
#' llm_batch_status(job)
#' # ... later, possibly in a new session:
#' res <- llm_batch_fetch("my_batch.rds")
#' }
#' @seealso [llm_batch_status()], [llm_batch_fetch()], [llm_batch_cancel()]
#' @export
llm_batch_submit <- function(config, messages, state_path = NULL) {
  stopifnot(inherits(config, "llm_config"))
  if (.is_embedding_config(config)) {
    stop("Batch jobs are supported for generative configs only.", call. = FALSE)
  }
  msgs <- .batch_inputs(messages)
  custom_ids <- sprintf("llmr-%06d", seq_along(msgs))

  job <- switch(config$provider,
    openai    = .batch_submit_openai(config, msgs, custom_ids),
    groq      = .batch_submit_openai(config, msgs, custom_ids),
    anthropic = .batch_submit_anthropic(config, msgs, custom_ids),
    gemini    = .batch_submit_gemini(config, msgs, custom_ids),
    stop("Provider '", config$provider, "' has no supported batch API in LLMR. ",
         "Supported: openai, groq, anthropic, gemini.", call. = FALSE)
  )
  if (!is.null(state_path)) job$state_path <- state_path
  .llmr_batch_save(job, state_path)
}

#' Check the status of a batch job
#'
#' @param job An `llmr_batch_job` from [llm_batch_submit()], or the path to one
#'   saved via `state_path`.
#' @return A one-row tibble: `provider`, `batch_id`, `status` (provider's
#'   wording; `"completed"`/`"ended"`/`"done"` mean ready), `n_total`,
#'   `n_completed`, `n_failed` (NA where a provider does not report counts).
#' @seealso [llm_batch_submit()], [llm_batch_fetch()]
#' @export
llm_batch_status <- function(job) {
  job <- .llmr_batch_load(job)
  switch(job$provider,
    openai    = .batch_status_openai(job),
    groq      = .batch_status_openai(job),
    anthropic = .batch_status_anthropic(job),
    gemini    = .batch_status_gemini(job)
  )
}

#' Fetch the results of a batch job
#'
#' Retrieves a finished job and returns one row per submitted request, in
#' submission order, with the same diagnostic columns as [call_llm_par()]
#' (response text, success, finish reason, token counts including cached
#' tokens, response id, raw JSON). Rows whose requests failed carry the
#' provider's error message. Parse structured replies afterwards with
#' [llm_parse_structured_col()] or [llm_parse_tags_col()], exactly as for
#' live results.
#'
#' @inheritParams llm_batch_status
#' @return A tibble with `custom_id` plus the diagnostic columns described
#'   above. If the job is not finished yet, an error is raised; check with
#'   [llm_batch_status()] first.
#' @export
llm_batch_fetch <- function(job) {
  job <- .llmr_batch_load(job)
  switch(job$provider,
    openai    = .batch_fetch_openai(job),
    groq      = .batch_fetch_openai(job),
    anthropic = .batch_fetch_anthropic(job),
    gemini    = .batch_fetch_gemini(job)
  )
}

#' Cancel a batch job
#'
#' @inheritParams llm_batch_status
#' @return The provider's response, invisibly.
#' @export
llm_batch_cancel <- function(job) {
  job <- .llmr_batch_load(job)
  resp <- switch(job$provider,
    openai = ,
    groq = {
      base <- .batch_api_base(job$config)
      httr2::request(paste0(base, "/batches/", job$batch_id, "/cancel")) |>
        httr2::req_headers(Authorization = .batch_api_auth(job$config)) |>
        httr2::req_method("POST") |>
        .batch_api_perform()
    },
    anthropic = {
      httr2::request(paste0("https://api.anthropic.com/v1/messages/batches/",
                            job$batch_id, "/cancel")) |>
        httr2::req_headers(
          "x-api-key" = .resolve_api_key(job$config$api_key, provider = "anthropic"),
          "anthropic-version" = "2023-06-01") |>
        httr2::req_method("POST") |>
        .batch_api_perform()
    },
    gemini = {
      httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/",
                            job$batch_id, ":cancel")) |>
        httr2::req_headers("x-goog-api-key" = .resolve_api_key(job$config$api_key, provider = "gemini")) |>
        httr2::req_method("POST") |>
        .batch_api_perform()
    }
  )
  invisible(httr2::resp_body_json(resp))
}

# ---- OpenAI / Groq (Files + Batches protocol) ----------------------------------

.batch_submit_openai <- function(config, msgs, custom_ids) {
  base <- .batch_api_base(config)
  spec <- .compat_stream_spec(config$provider)

  lines <- vapply(seq_along(msgs), function(i) {
    req <- .compat_chat_request(config, msgs[[i]],
                                endpoint = paste0(base, "/chat/completions"),
                                auth_style = "none",
                                drop_params = spec$drop)
    body <- req$body$data
    as.character(jsonlite::toJSON(list(
      custom_id = custom_ids[[i]],
      method = "POST",
      url = "/v1/chat/completions",
      body = body
    ), auto_unbox = TRUE, null = "null"))
  }, character(1))

  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lines, tmp, useBytes = TRUE)

  up <- httr2::request(paste0(base, "/files")) |>
    httr2::req_headers(Authorization = .batch_api_auth(config)) |>
    httr2::req_body_multipart(
      purpose = "batch",
      file = curl::form_file(tmp, type = "application/jsonl")
    ) |>
    .batch_api_perform() |>
    httr2::resp_body_json()

  created <- httr2::request(paste0(base, "/batches")) |>
    httr2::req_headers(Authorization = .batch_api_auth(config)) |>
    httr2::req_body_json(list(
      input_file_id = up$id,
      endpoint = "/v1/chat/completions",
      completion_window = "24h"
    )) |>
    .batch_api_perform() |>
    httr2::resp_body_json()

  .llmr_batch_job(config$provider, config, created$id, custom_ids,
                  extra = list(input_file_id = up$id))
}

.batch_status_openai <- function(job) {
  base <- .batch_api_base(job$config)
  j <- httr2::request(paste0(base, "/batches/", job$batch_id)) |>
    httr2::req_headers(Authorization = .batch_api_auth(job$config)) |>
    .batch_api_perform() |>
    httr2::resp_body_json()
  tibble::tibble(
    provider = job$provider, batch_id = job$batch_id,
    status = j$status %||% NA_character_,
    n_total = as.integer(j$request_counts$total %||% job$n),
    n_completed = as.integer(j$request_counts$completed %||% NA_integer_),
    n_failed = as.integer(j$request_counts$failed %||% NA_integer_)
  )
}

.batch_fetch_openai <- function(job) {
  base <- .batch_api_base(job$config)
  j <- httr2::request(paste0(base, "/batches/", job$batch_id)) |>
    httr2::req_headers(Authorization = .batch_api_auth(job$config)) |>
    .batch_api_perform() |>
    httr2::resp_body_json()
  if (!identical(j$status, "completed")) {
    stop("Batch job is not finished (status: ", j$status %||% "unknown",
         "). Poll with llm_batch_status().", call. = FALSE)
  }

  res <- .batch_empty_results(job$n, job$custom_ids)
  idx <- stats::setNames(seq_along(job$custom_ids), job$custom_ids)

  read_jsonl <- function(file_id) {
    if (is.null(file_id)) return(character(0))
    txt <- httr2::request(paste0(base, "/files/", file_id, "/content")) |>
      httr2::req_headers(Authorization = .batch_api_auth(job$config)) |>
      .batch_api_perform() |>
      httr2::resp_body_string()
    strsplit(txt, "\n", fixed = TRUE)[[1]]
  }

  for (ln in c(read_jsonl(j$output_file_id), read_jsonl(j$error_file_id))) {
    if (!nzchar(trimws(ln))) next
    rec <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(rec)) next
    i <- unname(idx[rec$custom_id %||% ""])
    if (is.null(i) || is.na(i)) next
    body <- rec$response$body
    if (!is.null(body) && (rec$response$status_code %||% 500) < 400 && is.null(rec$error)) {
      res <- .batch_fill_row(res, i, .batch_row_from_content(body), TRUE)
    } else {
      msg <- rec$error$message %||% body$error$message %||% "batch request failed"
      res <- .batch_fill_row(res, i, list(), FALSE, error_message = as.character(msg))
    }
  }
  res
}

# ---- Anthropic (Message Batches) ------------------------------------------------

.batch_submit_anthropic <- function(config, msgs, custom_ids) {
  requests <- lapply(seq_along(msgs), function(i) {
    req <- .anthropic_chat_request(config, msgs[[i]])
    list(custom_id = custom_ids[[i]], params = req$body$data)
  })
  created <- httr2::request("https://api.anthropic.com/v1/messages/batches") |>
    httr2::req_headers(
      "x-api-key" = .resolve_api_key(config$api_key, provider = "anthropic", model = config$model),
      "anthropic-version" = "2023-06-01") |>
    httr2::req_body_json(list(requests = requests)) |>
    .batch_api_perform() |>
    httr2::resp_body_json()
  .llmr_batch_job("anthropic", config, created$id, custom_ids)
}

.batch_status_anthropic <- function(job) {
  j <- httr2::request(paste0("https://api.anthropic.com/v1/messages/batches/", job$batch_id)) |>
    httr2::req_headers(
      "x-api-key" = .resolve_api_key(job$config$api_key, provider = "anthropic"),
      "anthropic-version" = "2023-06-01") |>
    .batch_api_perform() |>
    httr2::resp_body_json()
  rc <- j$request_counts
  tibble::tibble(
    provider = "anthropic", batch_id = job$batch_id,
    status = j$processing_status %||% NA_character_,
    n_total = job$n,
    n_completed = as.integer(rc$succeeded %||% NA_integer_),
    n_failed = as.integer(sum(c(rc$errored %||% 0L, rc$expired %||% 0L,
                                rc$canceled %||% 0L)))
  )
}

.batch_fetch_anthropic <- function(job) {
  hdr <- list(
    "x-api-key" = .resolve_api_key(job$config$api_key, provider = "anthropic"),
    "anthropic-version" = "2023-06-01")
  j <- httr2::request(paste0("https://api.anthropic.com/v1/messages/batches/", job$batch_id)) |>
    httr2::req_headers(!!!hdr) |>
    .batch_api_perform() |>
    httr2::resp_body_json()
  if (!identical(j$processing_status, "ended")) {
    stop("Batch job is not finished (status: ", j$processing_status %||% "unknown",
         "). Poll with llm_batch_status().", call. = FALSE)
  }
  txt <- httr2::request(j$results_url) |>
    httr2::req_headers(!!!hdr) |>
    .batch_api_perform() |>
    httr2::resp_body_string()

  res <- .batch_empty_results(job$n, job$custom_ids)
  idx <- stats::setNames(seq_along(job$custom_ids), job$custom_ids)
  for (ln in strsplit(txt, "\n", fixed = TRUE)[[1]]) {
    if (!nzchar(trimws(ln))) next
    rec <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(rec)) next
    i <- unname(idx[rec$custom_id %||% ""])
    if (is.null(i) || is.na(i)) next
    if (identical(rec$result$type, "succeeded")) {
      res <- .batch_fill_row(res, i, .batch_row_from_content(rec$result$message), TRUE)
    } else {
      msg <- rec$result$error$message %||% rec$result$type %||% "batch request failed"
      res <- .batch_fill_row(res, i, list(), FALSE, error_message = as.character(msg))
    }
  }
  res
}

# ---- Gemini (batchGenerateContent, inline mode) ---------------------------------

.batch_submit_gemini <- function(config, msgs, custom_ids) {
  if (.gemini_is_vertex(config)) {
    stop("Gemini batch jobs are supported on the developer API, not Vertex.", call. = FALSE)
  }
  requests <- lapply(seq_along(msgs), function(i) {
    req <- .gemini_chat_request(config, msgs[[i]])
    list(request = req$body$data, metadata = list(key = custom_ids[[i]]))
  })
  endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                     config$model, ":batchGenerateContent")
  created <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-goog-api-key" = .resolve_api_key(config$api_key, provider = "gemini", model = config$model)) |>
    httr2::req_body_json(list(batch = list(
      display_name = paste0("llmr-", format(Sys.time(), "%Y%m%d%H%M%S")),
      input_config = list(requests = list(requests = requests))
    ))) |>
    .batch_api_perform() |>
    httr2::resp_body_json()
  # the operation name ("batches/xxx") is the job handle
  .llmr_batch_job("gemini", config, created$name, custom_ids)
}

.gemini_batch_get <- function(job) {
  httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/", job$batch_id)) |>
    httr2::req_headers("x-goog-api-key" = .resolve_api_key(job$config$api_key, provider = "gemini")) |>
    .batch_api_perform() |>
    httr2::resp_body_json()
}

.batch_status_gemini <- function(job) {
  j <- .gemini_batch_get(job)
  state <- j$metadata$state %||% if (isTRUE(j$done)) "done" else "running"
  tibble::tibble(
    provider = "gemini", batch_id = job$batch_id,
    status = as.character(state),
    n_total = job$n, n_completed = NA_integer_, n_failed = NA_integer_
  )
}

.batch_fetch_gemini <- function(job) {
  j <- .gemini_batch_get(job)
  if (!isTRUE(j$done)) {
    stop("Batch job is not finished (state: ",
         j$metadata$state %||% "running", "). Poll with llm_batch_status().",
         call. = FALSE)
  }
  inlined <- tryCatch(j$response$inlinedResponses$inlinedResponses,
                      error = function(e) NULL)
  res <- .batch_empty_results(job$n, job$custom_ids)
  if (is.null(inlined)) {
    stop("This Gemini batch did not return inline responses; file-based output ",
         "is not supported by llm_batch_fetch() yet.", call. = FALSE)
  }
  idx <- stats::setNames(seq_along(job$custom_ids), job$custom_ids)
  for (item in inlined) {
    key <- item$metadata$key %||% ""
    i <- unname(idx[key])
    if (is.null(i) || is.na(i)) next
    if (!is.null(item$response)) {
      res <- .batch_fill_row(res, i, .batch_row_from_content(item$response), TRUE)
    } else {
      msg <- item$error$message %||% "batch request failed"
      res <- .batch_fill_row(res, i, list(), FALSE, error_message = as.character(msg))
    }
  }
  res
}
