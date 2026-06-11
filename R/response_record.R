# response_record.R -----------------------------------------------------------
# One flattening path from a generative call to a single provenance row. Every
# downstream package that keeps a record of what a model returned should reach
# for this, so the columns are the same in LLMRcoder, LLMRvalid, LLMRpanel,
# LLMRAgent, FocusGroup, and the archives that seal them. A caught error is a
# row too -- success = FALSE -- never a dropped call.

#' Flatten one LLM response to a provenance row
#'
#' Turns a single [llmr_response] (or a caught error) into a one-row tibble with
#' the columns the LLMR family treats as the response contract:
#' `response_text`, `response_id`, `provider`, `model`, `model_version`,
#' `finish_reason`, `sent_tokens`, `rec_tokens`, `total_tokens`,
#' `reasoning_tokens`, `cached_tokens`, `success`, `error_message`,
#' `duration_s`, `created_at`, and `request_hash`. It is the canonical flattening
#' path: reuse it rather than reaching into a response's fields, so a sealed
#' archive or an audit reconstructs the same columns everywhere.
#'
#' A failure is a record, not a gap. Pass a caught condition (or any non-response
#' object) as `error`, or as `x`, and the row carries `success = FALSE` with the
#' message in `error_message`; the call is never silently dropped.
#'
#' @param x An [llmr_response]. If `x` is a condition (a caught error) it is
#'   treated as a failed call. Any other object yields a failed row whose
#'   message notes the unexpected class.
#' @param request The messages or prompt that produced `x`, used together with
#'   `config` to fill `request_hash` via [llm_request_hash()]. `NULL` leaves
#'   `request_hash` as `NA`.
#' @param config The [llm_config()] used for the call, for `request_hash` and to
#'   backfill `provider`/`model` when the response does not carry them.
#' @param error Optional caught condition to record instead of (or alongside) a
#'   missing response; when supplied, the row is a failure carrying its message.
#' @param started_at,ended_at Optional `POSIXct`/numeric timestamps. When both
#'   are given and the response does not report a duration, `duration_s` is their
#'   difference; `created_at` is `ended_at` when supplied.
#' @return A one-row tibble with the response-contract columns.
#' @seealso [llm_request_hash()], [llmr_response], [llm_usage()]
#' @examples
#' r <- structure(
#'   list(text = "Hello!", provider = "openai", model = "demo",
#'        model_version = NA_character_, finish_reason = "stop",
#'        usage = list(sent = 12L, rec = 5L, total = 17L,
#'                     reasoning = NA_integer_, cached = NA_integer_),
#'        response_id = "resp_123", duration_s = 0.012),
#'   class = "llmr_response")
#' llm_response_record(r)
#' # a caught error is a row too:
#' llm_response_record(simpleError("boom"))
#' @export
llm_response_record <- function(x, request = NULL, config = NULL,
                                error = NULL, started_at = NULL,
                                ended_at = NULL) {
  req_hash <- if (!is.null(request) || !is.null(config)) {
    llm_request_hash(config = config, messages = request)
  } else {
    NA_character_
  }

  created_at <- if (!is.null(ended_at)) {
    format(as.POSIXct(ended_at), "%Y-%m-%dT%H:%M:%S%z")
  } else {
    NA_character_
  }
  dur_from_stamps <- if (!is.null(started_at) && !is.null(ended_at)) {
    as.numeric(as.POSIXct(ended_at)) - as.numeric(as.POSIXct(started_at))
  } else {
    NA_real_
  }

  # A caught condition, given as `error` or as `x`, is a failed row.
  cond <- if (inherits(error, "condition")) error else
    if (inherits(x, "condition")) x else NULL

  if (!is.null(cond) || !inherits(x, "llmr_response")) {
    msg <- if (!is.null(cond)) conditionMessage(cond) else
      sprintf("Expected an llmr_response; got <%s>.",
              paste(class(x), collapse = ", "))
    return(tibble::tibble(
      response_text    = NA_character_,
      response_id      = NA_character_,
      provider         = as.character(config$provider %||% NA_character_),
      model            = as.character(config$model %||% NA_character_),
      model_version    = NA_character_,
      finish_reason    = NA_character_,
      sent_tokens      = NA_integer_,
      rec_tokens       = NA_integer_,
      total_tokens     = NA_integer_,
      reasoning_tokens = NA_integer_,
      cached_tokens    = NA_integer_,
      success          = FALSE,
      error_message    = as.character(msg),
      duration_s       = dur_from_stamps,
      created_at       = created_at,
      request_hash     = req_hash
    ))
  }

  u <- tokens(x)
  dur <- x$duration_s %||% NA_real_
  if (is.na(dur)) dur <- dur_from_stamps

  tibble::tibble(
    response_text    = as.character(x$text %||% NA_character_),
    response_id      = as.character(x$response_id %||% NA_character_),
    provider         = as.character(x$provider %||% config$provider %||% NA_character_),
    model            = as.character(x$model %||% config$model %||% NA_character_),
    model_version    = as.character(x$model_version %||% NA_character_),
    finish_reason    = as.character(finish_reason(x) %||% NA_character_),
    sent_tokens      = .as_int1(u$sent),
    rec_tokens       = .as_int1(u$rec),
    total_tokens     = .as_int1(u$total),
    reasoning_tokens = .as_int1(u$reasoning),
    cached_tokens    = .as_int1(u$cached),
    success          = TRUE,
    error_message    = NA_character_,
    duration_s       = as.numeric(dur),
    created_at       = created_at,
    request_hash     = req_hash
  )
}

# Internal: coerce a possibly-NULL/NA scalar to a length-1 integer.
.as_int1 <- function(v) {
  if (is.null(v) || length(v) != 1L) return(NA_integer_)
  suppressWarnings(as.integer(v))
}
