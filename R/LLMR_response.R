# LLMR_response.R ============================================================
# Constructor + helpers + S3 for a proper response object for generative calls

#' LLMR Response Object
#'
#' A lightweight S3 container for **generative** model calls. It standardizes
#' finish reasons and token usage across providers and keeps the raw response
#' for advanced users.
#'
#' ## Fields
#' - `text`: character scalar. Assistant reply.
#' - `provider`: character. Provider id (e.g., `"openai"`, `"gemini"`).
#' - `model`: character. Model id.
#' - `finish_reason`: one of `"stop"`, `"length"`, `"filter"`, `"tool"`, `"other"`.
#' - `usage`: list with integers `sent`, `rec`, `total`, `reasoning` (if available).
#' - `response_id`: provider’s response identifier if present.
#' - `duration_s`: numeric seconds from request to parse.
#' - `raw`: parsed provider JSON (list).
#' - `raw_json`: raw JSON string.
#'
#' ## Printing
#' `print()` shows the text, then a compact status line with model, finish reason,
#' token counts, and a terse hint if truncated or filtered.
#'
#' ## Coercion
#' `as.character()` extracts `text` so the object remains drop-in for code that
#' expects a character return.
#'
#' @section See also:
#' [call_llm()], [call_llm_robust()], [llm_chat_session()],
#' [llm_config()], [llm_mutate()], [llm_fn()]
#'
#' @name llmr_response
#'
#' @examples
#' # Minimal fabricated example (no network):
#' r <- structure(
#'   list(
#'     text = "Hello!",
#'     provider = "openai",
#'     model = "demo",
#'     finish_reason = "stop",
#'     usage = list(sent = 12L, rec = 5L, total = 17L, reasoning = NA_integer_),
#'     response_id = "resp_123",
#'     duration_s = 0.012,
#'     raw = list(choices = list(list(message = list(content = "Hello!")))),
#'     raw_json = "{}"
#'   ),
#'   class = "llmr_response"
#' )
#' as.character(r)
#' finish_reason(r)
#' tokens(r)
#' print(r)
NULL


# ---------------------------------------------------------------------------
# Constructor (kept internal; created by perform_request())
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_llmr_response <- function(text,
                              provider = NA_character_,
                              model = NA_character_,
                              finish_reason = NA_character_,
                              usage = list(sent = NA_integer_,
                                           rec = NA_integer_,
                                           total = NA_integer_,
                                           reasoning = NA_integer_),
                              response_id = NA_character_,
                              duration_s = NA_real_,
                              raw = NULL,
                              raw_json = NULL) {
  structure(
    list(
      text          = as.character(text %||% NA_character_),
      provider      = provider,
      model         = model,
      finish_reason = finish_reason,
      usage         = usage,
      response_id   = response_id,
      duration_s    = duration_s,
      raw           = raw,
      raw_json      = raw_json
    ),
    class = "llmr_response"
  )
}

# ---------------------------------------------------------------------------
# Public helpers
# ---------------------------------------------------------------------------

#' Get finish reason
#'
#' Returns the standardized finish reason for an \code{llmr_response}.
#'
#' @param x An \code{llmr_response} object.
#' @return A length-1 character vector or \code{NA_character_}.
#' @examples
#' \dontrun{
#' fr <- finish_reason(r)
#' }
#' @rdname llmr_response
#' @export
finish_reason <- function(x) {
  if (inherits(x, "llmr_response")) x$finish_reason else NA_character_
}

#' Get token usage
#'
#' Returns a list with token counts for an \code{llmr_response}.
#'
#' @param x An \code{llmr_response} object.
#' @return A list \code{list(sent, rec, total, reasoning)}. Missing values are \code{NA}.
#' @examples
#' \dontrun{
#' u <- tokens(r)
#' u$total
#' }
#' @rdname llmr_response
#' @export
tokens <- function(x) {
  if (!inherits(x, "llmr_response")) return(list(sent = NA, rec = NA, total = NA, reasoning = NA))
  x$usage
}

#' Did the response hit a length cap?
#'
#' Convenience check for truncation due to token limits.
#'
#' @param x An \code{llmr_response} object.
#' @return \code{TRUE} if truncated, otherwise \code{FALSE}.
#' @examples
#' \dontrun{
#' if (is_truncated(r)) message("Increase max_tokens")
#' }
#' @rdname llmr_response
#' @export
is_truncated <- function(x) identical(finish_reason(x), "length")

# ---------------------------------------------------------------------------
# S3 methods
# ---------------------------------------------------------------------------

#' @rdname llmr_response
#' @param ... Ignored.
#' @exportS3Method as.character llmr_response
as.character.llmr_response <- function(x, ...) x$text

#' @rdname llmr_response
#' @param ... Ignored.
#' @exportS3Method print llmr_response
print.llmr_response <- function(x, ...) {
  txt <- x$text %||% ""
  cat(txt, "\n", sep = "")

  # Minimal, actionable status line
  u  <- x$usage
  fr <- x$finish_reason %||% "other"
  hint <- switch(fr,
                 length = "Hit token limit; raise max_tokens.",
                 filter = "Content filtered; adjust prompt/safety.",
                 tool   = "Tool call; supply tool handler.",
                 "stop" = "",
                 "other"= "")
  cat(sprintf("[model=%s | finish=%s | sent=%s rec=%s tot=%s | t=%.3fs] %s\n",
              x$model %||% "?", fr,
              u$sent %||% NA_integer_, u$rec %||% NA_integer_, u$total %||% NA_integer_,
              x$duration_s %||% NA_real_, hint))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Internal: canonicalization from provider JSON
# ---------------------------------------------------------------------------

#' Map provider finish signals to standard labels
#'
#' Normalizes heterogeneous finish indicators to one of
#' \code{c("stop","length","filter","tool","other")}.
#'
#' @keywords internal
#' @noRd
.std_finish_reason <- function(content) {
  # OpenAI/Groq: choices[[1]]$finish_reason
  fr <- NULL
  if (!is.null(content$choices) && length(content$choices) >= 1) {
    fr <- content$choices[[1]]$finish_reason %||% content$choices[[1]]$finishReason
  }
  # Gemini: candidates[[1]]$finishReason
  if (is.null(fr) && !is.null(content$candidates) && length(content$candidates) >= 1) {
    fr <- content$candidates[[1]]$finishReason
  }
  # Anthropic: stop_reason
  if (is.null(fr) && !is.null(content$stop_reason)) {
    fr <- content$stop_reason
  }

  fr <- tolower(as.character(fr %||% ""))
  if (!nzchar(fr)) return("other")
  if (fr %in% c("stop","end_turn","completed","done")) return("stop")
  if (fr %in% c("length","max_tokens","max_token","maxoutputtokens","max_completion_tokens")) return("length")
  if (fr %in% c("content_filter","safety","blocked")) return("filter")
  if (fr %in% c("tool_calls","tool_use","tool","function_call")) return("tool")
  "other"
}

#' Extract a response id if present
#' @keywords internal
#' @noRd
.response_id_from <- function(content) {
  # OpenAI/Groq
  if (!is.null(content$id)) return(as.character(content$id))
  # Gemini
  if (!is.null(content$responseId)) return(as.character(content$responseId))
  NA_character_
}

#' Extract provider reasoning token counts if present
#' @keywords internal
#' @noRd
.reasoning_tokens_from <- function(content) {
  # OpenAI
  rt <- tryCatch(
    content$usage$completion_tokens_details$reasoning_tokens,
    error = function(e) NA_integer_
  )
  if (!is.null(rt) && !is.na(rt)) return(as.integer(rt))
  # Gemini
  rt <- tryCatch(content$usageMetadata$thoughtsTokenCount, error = function(e) NA_integer_)
  if (!is.null(rt) && !is.na(rt)) return(as.integer(rt))
  NA_integer_
}
