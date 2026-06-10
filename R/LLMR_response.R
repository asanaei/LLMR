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
#' - `model`: character. Model id as requested in the config.
#' - `model_version`: character. The model identifier the server reports having
#'   served (e.g., a dated snapshot). Useful for reproducibility records; `NA`
#'   when the provider does not echo it.
#' - `finish_reason`: one of `"stop"`, `"length"`, `"filter"`, `"tool"`, `"other"`.
#' - `usage`: list with integers `sent`, `rec`, `total`, `reasoning`, and
#'   `cached` (tokens read from the provider's prompt cache; `NA` when not
#'   reported).
#' - `thinking`: character. Reasoning text when the provider returns it
#'   separately (e.g., Anthropic thinking blocks, Gemini thought parts,
#'   DeepSeek `reasoning_content`); `NA` otherwise.
#' - `response_id`: provider's response identifier if present.
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
                                           reasoning = NA_integer_,
                                           cached = NA_integer_),
                              response_id = NA_character_,
                              duration_s = NA_real_,
                              raw = NULL,
                              raw_json = NULL,
                              model_version = NA_character_,
                              thinking = NA_character_) {
  structure(
    list(
      text          = as.character(text %||% NA_character_),
      provider      = provider,
      model         = model,
      model_version = model_version,
      finish_reason = finish_reason,
      usage         = usage,
      thinking      = thinking,
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
#' @return A list \code{list(sent, rec, total, reasoning, cached)}. Missing
#'   values are \code{NA}. `cached` counts prompt tokens the provider read from
#'   its cache (cheaper than fresh input tokens); it is `NA` for providers that
#'   do not report cache usage.
#' @examples
#' \dontrun{
#' u <- tokens(r)
#' u$total
#' }
#' @rdname llmr_response
#' @export
tokens <- function(x) {
  if (!inherits(x, "llmr_response")) {
    return(list(sent = NA, rec = NA, total = NA, reasoning = NA, cached = NA))
  }
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
  if (length(fr) != 1L || is.na(fr)) fr <- "other"
  # The trailing unnamed "" is the default: without it, a non-standard finish
  # reason makes switch() return NULL, sprintf() collapse to character(0), and
  # the whole status line silently vanish.
  hint <- switch(fr,
                 length = "Hit token limit; raise max_tokens.",
                 filter = "Content filtered; adjust prompt/safety.",
                 tool   = "Tool call; supply tool handler.",
                 stop   = "",
                 other  = "",
                 "")
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
#' @keywords internal
#' @noRd
.std_finish_reason <- function(content) {
  fr <- NULL
  if (!is.null(content$choices) && length(content$choices) >= 1) {
    # An OpenAI-style refusal is a content filter in standardized terms even
    # though the provider labels the finish "stop".
    ref <- content$choices[[1]]$message$refusal
    if (!is.null(ref) && is.character(ref) && nzchar(ref[1])) return("filter")
    fr <- content$choices[[1]]$finish_reason %||% content$choices[[1]]$finishReason
  }

  # OpenAI Responses API (detect via object type)
  if (is.null(fr) && identical(content$object, "response")) {
    if (!is.null(content$output) && length(content$output) >= 1) {
      fr <- content$output[[1]]$status
    }
    if (is.null(fr) && !is.null(content$status)) {
      fr <- content$status
    }
  }

  if (is.null(fr) && !is.null(content$candidates) && length(content$candidates) >= 1) {
    fr <- content$candidates[[1]]$finishReason
  }
  if (is.null(fr) && !is.null(content$stop_reason)) {
    fr <- content$stop_reason
  }

  fr <- tolower(as.character(fr %||% ""))
  if (!nzchar(fr)) return("other")
  if (fr %in% c("stop","end_turn","stop_sequence","completed","done")) return("stop")
  if (fr %in% c("length","max_tokens","max_token","maxoutputtokens","max_completion_tokens","incomplete")) return("length")
  if (fr %in% c("content_filter","safety","blocked","refusal","recitation",
                "prohibited_content","blocklist","spii","image_safety")) return("filter")
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
  # OpenAI Chat Completions
  rt <- tryCatch(
    content$usage$completion_tokens_details$reasoning_tokens,
    error = function(e) NA_integer_
  )
  if (!is.null(rt) && !is.na(rt)) return(as.integer(rt))

  # OpenAI Responses API
  rt2 <- tryCatch(
    content$usage$output_tokens_details$reasoning_tokens,
    error = function(e) NA_integer_
  )
  if (!is.null(rt2) && !is.na(rt2)) return(as.integer(rt2))

  # Gemini
  rt3 <- tryCatch(content$usageMetadata$thoughtsTokenCount, error = function(e) NA_integer_)
  if (!is.null(rt3) && !is.na(rt3)) return(as.integer(rt3))
  NA_integer_
}

#' Extract cached prompt-token counts if present
#' @keywords internal
#' @noRd
.cached_tokens_from <- function(content) {
  grab <- function(x) {
    if (is.null(x)) return(NULL)
    v <- suppressWarnings(as.integer(x))
    if (length(v) == 1L && !is.na(v)) v else NULL
  }
  # OpenAI Chat Completions / OpenAI-compatible providers
  v <- grab(tryCatch(content$usage$prompt_tokens_details$cached_tokens, error = function(e) NULL))
  if (!is.null(v)) return(v)
  # OpenAI Responses API
  v <- grab(tryCatch(content$usage$input_tokens_details$cached_tokens, error = function(e) NULL))
  if (!is.null(v)) return(v)
  # Anthropic
  v <- grab(tryCatch(content$usage$cache_read_input_tokens, error = function(e) NULL))
  if (!is.null(v)) return(v)
  # DeepSeek reports prompt_cache_hit_tokens at the top level of usage
  v <- grab(tryCatch(content$usage$prompt_cache_hit_tokens, error = function(e) NULL))
  if (!is.null(v)) return(v)
  # Gemini
  v <- grab(tryCatch(content$usageMetadata$cachedContentTokenCount, error = function(e) NULL))
  if (!is.null(v)) return(v)
  NA_integer_
}

#' Extract the served model identifier if the provider echoes one
#' @keywords internal
#' @noRd
.model_version_from <- function(content) {
  mv <- content$model %||% content$modelVersion %||% NULL
  if (!is.null(mv) && is.character(mv) && nzchar(mv[1])) return(mv[1])
  # OpenAI chat completions also carry a backend fingerprint
  fp <- content$system_fingerprint %||% NULL
  if (!is.null(fp) && is.character(fp) && nzchar(fp[1])) return(fp[1])
  NA_character_
}

#' Extract separately-returned reasoning text if present
#'
#' Providers differ: Anthropic returns `thinking` content blocks, Gemini marks
#' parts with `thought = TRUE` (when `include_thoughts` is requested), and
#' DeepSeek-style reasoners return `reasoning_content` next to `content`.
#' @keywords internal
#' @noRd
.thinking_from <- function(content) {
  # Anthropic: content blocks of type "thinking"
  if (!is.null(content$content) && is.list(content$content)) {
    th <- vapply(content$content, function(b) {
      if (identical(b$type, "thinking") && is.character(b$thinking)) b$thinking else ""
    }, character(1))
    th <- th[nzchar(th)]
    if (length(th)) return(paste(th, collapse = "\n"))
  }
  # OpenAI-compatible reasoners (DeepSeek, some Groq/Together models)
  if (!is.null(content$choices) && length(content$choices) >= 1) {
    rc <- content$choices[[1]]$message$reasoning_content %||%
          content$choices[[1]]$message$reasoning %||% NULL
    if (!is.null(rc) && is.character(rc) && nzchar(rc[1])) return(rc[1])
  }
  # Gemini: parts flagged as thoughts
  if (!is.null(content$candidates) && length(content$candidates) >= 1) {
    parts <- tryCatch(content$candidates[[1]]$content$parts, error = function(e) NULL)
    if (is.list(parts) && length(parts)) {
      th <- vapply(parts, function(p) {
        if (isTRUE(p$thought) && is.character(p$text)) p$text else ""
      }, character(1))
      th <- th[nzchar(th)]
      if (length(th)) return(paste(th, collapse = "\n"))
    }
  }
  NA_character_
}
