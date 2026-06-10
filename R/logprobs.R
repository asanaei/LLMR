# logprobs.R -----------------------------------------------------------------
# Tidy extraction of token log-probabilities.
#
# Requesting them: pass `logprobs = TRUE` (and optionally `top_logprobs = k`,
# k <= 20 for most providers) in llm_config() for OpenAI-compatible providers,
# or the same canonical names for Gemini (translated to responseLogprobs /
# logprobs). Anthropic does not expose logprobs.

#' Extract token log-probabilities from a response
#'
#' Token-level log-probabilities turn a classification into a measurement: the
#' probability the model assigned to its own answer is a confidence score you
#' can calibrate, threshold, or carry into downstream models as a soft label.
#' Request them at config time (`llm_config(..., logprobs = TRUE,
#' top_logprobs = 5)`); this helper then returns them tidily.
#'
#' @param x An [llmr_response] object (from [call_llm()] and friends), or a
#'   result frame from [call_llm_par()] (whose `response` list-column holds the
#'   response objects).
#' @return For a single response: a tibble with one row per generated token:
#'   `token` (character), `logprob` (double), and `top_logprobs` (a list-column
#'   of data frames with the `k` most likely alternatives at that position,
#'   when requested). Returns a zero-row tibble when the response carries no
#'   logprobs. For a result frame: a list of such tibbles, one per row.
#' @examples
#' \dontrun{
#' # Provider support varies; deepseek-chat and OpenAI expose logprobs,
#' # Anthropic does not, and several hosts reject the flag model by model.
#' cfg <- llm_config("deepseek", "deepseek-chat",
#'                   logprobs = TRUE, top_logprobs = 3, temperature = 0)
#' r <- call_llm(cfg, "Answer with one word: is water wet?")
#' llm_logprobs(r)
#'
#' # Confidence of the first answer token:
#' exp(llm_logprobs(r)$logprob[1])
#' }
#' @seealso [llm_config()], [tokens()]
#' @export
llm_logprobs <- function(x) {
  if (is.data.frame(x)) {
    if (!"response" %in% names(x)) {
      stop("This frame has no `response` list-column; run call_llm_par() ",
           "(or llm_fn(.return = \"object\")) to keep response objects.",
           call. = FALSE)
    }
    return(lapply(x$response, function(r) {
      if (inherits(r, "llmr_response")) llm_logprobs(r) else .llm_logprobs_empty()
    }))
  }
  if (!inherits(x, "llmr_response")) {
    stop("`x` must be an llmr_response or a call_llm_par() result frame.",
         call. = FALSE)
  }
  content <- x$raw
  out <- .llm_logprobs_openai(content)
  if (is.null(out)) out <- .llm_logprobs_gemini(content)
  out %||% .llm_logprobs_empty()
}

.llm_logprobs_empty <- function() {
  tibble::tibble(token = character(0), logprob = numeric(0),
                 top_logprobs = list())
}

# OpenAI chat-completions shape (also Groq, Together, DeepSeek, xAI):
# choices[[1]]$logprobs$content = [{token, logprob, top_logprobs: [...]}, ...]
.llm_logprobs_openai <- function(content) {
  lp <- tryCatch(content$choices[[1]]$logprobs$content, error = function(e) NULL)
  if (is.null(lp) || !length(lp)) return(NULL)
  token   <- vapply(lp, function(t) as.character(t$token %||% NA_character_), character(1))
  logprob <- vapply(lp, function(t) as.numeric(t$logprob %||% NA_real_), numeric(1))
  tops <- lapply(lp, function(t) {
    tl <- t$top_logprobs
    if (is.null(tl) || !length(tl)) return(NULL)
    data.frame(
      token   = vapply(tl, function(a) as.character(a$token %||% NA_character_), character(1)),
      logprob = vapply(tl, function(a) as.numeric(a$logprob %||% NA_real_), numeric(1)),
      stringsAsFactors = FALSE
    )
  })
  tibble::tibble(token = token, logprob = logprob, top_logprobs = tops)
}

# Gemini shape: candidates[[1]]$logprobsResult with chosenCandidates and
# topCandidates (each top entry has candidates = [{token, logProbability}]).
.llm_logprobs_gemini <- function(content) {
  lr <- tryCatch(content$candidates[[1]]$logprobsResult, error = function(e) NULL)
  if (is.null(lr)) return(NULL)
  chosen <- lr$chosenCandidates
  if (is.null(chosen) || !length(chosen)) return(NULL)
  token   <- vapply(chosen, function(t) as.character(t$token %||% NA_character_), character(1))
  logprob <- vapply(chosen, function(t) as.numeric(t$logProbability %||% NA_real_), numeric(1))
  tops_raw <- lr$topCandidates
  tops <- lapply(seq_along(chosen), function(i) {
    tc <- tryCatch(tops_raw[[i]]$candidates, error = function(e) NULL)
    if (is.null(tc) || !length(tc)) return(NULL)
    data.frame(
      token   = vapply(tc, function(a) as.character(a$token %||% NA_character_), character(1)),
      logprob = vapply(tc, function(a) as.numeric(a$logProbability %||% NA_real_), numeric(1)),
      stringsAsFactors = FALSE
    )
  })
  tibble::tibble(token = token, logprob = logprob, top_logprobs = tops)
}
