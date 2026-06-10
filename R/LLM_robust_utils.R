# LLM_robust_utils.R
# -------------------------------------------------------------------
# This file provides additional features that improve reliability and
# fault tolerance of calls made through the LLMR package:
#
#   1. retry_with_backoff() - A generic utility that retries any function
#      with exponential backoff (INTERNAL, not exported).
#
#   2. call_llm_robust() - A specialized wrapper around call_llm() with
#      simple retry logic for rate-limit errors (HTTP 429), now
#      enhanced with variable backoff and optional memoization.
#
#   3. cache_llm_call() - A caching wrapper for call_llm(), preventing
#      repeated identical requests.
#
#   4. log_llm_error() - A convenience function for logging errors
#      with timestamps.
#


#--------- Helper
#' Construct and throw structured LLMR errors
#'
#' Creates a typed error with a category-specific subclass, plus a common
#' `llmr_api_error` base class. Extra fields (e.g., `param`) are attached
#' as condition attributes for handlers to inspect.
#'
#' Classes produced:
#'   - llmr_api_param_error
#'   - llmr_api_auth_error
#'   - llmr_api_rate_limit_error
#'   - llmr_api_server_error
#'   - llmr_api_unknown_error
#'
#' @keywords internal
#' @noRd
.llmr_error <- function(message, category = c("param","auth","rate_limit","server","unknown"),
                        status_code = NA_integer_,
                        provider = NA_character_,
                        model = NA_character_,
                        param = NA_character_,
                        code = NA_character_,
                        request_id = NA_character_,
                        retry_after = NA_real_,
                        body = NULL) {

  category <- match.arg(category)
  cls <- c(
    paste0("llmr_api_", category, "_error"),
    "llmr_api_error", "error", "condition"
  )

  # Use cli if available; fall back to rlang otherwise.
  # cli::cli_abort() treats `message` as a glue/inline-markup template, so any
  # literal "{...}" in a provider error string (e.g. echoed JSON fragments) would
  # be evaluated as R code, throwing cli's own error and dropping our typed class
  # and fields. Escape braces so the provider text is shown verbatim.
  if (requireNamespace("cli", quietly = TRUE)) {
    safe_message <- gsub("}", "}}", gsub("{", "{{", message, fixed = TRUE), fixed = TRUE)
    cli::cli_abort(
      message = safe_message,
      class   = cls,
      # attach fields for tryCatch handlers
      status_code = status_code,
      provider    = provider,
      model       = model,
      param       = param,
      code        = code,
      request_id  = request_id,
      retry_after = retry_after,
      body        = body
    )
  } else {
    rlang::abort(
      message = message,
      class   = cls,
      status_code = status_code,
      provider    = provider,
      model       = model,
      param       = param,
      code        = code,
      request_id  = request_id,
      retry_after = retry_after,
      body        = body
    )
  }
}




# -------------------------------------------------------------------
# 1. Exponential Backoff (Internal)
# -------------------------------------------------------------------

# A generic utility that calls a function repeatedly, waiting between failed
# attempts. The wait honors a provider-supplied Retry-After when the error
# carries one; otherwise it grows exponentially with multiplicative jitter so
# parallel workers do not retry in lockstep. There is no sleep after the final
# attempt: once the budget is spent the last error is re-raised immediately.
# Not exported, no roxygen docs.

retry_with_backoff <- function(func,
                               tries = 5,
                               initial_wait = 10,
                               backoff_factor = 5,
                               error_filter_func = NULL,
                               ...) {
  last_error <- NULL
  for (attempt in seq_len(tries)) {
    result <- tryCatch(
      # Box the value so that a legitimate NULL return is not mistaken for a
      # failed attempt.
      list(value = func(...)),
      error = function(e) {
        if (!is.null(error_filter_func) && !error_filter_func(e)) {
          stop(e)
        }
        last_error <<- e
        NULL
      }
    )
    if (!is.null(result)) return(result$value)
    if (attempt == tries) break
    ra <- suppressWarnings(as.numeric(last_error$retry_after %||% NA_real_))
    wait_time <- if (length(ra) == 1L && is.finite(ra) && ra > 0) {
      ra
    } else {
      initial_wait * (backoff_factor ^ (attempt - 1L)) * stats::runif(1, 0.8, 1.2)
    }
    message(sprintf("Attempt %d of %d failed: %s", attempt, tries,
                    conditionMessage(last_error)))
    message(sprintf("Waiting %s seconds before retry...", format(round(wait_time, 1))))
    Sys.sleep(wait_time)
  }
  # Re-raise the last real error so its typed class, status_code, and provider
  # message survive for downstream handlers, instead of a generic message.
  if (!is.null(last_error)) stop(last_error) else stop("All attempts failed.")
}

# -------------------------------------------------------------------
# 2. Rate-Limit-Aware LLM API Call
# -------------------------------------------------------------------

#' Robustly Call LLM API (Simple Retry)
#'
#' Wraps \code{\link{call_llm}} so that transient failures are retried while
#' permanent ones fail fast. Retried conditions are rate limits (HTTP 429),
#' server errors (HTTP 5xx and 408), and network-level interruptions
#' (timeouts, connection resets, DNS failures). Errors that retrying cannot
#' fix, such as an invalid parameter (400), a missing key (401/403), or a
#' prompt that exceeds the context window, are raised immediately.
#'
#' When the provider supplies a \code{Retry-After} header with a 429, the wait
#' honors it; otherwise waits grow exponentially with a little jitter so that
#' parallel workers do not retry in lockstep.
#'
#' @param config An \code{llm_config} object from \code{\link{llm_config}}.
#' @param messages A list of message objects (or character vector for embeddings).
#' @param tries Integer. Total number of attempts (the first call plus retries)
#'   before giving up. Default is 5.
#' @param wait_seconds Numeric. Initial wait time (seconds) before the first retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 3.
#' @param verbose Logical. If TRUE, prints the full API response.
#' @param memoize Logical. If TRUE, calls are cached to avoid repeated identical requests. Default is FALSE.
#'
#' @return The successful result from \code{\link{call_llm}}, or an error if all retries fail.
#' @seealso
#' \code{\link{call_llm}} for the underlying, non-robust API call.
#' \code{\link{cache_llm_call}} for a memoised version that avoids repeated requests.
#' \code{\link{llm_config}} to create the configuration object.
#' \code{\link{chat_session}} for stateful, interactive conversations.
#' @export
#'
#' @examples
#' \dontrun{
#' robust_resp <- call_llm_robust(
#' config = llm_config("groq", "openai/gpt-oss-20b"),
#' messages = list(list(role = "user", content = "Hello, LLM!")),
#' tries = 5,
#' wait_seconds = 2,
#' memoize = FALSE
#' )
#' print(robust_resp)
#' as.character(robust_resp)
#' }
call_llm_robust <- function(config, messages,
                            tries = 5,
                            wait_seconds = 2,
                            backoff_factor = 3,
                            verbose = FALSE,
                            memoize = FALSE) {

  # Internal helper that calls either the direct function or the cached variant
  call_func <- function() {
    if (memoize) {
      if (!requireNamespace("memoise", quietly = TRUE)) {
        stop("memoize=TRUE requires the 'memoise' package. Please install it (install.packages('memoise')).")
      }
      return(cache_llm_call(config, messages, verbose = verbose))
    } else {
      return(call_llm(config, messages, verbose = verbose))
    }
  }

  # Error filter for retry_with_backoff: only retry conditions that can change
  # on a second attempt.
  is_retryable_error <- function(e) {
    # Typed LLMR conditions carry an exact category: rate limits and server
    # errors are transient; param/auth/unknown are permanent, whatever their
    # message text happens to contain (e.g., "context length exceeded").
    if (inherits(e, "llmr_api_rate_limit_error") || inherits(e, "llmr_api_server_error")) {
      return(TRUE)
    }
    if (inherits(e, "llmr_api_error")) return(FALSE)
    # Untyped errors (network layer, providers without parsed bodies): fall
    # back to message inspection for transient signatures.
    err_msg <- conditionMessage(e)
    grepl(
      "429|500|502|503|504|408|rate limit|too many requests|temporarily unavailable|overloaded|timeout|timed out|resolve host|could not resolve|connection refused|connection reset|peer closed",
      err_msg, ignore.case = TRUE
    )
  }

  tryCatch(
    retry_with_backoff(
      func = call_func,
      tries = tries,
      initial_wait = wait_seconds,
      backoff_factor = backoff_factor,
      error_filter_func = is_retryable_error
    ),
    error = function(e) {
      log_llm_error(e)
      stop(e)
    }
  )
}

# -------------------------------------------------------------------
# 3. Caching Wrapper
# -------------------------------------------------------------------

#' Cache LLM API Calls
#'
#' A memoised version of \code{\link{call_llm}} to avoid repeated identical requests.
#'
#' @param config An \code{llm_config} object from \code{\link{llm_config}}.
#' @param messages A list of message objects or character vector for embeddings.
#' @param verbose Logical. If TRUE, prints the full API response (passed to \code{\link{call_llm}}).
#'
#' @details
#' - Requires the \code{memoise} package. Add \code{memoise} to your
#'   package's DESCRIPTION.
#' - Clearing the cache can be done via \code{memoise::forget(cache_llm_call)}
#'   or by restarting your R session.
#' - The cache lives in the R process that makes the call. Under a
#'   \code{future} multisession plan (as used by \code{\link{call_llm_par}}),
#'   each worker process keeps its own cache, which disappears with the
#'   worker; identical requests in different workers are not deduplicated.
#'
#' @return The (memoised) response object from \code{\link{call_llm}}.
#'
#' @keywords internal
#' @importFrom memoise memoise
#' @export
#' @name cache_llm_call
#'
#' @examples
#' \dontrun{
#'   # Using cache_llm_call:
#'   response1 <- cache_llm_call(my_config, list(list(role="user", content="Hello!")))
#'   # Subsequent identical calls won't hit the API unless we clear the cache.
#'   response2 <- cache_llm_call(my_config, list(list(role="user", content="Hello!")))
#' }
cache_llm_call <- memoise::memoise(function(config, messages, verbose = FALSE) {
  if (!requireNamespace("memoise", quietly = TRUE)) {
    stop("Caching with cache_llm_call requires the 'memoise' package. Please install it (install.packages('memoise')).")
  }
  call_llm(config, messages, verbose = verbose)
})

# -------------------------------------------------------------------
# 4. Error Logging
# -------------------------------------------------------------------

#' Log LLMR Errors
#'
#' Logs an error with a timestamp for troubleshooting.
#'
#' @param err An error object.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
#'
#' @keywords internal
#' @examples
#' \dontrun{
#'   # Example of logging an error by catching a failure. The variable name
#'   # below is deliberately one that is not set in the environment, so the
#'   # call fails locally with a "Missing API key" authentication error.
#'   config_test <- llm_config(
#'     provider = "openai",
#'     model = "gpt-4.1-nano",
#'     api_key = "LLMR_UNSET_DEMO_KEY",
#'     temperature = 0.5,
#'     max_tokens = 30
#'   )
#'
#'   tryCatch(
#'     call_llm(config_test, list(list(role = "user", content = "Hello world!"))),
#'     error = function(e) log_llm_error(e)
#'   )
#' }
log_llm_error <- function(err) {
  stamp <- Sys.time()
  msg <- conditionMessage(err)
  message(sprintf("[%s] LLMR Error: %s", stamp, msg))
  invisible(NULL)
}
