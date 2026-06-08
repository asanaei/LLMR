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
                        request_id = NA_character_) {

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
      request_id  = request_id
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
      request_id  = request_id
    )
  }
}




# -------------------------------------------------------------------
# 1. Exponential Backoff (Internal)
# -------------------------------------------------------------------

# A generic utility that calls a function repeatedly with exponentially
# increasing wait time after each failure. Not exported, no roxygen docs.

retry_with_backoff <- function(func,
                               tries = 5,
                               initial_wait = 10,
                               backoff_factor = 5,
                               error_filter_func = NULL,
                               ...) {
  last_error <- NULL
  for (attempt in seq_len(tries)) {
    wait_time <- initial_wait * (backoff_factor ^ (attempt - 1L))
    result <- tryCatch(
      func(...),
      error = function(e) {
        if (!is.null(error_filter_func) && !error_filter_func(e)) {
          stop(e)
        }
        last_error <<- e
        message(sprintf("Error on attempt %d: %s", attempt, conditionMessage(e)))
        message(sprintf("Waiting %s seconds before retry...", format(round(wait_time, 1))))
        Sys.sleep(wait_time)
        return(NULL)
      }
    )
    if (!is.null(result)) {
      return(result)
    }
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
#' Wraps \code{\link{call_llm}} to handle rate-limit errors (HTTP 429 or related
#' "Too Many Requests" messages). It retries the call a specified number of times,
#' using exponential backoff. You can also choose to cache responses if you do
#' not need fresh results each time.
#'
#' @param config An \code{llm_config} object from \code{\link{llm_config}}.
#' @param messages A list of message objects (or character vector for embeddings).
#' @param tries Integer. Number of retries before giving up. Default is 5.
#' @param wait_seconds Numeric. Initial wait time (seconds) before the first retry. Default is 10.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 5.
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
#' config = llm_config("openai", "gpt-5-nano"),
#' messages = list(list(role = "user", content = "Hello, LLM!")),
#' tries = 5,
#' wait_seconds = 10,
#' memoize = FALSE
#' )
#' print(robust_resp)
#' as.character(robust_resp)
#' }
call_llm_robust <- function(config, messages,
                            tries = 5,
                            wait_seconds = 10,
                            backoff_factor = 5,
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

  # Error filter for retry_with_backoff: only retry for specific conditions
  is_retryable_error <- function(e) {
    if (inherits(e, "llmr_api_rate_limit_error") || inherits(e, "llmr_api_server_error")) {
      log_llm_error(e)
      return(TRUE)
    }
    err_msg <- conditionMessage(e)
    is_transient <- grepl(
      "429|500|502|503|504|rate limit|too many requests|exceeded|timeout|timed out|resolve host|could not resolve|connection refused|connection reset|peer closed",
      err_msg, ignore.case = TRUE
    )
    if (is_transient) {
      log_llm_error(e)
      return(TRUE)
    }
    return(FALSE)
  }

  # Original error handling logic for non-retryable errors or after all retries fail
  tryCatch(
    retry_with_backoff(
      func = call_func,
      tries = tries,
      initial_wait = wait_seconds,
      backoff_factor = backoff_factor,
      error_filter_func = is_retryable_error
    ),
    error = function(e) {
      if (!is_retryable_error(e)) {
         log_llm_error(e)
      }
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
#'   # Example of logging an error by catching a failure:
#'   # Use a deliberately fake API key to force an error
#'   config_test <- llm_config(
#'     provider = "openai",
#'     model = "gpt-4.1-nano",
#'     api_key = "FAKE_KEY",
#'     temperature = 0.5,
#'     top_p = 1,
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









