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
# -------------------------------------------------------------------
# 1. Exponential Backoff (Internal)
# -------------------------------------------------------------------

# A generic utility that calls a function repeatedly with exponentially
# increasing wait time after each failure. Not exported, no roxygen docs.

retry_with_backoff <- function(func,
                               tries = 3,
                               initial_wait = 10,
                               backoff_factor = 10,
                               ...) {
  wait_time <- initial_wait
  for (attempt in seq_len(tries)) {
    result <- tryCatch(
      func(...),
      error = function(e) {
        message(sprintf("Error on attempt %d: %s", attempt, conditionMessage(e)))
        message(sprintf("Waiting %d seconds before retry...", wait_time))
        Sys.sleep(wait_time)
        wait_time <- wait_time * backoff_factor
        return(NULL)
      }
    )
    if (!is.null(result)) {
      return(result)
    }
  }
  stop("All attempts failed.")
}

# -------------------------------------------------------------------
# 2. Rate-Limit-Aware LLM API Call
# -------------------------------------------------------------------

#' Robustly Call LLM API (Simple Retry)
#'
#' Wraps \code{\link{call_llm}} to handle rate-limit errors (HTTP 429 or related
#' "Too Many Requests" messages). It retries the call a specified number of times,
#' now using exponential backoff. You can also choose to cache responses if you do
#' not need fresh results each time.
#'
#' @param config An \code{llm_config} object from \code{\link{llm_config}}.
#' @param messages A list of message objects (or character vector for embeddings).
#' @param tries Integer. Number of retries before giving up. Default is 3.
#' @param wait_seconds Numeric. Initial wait time (seconds) before the first retry. Default is 10.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 2.
#' @param verbose Logical. If TRUE, prints the full API response.
#' @param json Logical. If TRUE, returns the raw JSON as an attribute.
#' @param memoize Logical. If TRUE, calls are cached to avoid repeated identical requests. Default is FALSE.
#'
#' @return The successful result from \code{\link{call_llm}}, or an error if all retries fail.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic usage:
#'   robust_resp <- call_llm_robust(
#'     config = my_llm_config,
#'     messages = list(list(role = "user", content = "Hello, LLM!")),
#'     tries = 3,
#'     wait_seconds = 10,
#'     memoize = FALSE
#'   )
#'   cat("Response:", robust_resp, "\n")
#' }
call_llm_robust <- function(config, messages,
                            tries = 3,
                            wait_seconds = 10,
                            backoff_factor = 2,
                            verbose = FALSE,
                            json = FALSE,
                            memoize = FALSE) {

  # Internal helper that calls either the direct function or the cached variant
  call_func <- function() {
    tryCatch(
      {
        if (memoize) {
          if (!requireNamespace("memoise", quietly = TRUE)) {
            stop("memoize=TRUE requires the 'memoise' package. Please install it.")
          }
          cache_llm_call(config, messages, verbose = verbose, json = json)
        } else {
          call_llm(config, messages, verbose = verbose, json = json)
        }
      },
      error = function(e) {
        # Use our logging function here
        log_llm_error(e)

        err_msg <- conditionMessage(e)
        # Simple detection of rate-limit-like errors
        is_rate_error <- grepl("429|rate limit|too many requests|exceeded",
                               err_msg, ignore.case = TRUE)
        if (is_rate_error) {
          stop("Rate-limit error. Triggering backoff retry.")
        } else {
          stop(e)
        }
      }
    )
  }

  # Use the internal retry function with exponential backoff
  retry_with_backoff(
    func = call_func,
    tries = tries,
    initial_wait = wait_seconds,
    backoff_factor = backoff_factor
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
#' @param json Logical. If TRUE, returns raw JSON (passed to \code{\link{call_llm}}).
#'
#' @details
#' - Requires the \code{memoise} package. Add \code{memoise} to your
#'   package's DESCRIPTION.
#' - Clearing the cache can be done via \code{memoise::forget(cache_llm_call)}
#'   or by restarting your R session.
#'
#' @return The (memoised) response object from \code{\link{call_llm}}.
#'
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
if (!requireNamespace("memoise", quietly = TRUE)) {
  stop("Caching requires the 'memoise' package. Please install it (install.packages('memoise')).")
}
cache_llm_call <- memoise::memoise(function(config, messages, verbose = FALSE, json = FALSE) {
  call_llm(config, messages, verbose = verbose, json = json)
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
#' @examples
#' \dontrun{
#'   # Example of logging an error by catching a failure:
#'   # Use a deliberately fake API key to force an error
#'   config_test <- llm_config(
#'     provider = "openai",
#'     model = "gpt-3.5-turbo",
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









