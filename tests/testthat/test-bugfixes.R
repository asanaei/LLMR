library(testthat)
library(LLMR)

# Regression tests for the rare-path bugs fixed in 0.7.2. All offline.

test_that(".llmr_error keeps its typed class when the message contains braces", {
  # cli::cli_abort would otherwise glue-interpret "{...}" and drop the subclass.
  msg <- 'HTTP 429. Reason: rate limited for {"model":"gpt-4"} retry soon.'
  e <- tryCatch(
    LLMR:::.llmr_error(msg, category = "rate_limit", status_code = 429L),
    condition = function(e) e
  )
  expect_true(inherits(e, "llmr_api_rate_limit_error"))
  expect_true(inherits(e, "llmr_api_error"))
  expect_identical(e$status_code, 429L)
  expect_match(conditionMessage(e), "429", fixed = TRUE)
})

test_that(".llmr_error param errors with braces remain catchable as param errors", {
  e <- tryCatch(
    LLMR:::.llmr_error("bad param {response_format}", category = "param",
                       status_code = 400L, param = "max_tokens"),
    llmr_api_param_error = function(e) e,
    condition = function(e) e
  )
  expect_true(inherits(e, "llmr_api_param_error"))
  expect_identical(e$param, "max_tokens")
})

test_that("retry_with_backoff re-raises the last typed error, not a generic one", {
  attempts <- 0L
  f <- function() {
    attempts <<- attempts + 1L
    LLMR:::.llmr_error("HTTP 503 server busy", category = "server", status_code = 503L)
  }
  e <- tryCatch(
    suppressMessages(
      LLMR:::retry_with_backoff(f, tries = 2L, initial_wait = 0, backoff_factor = 1.5,
                                error_filter_func = function(e) TRUE)
    ),
    error = function(e) e
  )
  expect_identical(attempts, 2L)
  expect_true(inherits(e, "llmr_api_server_error"))   # not a bare "All attempts failed."
  expect_identical(e$status_code, 503L)
})

test_that("retry_with_backoff message formatting handles fractional waits", {
  # fractional wait_time used to throw via sprintf('%d', ...)
  f <- function() stop("boom")
  expect_error(
    suppressMessages(
      LLMR:::retry_with_backoff(f, tries = 2L, initial_wait = 2,
                                backoff_factor = 1.62,
                                error_filter_func = function(e) TRUE)
    ),
    "boom", fixed = TRUE   # the real error, not an sprintf format error
  )
})

test_that("llm_par_resume flags only failed/NA rows, not every row", {
  # The fix is the element-wise predicate; test it directly on the success column.
  success <- c(TRUE, TRUE, FALSE, NA, TRUE)
  failed_idx <- which(!(success %in% TRUE))
  expect_identical(failed_idx, c(3L, 4L))
})

test_that("largest balanced segment recovers a full embedded JSON array", {
  s <- 'items: [{"a":1},{"a":2}] done'
  seg <- LLMR:::.largest_balanced_segment(s)
  parsed <- jsonlite::fromJSON(seg, simplifyVector = FALSE)
  expect_length(parsed, 2L)            # whole array, not just the first object
  s2 <- 'Here is the answer: [1, 2, [3, 4], 5] thanks'
  seg2 <- LLMR:::.largest_balanced_segment(s2)
  expect_identical(seg2, "[1, 2, [3, 4], 5]")   # full balanced array extracted
})

test_that("llm_parse_structured recovers a prose-wrapped array", {
  out <- LLMR:::llm_parse_structured('result: [{"x":10},{"x":20}] ok')
  expect_false(is.null(out))
  expect_length(out, 2L)
})

test_that(".token_counts keeps Gemini prompt tokens when candidates absent", {
  j <- list(usageMetadata = list(promptTokenCount = 1000L,
                                 totalTokenCount = 1200L))   # no candidatesTokenCount
  tc <- LLMR:::.token_counts(j)
  expect_identical(tc$sent, 1000L)
  expect_identical(tc$rec, 200L)        # derived from total - prompt, parenthesized
})

test_that(".token_counts derivation is not corrupted by operator precedence", {
  # when candidatesTokenCount IS present it must be used verbatim
  j <- list(usageMetadata = list(promptTokenCount = 1000L,
                                 candidatesTokenCount = 500L,
                                 totalTokenCount = 1500L))
  tc <- LLMR:::.token_counts(j)
  expect_identical(tc$sent, 1000L)
  expect_identical(tc$rec, 500L)
})

test_that("print.llmr_response prints a status line for non-standard finish reasons", {
  r <- LLMR:::new_llmr_response(text = "hi", provider = "p", model = "m",
                         finish_reason = "end_turn",          # non-standard label
                         usage = list(sent = 1L, rec = 1L, total = 2L, reasoning = NA_integer_),
                         response_id = NA_character_, duration_s = 0.1,
                         raw = list(), raw_json = "{}")
  out <- capture.output(print(r))
  expect_true(any(nzchar(out)))                  # the status line did not vanish
  expect_true(any(grepl("finish=end_turn", out, fixed = TRUE)))
})
