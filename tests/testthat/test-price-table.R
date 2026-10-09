library(testthat)
library(LLMR)

# The cost estimate refuses rates it cannot use and prices Anthropic's cached
# tokens once.

usage_frame <- function(...) {
  tibble::tibble(success = TRUE, finish_reason = "stop", ...)
}

test_that("Anthropic cached tokens are not subtracted from fresh input twice", {
  x <- usage_frame(model = "claude-x", provider = "anthropic",
                   sent_tokens = 400L, rec_tokens = 50L, total_tokens = 450L,
                   cached_tokens = 200L)
  prices <- data.frame(model = "claude-x", input = 1, output = 5, cached = 0.1)
  expect_equal(llm_usage(x, price_table = prices)$cost_estimate,
               (400 * 1 + 200 * 0.1 + 50 * 5) / 1e6)
})

test_that("for other providers cached tokens are part of the prompt count", {
  x <- usage_frame(model = "gpt-x", provider = "openai",
                   sent_tokens = 400L, rec_tokens = 50L, total_tokens = 450L,
                   cached_tokens = 200L)
  prices <- data.frame(model = "gpt-x", input = 1, output = 5, cached = 0.1)
  expect_equal(llm_usage(x, price_table = prices)$cost_estimate,
               (200 * 1 + 200 * 0.1 + 50 * 5) / 1e6)
})

test_that("missing, negative, factor, and duplicated rates are refused", {
  x <- usage_frame(model = c("a", "b"), sent_tokens = c(10L, 10L),
                   rec_tokens = c(5L, 5L), total_tokens = c(15L, 15L),
                   cached_tokens = c(0L, 0L))
  expect_error(llm_usage(x, price_table = data.frame(model = c("a", "b"),
               input = c(1, NA), output = 1)), "rate for model\\(s\\): b")
  expect_error(llm_usage(x, price_table = data.frame(model = c("a", "b"),
               input = c(1, -1), output = 1)), "rate for model\\(s\\): b")
  expect_error(llm_usage(x, price_table = data.frame(model = c("a", "b"),
               input = factor(c("1", "2")), output = 1)), "must be numeric")
  expect_error(llm_usage(x, price_table = data.frame(model = c("a", "a", "b"),
               input = 1, output = 1)), "more than once: a")
})

test_that("a missing cached rate is allowed only without cached tokens", {
  prices <- data.frame(model = c("a", "b"), input = 1, output = 1, cached = c(NA, 0.5))
  x_ok <- usage_frame(model = c("a", "b"), sent_tokens = 10L, rec_tokens = 5L,
                      total_tokens = 15L, cached_tokens = c(0L, 4L))
  expect_equal(llm_usage(x_ok, price_table = prices)$cost_estimate,
               (10 + 5 + (6 + 4 * 0.5 + 5)) / 1e6)
  x_bad <- usage_frame(model = c("a", "b"), sent_tokens = 10L, rec_tokens = 5L,
                       total_tokens = 15L, cached_tokens = c(3L, 4L))
  expect_error(llm_usage(x_bad, price_table = prices), "rate for model\\(s\\): a")
})
