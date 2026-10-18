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
  prices <- data.frame(model = c("a", "b"), provider = "openai", input = 1,
                       output = 1, cached = c(NA, 0.5))
  x_ok <- usage_frame(model = c("a", "b"), sent_tokens = 10L, rec_tokens = 5L,
                      total_tokens = 15L, cached_tokens = c(0L, 4L))
  expect_equal(llm_usage(x_ok, price_table = prices)$cost_estimate,
               (10 + 5 + (6 + 4 * 0.5 + 5)) / 1e6)
  x_bad <- usage_frame(model = c("a", "b"), sent_tokens = 10L, rec_tokens = 5L,
                       total_tokens = 15L, cached_tokens = c(3L, 4L))
  expect_error(llm_usage(x_bad, price_table = prices), "rate for model\\(s\\): a")
})

test_that("a price-table provider column prices rows that carry none", {
  # llm_mutate()/llm_fn()/llm_batch_fetch() results have no provider column
  x <- usage_frame(sent_tokens = 400L, rec_tokens = 50L, total_tokens = 450L,
                   cached_tokens = 200L)
  claude <- data.frame(model = "claude-x", provider = "anthropic", input = 1,
                       output = 5, cached = 0.1)
  expect_no_warning(cost <- llm_usage(x, price_table = claude)$cost_estimate)
  expect_equal(cost, (400 * 1 + 200 * 0.1 + 50 * 5) / 1e6)

  gpt <- transform(claude, model = "gpt-x", provider = "openai")
  expect_equal(llm_usage(x, price_table = gpt)$cost_estimate,
               (200 * 1 + 200 * 0.1 + 50 * 5) / 1e6)

  # a provider on the rows wins over the table
  xr <- usage_frame(model = "claude-x", provider = "anthropic",
                    sent_tokens = 400L, rec_tokens = 50L, total_tokens = 450L,
                    cached_tokens = 200L)
  expect_equal(llm_usage(xr, price_table = transform(claude, provider = "openai"))$cost_estimate,
               (400 * 1 + 200 * 0.1 + 50 * 5) / 1e6)
})

test_that("cached tokens with no known provider are priced with a warning", {
  x <- usage_frame(sent_tokens = 400L, rec_tokens = 50L, total_tokens = 450L,
                   cached_tokens = 200L)
  prices <- data.frame(model = "claude-x", input = 1, output = 5, cached = 0.1)
  expect_warning(cost <- llm_usage(x, price_table = prices)$cost_estimate,
                 "1 row\\(s\\) with cached tokens have no provider")
  expect_equal(cost, (200 * 1 + 200 * 0.1 + 50 * 5) / 1e6)
  # no cached tokens: the provider does not matter, so no warning
  x0 <- usage_frame(sent_tokens = 400L, rec_tokens = 50L, total_tokens = 450L,
                    cached_tokens = 0L)
  expect_no_warning(llm_usage(x0, price_table = prices))
})
