library(testthat)
library(LLMR)

# Regression tests for the second audit pass. All offline.

# ---- claim 1: bare env-name resolves regardless of whether set ------------

test_that("a bare ENV-NAME api_key is stored as an env reference even when unset", {
  var <- "LLMR_TEST_UNSET_KEY_XYZ"
  Sys.unsetenv(var)
  cfg <- llm_config("openai", "gpt-4.1-nano", api_key = var)
  # the config must hold an env handle pointing at the name, NOT a literal
  expect_true(inherits(cfg$api_key, "llmr_secret_env"))
  expect_false(inherits(cfg$api_key, "llmr_secret_literal"))
  expect_true(var %in% cfg$api_key$ref)
})

test_that("an unset bare env-name fails cleanly at resolve time (not silently literal)", {
  var <- "LLMR_TEST_UNSET_KEY_XYZ2"
  Sys.unsetenv(var)
  cfg <- llm_config("openai", "gpt-4.1-nano", api_key = var)
  expect_error(
    LLMR:::.resolve_api_key(cfg$api_key, provider = "openai"),
    "Missing API key", fixed = TRUE
  )
})

test_that("a set bare env-name resolves to the variable's value", {
  var <- "LLMR_TEST_SET_KEY_XYZ"
  withr_set <- Sys.getenv(var, unset = NA)
  Sys.setenv(LLMR_TEST_SET_KEY_XYZ = "sk-from-env")
  on.exit(Sys.unsetenv("LLMR_TEST_SET_KEY_XYZ"), add = TRUE)
  cfg <- llm_config("openai", "gpt-4.1-nano", api_key = var)
  expect_identical(LLMR:::.resolve_api_key(cfg$api_key, provider = "openai"),
                   "sk-from-env")
})

# ---- claim 2: required = FALSE is honored ---------------------------------

test_that("llm_api_key_env(required = FALSE) returns '' instead of erroring when unset", {
  var <- "LLMR_TEST_OPTIONAL_KEY_XYZ"
  Sys.unsetenv(var)
  h <- LLMR:::llm_api_key_env(var, required = FALSE)
  expect_identical(LLMR:::.resolve_api_key(h, provider = "ollama"), "")
})

test_that("required = TRUE (default) still errors when unset", {
  var <- "LLMR_TEST_REQUIRED_KEY_XYZ"
  Sys.unsetenv(var)
  h <- LLMR:::llm_api_key_env(var)   # required defaults TRUE
  expect_error(LLMR:::.resolve_api_key(h, provider = "openai"),
               "Missing API key", fixed = TRUE)
})

test_that("a default is still preferred over the required flag", {
  var <- "LLMR_TEST_DEFAULTED_KEY_XYZ"
  Sys.unsetenv(var)
  h <- LLMR:::llm_api_key_env(var, required = TRUE, default = "fallback-key")
  expect_identical(LLMR:::.resolve_api_key(h, provider = "openai"),
                   "fallback-key")
})

# ---- claim 5: llm_parse_structured_col always returns a tibble ------------

test_that("missing structured column path still returns a tibble", {
  df <- as.data.frame(list(other = c("a", "b")), stringsAsFactors = FALSE)
  out <- LLMR::llm_parse_structured_col(df, fields = c("x"),
                                        structured_col = "response_text")
  expect_s3_class(out, "tbl_df")
})

test_that("normal structured path also returns a tibble", {
  df <- tibble::tibble(response_text = c('{"x":1}'))
  out <- LLMR::llm_parse_structured_col(df, fields = c("x"))
  expect_s3_class(out, "tbl_df")
})

# ---- claim 6: llm_usage flags all-unknown token usage ---------------------

test_that("llm_usage reports n_unknown_tokens when usage is absent", {
  x <- tibble::tibble(
    success = c(TRUE, TRUE, TRUE),
    finish_reason = c("stop", "stop", "stop"),
    sent_tokens = c(NA_integer_, NA_integer_, NA_integer_),
    rec_tokens = c(NA_integer_, NA_integer_, NA_integer_),
    total_tokens = c(NA_integer_, NA_integer_, NA_integer_)
  )
  u <- llm_usage(x)
  expect_equal(u$total_tokens, 0)        # na.rm sum, unchanged
  expect_equal(u$n_unknown_tokens, 3L)   # but flagged as unknown
})

test_that("llm_usage does not over-count batch follow-on NA rows as unknown", {
  # batch of 3: tokens on row 1, NA on rows 2-3 by design (not 'unknown')
  x <- tibble::tibble(
    success = c(TRUE, TRUE, TRUE),
    finish_reason = c("stop", "stop", "stop"),
    sent_tokens = c(60L, NA_integer_, NA_integer_),
    rec_tokens = c(30L, NA_integer_, NA_integer_),
    total_tokens = c(90L, NA_integer_, NA_integer_),
    batch_id = c(1L, 1L, 1L),
    batch_size = c(3L, 3L, 3L),
    batch_row = c(1L, 2L, 3L)
  )
  u <- llm_usage(x)
  expect_equal(u$total_tokens, 90L)
  expect_equal(u$n_unknown_tokens, 0L)   # the NA rows are by-design, not unknown
})
