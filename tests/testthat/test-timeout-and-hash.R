library(testthat)
library(LLMR)

# timeout must be one positive number (Inf for no limit); an invalid value used
# to leave the request with no timeout at all. A request hash that cannot be
# computed stays NA but is reported.

test_that("llm_config refuses a timeout that is not one positive number", {
  for (bad in list("30s", 0, -1, NA, NA_real_, c(5, 10), TRUE, list(5))) {
    expect_error(llm_config("groq", "m", timeout = bad),
                 "`timeout` must be one positive number", info = deparse(bad))
  }
  expect_error(llm_config("groq", "m", timeout = "30s"), '"30s"')
  expect_silent(llm_config("groq", "m", timeout = 30))
  expect_silent(llm_config("groq", "m", timeout = 30L))
  expect_silent(llm_config("groq", "m", timeout = Inf))
})

test_that("requests carry the configured, optioned, or default timeout", {
  req0 <- httr2::request("https://example.invalid")
  r <- LLMR:::.llmr_apply_req_hooks(req0, llm_config("groq", "m", timeout = 42))
  expect_equal(r$options$timeout_ms, 42000)
  r <- LLMR:::.llmr_apply_req_hooks(req0, llm_config("groq", "m"))
  expect_equal(r$options$timeout_ms, 600000)
  withr::local_options(llmr.timeout = 15)
  r <- LLMR:::.llmr_apply_req_hooks(req0, llm_config("groq", "m"))
  expect_equal(r$options$timeout_ms, 15000)
  # Inf: no limit is set
  r <- LLMR:::.llmr_apply_req_hooks(req0, llm_config("groq", "m", timeout = Inf))
  expect_null(r$options$timeout_ms)
})

test_that("an invalid timeout in a saved config or the option is refused at request time", {
  req0 <- httr2::request("https://example.invalid")
  cfg <- llm_config("groq", "m")
  cfg$model_params$timeout <- "30s"          # e.g. a config saved by an older LLMR
  expect_error(LLMR:::.llmr_apply_req_hooks(req0, cfg), "`timeout` must be")
  withr::local_options(llmr.timeout = "10 minutes")
  expect_error(LLMR:::.llmr_apply_req_hooks(req0, llm_config("groq", "m")),
               "options\\(llmr.timeout\\)")
})

test_that("a request hash that cannot be computed is NA with a warning naming the row", {
  missing_file <- file.path(tempdir(), "llmr-no-such-file.png")
  df <- tibble::tibble(
    config = list(llm_config("groq", "m"), llm_config("groq", "m")),
    messages = list("hello", list(list(role = "user", content = list(
      list(type = "text", text = "describe"),
      list(type = "file", path = missing_file))))))
  expect_warning(out <- LLMR:::.llmr_add_request_hash(df),
                 "No request hash for 1 row\\(s\\) \\(2\\)")
  expect_false(is.na(out$request_hash[1]))
  expect_true(is.na(out$request_hash[2]))
})
