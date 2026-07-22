library(testthat)
library(LLMR)

# Provider-batch job persistence. Offline: the job object is constructed
# directly; no submission happens.

test_that("batch state refuses a literal-key config", {
  cfg <- llm_config("openai", "gpt-4o-mini", api_key = "sk-sentinel-0451")
  job <- LLMR:::.llmr_batch_job("openai", cfg, "batch_x", c("llmr-000001"))
  path <- tempfile(fileext = ".rds")
  expect_error(LLMR:::.llmr_batch_save(job, path), "literal API key")
  expect_false(file.exists(path))
})

test_that("batch state with an env-handle config contains no key bytes", {
  withr::local_envvar(LLMR_TEST_BATCH_KEY = "sk-sentinel-0451")
  cfg <- llm_config("openai", "gpt-4o-mini",
                    api_key = llm_api_key_env("LLMR_TEST_BATCH_KEY"))
  job <- LLMR:::.llmr_batch_job("openai", cfg, "batch_x", c("llmr-000001"))
  path <- tempfile(fileext = ".rds")
  LLMR:::.llmr_batch_save(job, path)
  bytes <- rawToChar(serialize(readRDS(path), NULL, ascii = TRUE))
  expect_false(grepl("sk-sentinel-0451", bytes, fixed = TRUE))
  expect_true(grepl("LLMR_TEST_BATCH_KEY", bytes, fixed = TRUE))
})
