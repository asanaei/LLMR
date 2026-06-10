library(testthat)
library(LLMR)

## use shared helper in helper-keys.R
## Live tests run against the inexpensive open-weight gpt-oss-20b on Groq.

test_that("call_llm works with the Groq API", {
  skip_if_no_env("GROQ_API_KEY")
  skip_on_cran()
  config <- llm_config(
    provider = "groq",
    model = "openai/gpt-oss-20b",
    temperature = 1,
    max_tokens = 1024
  )

  messages <- list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "What's the capital of France?")
  )

  result <- call_llm(config, messages)

  expect_s3_class(result, "llmr_response")
  expect_true(grepl("Paris", as.character(result), ignore.case = TRUE))
  expect_true(is.finite(tokens(result)$total))
})
