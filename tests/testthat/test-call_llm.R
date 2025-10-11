library(testthat)
library(LLMR)

## use shared helper in helper-keys.R


test_that("call_llm works with OpenAI API", {
  skip_if_no_env("OPENAI_API_KEY")
  skip_on_cran()  # Skip this test on CRAN
  config <- llm_config(
    provider = "openai",
    model = "gpt-4.1-nano",
    api_key = llm_api_key_env("OPENAI_API_KEY"),
    temperature = 1,
    max_tokens = 1024,
#    top_p = 1,
#   troubleshooting = FALSE,
#    frequency_penalty = 0,
#    presence_penalty = 0
  )

  messages <- list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "What's the capital of France?")
  )

  # Call the function (this will make a real API call)
  result <- call_llm(config, messages)

  # Check the result (assuming you have a way to validate it)
  expect_true(grepl("Paris", result, ignore.case = TRUE))
})
