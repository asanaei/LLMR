library(testthat)
library(LLMR)

test_that("call_llm works with OpenAI API", {

  skip_on_cran()  # Skip this test on CRAN
  config <- llm_config(
    provider = "openai",
    model = "gpt-4o-mini",
    # if OPENAI_KEY exists, make it the api_key, otherwise use "fake_api
    api_key = Sys.getenv("OPENAI_API_KEY", unset = "default_fake_key"),
    temperature = 1,
    max_tokens = 1024,
    top_p = 1,
#   troubleshooting = FALSE,
    frequency_penalty = 0,
    presence_penalty = 0
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
