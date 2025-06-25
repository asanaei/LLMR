library(testthat)
library(LLMR)

test_that("llm_fn returns correct length vector", {
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")))
  skip_on_cran()                       # avoid API calls on CRAN
  cfg <- llm_config(
    provider = "openai",
    model    = "gpt-4.1-nano",
    api_key  = Sys.getenv("OPENAI_API_KEY"),
    temperature = 0
  )

  x <- c("good", "bad", "average", "good")

  out <- llm_fn(
    x,
    prompt  = "Turn this into a numerical scale from 0 to 10, where 10 means excellent and 0 means horrible: {x}",
    .system_prompt = 'you only answer in integer numbers; not even one more word!',
    .config = cfg
  )

  expect_length(out, length(x))
  expect_type(out, "character")
})
