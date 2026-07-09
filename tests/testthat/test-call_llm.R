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

test_that(".normalize_messages defaults empty names to the user role (offline)", {
  roles <- function(m) vapply(LLMR:::.normalize_messages(m), `[[`, "", "role")

  # partially named vector: the unnamed element must become "user", not "" (which
  # OpenAI-compatible providers reject with HTTP 400 "invalid role")
  expect_identical(roles(c(system = "be brief", "hello")), c("system", "user"))
  expect_identical(roles(c("a", user = "b", "c")), c("user", "user", "user"))

  # fully named and fully unnamed paths are unchanged
  expect_identical(roles(c(system = "s", user = "u", assistant = "a")),
                   c("system", "user", "assistant"))
  expect_identical(roles(c("one", "two")), c("user", "user"))

  # no empty role survives for any partially-named input
  expect_false(any(roles(c(system = "s", "x", "y", user = "z")) == ""))
})
