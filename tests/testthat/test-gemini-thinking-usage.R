library(testthat)
library(LLMR)

# Gemini's thinking tokens count in total_tokens and in the cost estimate;
# rec_tokens stays the visible output, as the provider reports it.

gemini_body <- function(usage) {
  jsonlite::toJSON(list(
    candidates = list(list(content = list(parts = list(list(text = "Four.")),
                                          role = "model"),
                           finishReason = "STOP")),
    usageMetadata = usage, responseId = "g-1"), auto_unbox = TRUE)
}

mock_json <- function(body) {
  function(req) httr2::response(status_code = 200,
                                headers = list(`Content-Type` = "application/json"),
                                body = charToRaw(as.character(body)))
}

test_that("a non-streamed Gemini reply reports the provider's total", {
  withr::local_envvar(c(GEMINI_API_KEY = "test-key-not-real"))
  body <- gemini_body(list(promptTokenCount = 50, candidatesTokenCount = 10,
                           thoughtsTokenCount = 300, totalTokenCount = 360))
  r <- httr2::with_mocked_responses(mock_json(body),
    call_llm(llm_config("gemini", "gemini-x"), "What is 2 + 2?"))
  u <- tokens(r)
  expect_identical(u$total, 360L)
  expect_identical(u$rec, 10L)
  expect_identical(u$reasoning, 300L)
})

test_that("the cost estimate bills thinking tokens as output, once", {
  x <- tibble::tibble(
    model = c("gemini-x", "gpt-x"), success = TRUE, finish_reason = "stop",
    sent_tokens = c(50L, 9L), rec_tokens = c(10L, 7L), total_tokens = c(360L, 16L),
    reasoning_tokens = c(300L, NA_integer_), cached_tokens = NA_integer_)
  prices <- data.frame(model = c("gemini-x", "gpt-x"), input = 1, output = 10)
  cost <- llm_usage(x, price_table = prices)$cost_estimate
  expect_equal(cost, (50 * 1 + 310 * 10 + 9 * 1 + 7 * 10) / 1e6)
})

test_that("a streamed Gemini reply without a candidates count keeps the total", {
  skip_if(utils::packageVersion("httr2") < "1.2.0",
          "mocking req_perform_connection() needs httr2 >= 1.2.0")
  withr::local_envvar(c(GEMINI_API_KEY = "test-key-not-real"))
  ev <- paste0("data: ", as.character(jsonlite::toJSON(list(
    candidates = list(list(content = list(parts = list(list(text = "ok")), role = "model"),
                           finishReason = "STOP")),
    usageMetadata = list(promptTokenCount = 50, thoughtsTokenCount = 300,
                         totalTokenCount = 350),
    responseId = "g-2"), auto_unbox = TRUE)), "\n\n")
  mock <- function(req) httr2::response(status_code = 200,
    headers = list(`Content-Type` = "text/event-stream"),
    body = httr2::StreamingBody$new(rawConnection(charToRaw(ev))))
  r <- httr2::with_mocked_responses(mock,
    call_llm_stream(llm_config("gemini", "gemini-x"), "hi", callback = function(ch) NULL))
  expect_identical(tokens(r)$total, 350L)
})
