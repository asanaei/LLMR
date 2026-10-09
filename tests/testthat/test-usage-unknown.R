library(testthat)
library(LLMR)

# A total is known only when the provider reported it or both parts are known;
# unknown usage is never counted as zero.

no_usage_resp <- function(text = "ok") {
  structure(list(
    text = text, provider = "openai", model = "m", finish_reason = "stop",
    usage = list(sent = NA_integer_, rec = NA_integer_, total = NA_integer_,
                 reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01,
    raw = list(choices = list(list(message = list(role = "assistant", content = text)))),
    raw_json = "{}"), class = "llmr_response")
}

test_that("a batch reply without usage has an unknown total", {
  row <- .batch_row_from_content(list(
    choices = list(list(message = list(role = "assistant", content = "hi"),
                        finish_reason = "stop"))))
  expect_true(is.na(row$total_tokens))
  x <- tibble::tibble(success = TRUE, finish_reason = "stop",
                      sent_tokens = row$sent_tokens, rec_tokens = row$rec_tokens,
                      total_tokens = row$total_tokens)
  expect_identical(llm_usage(x)$n_unknown_tokens, 1L)
})

test_that("a partial Gemini usage record is unknown, a complete one is not", {
  partial <- .token_counts(list(usageMetadata = list(promptTokenCount = 50)))
  expect_identical(partial$sent, 50)
  expect_true(is.na(partial$rec))
  complete <- .token_counts(list(usageMetadata = list(promptTokenCount = 50,
                                                     totalTokenCount = 50)))
  expect_equal(complete$rec, 0)
})

test_that("a completed stream with one usage part missing has an unknown total", {
  skip_if(utils::packageVersion("httr2") < "1.2.0",
          "mocking req_perform_connection() needs httr2 >= 1.2.0")
  withr::local_envvar(c(ANTHROPIC_API_KEY = "test-key-not-real"))
  ev <- paste0("data: ", c(
    '{"type":"message_start","message":{"id":"m1","model":"claude-x","usage":{"input_tokens":120}}}',
    '{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Four."}}',
    '{"type":"message_delta","delta":{"stop_reason":"end_turn"}}',
    '{"type":"message_stop"}'), "\n\n", collapse = "")
  mock <- function(req) httr2::response(status_code = 200,
    headers = list(`Content-Type` = "text/event-stream"),
    body = httr2::StreamingBody$new(rawConnection(charToRaw(ev))))
  r <- httr2::with_mocked_responses(mock,
    call_llm_stream(llm_config("anthropic", "claude-x", max_tokens = 64), "hi",
                    callback = function(ch) NULL))
  expect_identical(tokens(r)$sent, 120L)
  expect_true(is.na(tokens(r)$total))
})

test_that("a chat session counts replies whose usage is unknown", {
  testthat::local_mocked_bindings(call_llm_robust = function(...) no_usage_resp("hi"),
                                  .package = "LLMR")
  chat <- chat_session(llm_config("openai", "m"))
  invisible(utils::capture.output(chat$send("hello")))
  expect_identical(chat$usage_unknown(), 1L)
  expect_output(print(summary(chat)), "Usage unknown:    1 reply")
})

test_that("the tool loop counts model calls whose usage is unknown", {
  testthat::local_mocked_bindings(call_llm_robust = function(...) no_usage_resp("done"),
                                  .package = "LLMR")
  tool <- llm_tool(function(x) x, name = "echo", description = "echo",
                   parameters = list(x = list(type = "string")))
  r <- call_llm_tools(llm_config("openai", "m"), "hi", tools = tool)
  expect_identical(r$tool_loop$n_unknown_usage, 1L)
})
