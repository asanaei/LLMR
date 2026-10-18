library(testthat)
library(LLMR)

# call_llm_stream() through the real httr2 streaming transport with mocked
# responses: a completed stream returns a response, and a failed or interrupted
# one raises a typed error that carries the text received so far.

sse <- function(...) paste0(paste0("data: ", c(...), "\n\n"), collapse = "")

mock_stream <- function(text, status = 200, type = "text/event-stream") {
  function(req) {
    httr2::response(status_code = status, headers = list(`Content-Type` = type),
                    body = httr2::StreamingBody$new(rawConnection(charToRaw(text))))
  }
}

run_stream <- function(cfg, mock, callback = function(chunk) invisible(NULL)) {
  httr2::with_mocked_responses(mock, call_llm_stream(cfg, "hi", callback = callback))
}

local_fake_keys <- function(env = parent.frame()) {
  withr::local_envvar(c(OPENAI_API_KEY = "test-key-not-real",
                        ANTHROPIC_API_KEY = "test-key-not-real",
                        GEMINI_API_KEY = "test-key-not-real"), .local_envir = env)
}

skip_without_stream_mocks <- function() {
  skip_if(utils::packageVersion("httr2") < "1.2.0",
          "mocking req_perform_connection() needs httr2 >= 1.2.0")
}

openai_cfg <- function() llm_config("openai", "gpt-4o-mini")
anthropic_cfg <- function() llm_config("anthropic", "claude-x", max_tokens = 64)
gemini_cfg <- function() llm_config("gemini", "gemini-x")

test_that("complete streams return a response for each provider family", {
  skip_without_stream_mocks()
  local_fake_keys()
  chunks <- character(0)
  r <- run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c1","model":"gpt-4o-mini","choices":[{"index":0,"delta":{"content":"Paris is "}}]}',
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"the capital."},"finish_reason":"stop"}]}',
    '{"id":"c1","choices":[],"usage":{"prompt_tokens":9,"completion_tokens":7}}',
    "[DONE]")), callback = function(ch) chunks <<- c(chunks, ch))
  expect_identical(r$text, "Paris is the capital.")
  expect_identical(r$finish_reason, "stop")
  expect_identical(chunks, c("Paris is ", "the capital."))
  expect_identical(tokens(r)$sent, 9L)

  r <- run_stream(anthropic_cfg(), mock_stream(sse(
    '{"type":"message_start","message":{"id":"m1","model":"claude-x","usage":{"input_tokens":12}}}',
    '{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Four."}}',
    '{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":3}}',
    '{"type":"message_stop"}')))
  expect_identical(r$text, "Four.")
  expect_identical(r$finish_reason, "stop")

  r <- run_stream(gemini_cfg(), mock_stream(sse(
    '{"candidates":[{"content":{"parts":[{"text":"Hi"}],"role":"model"},"finishReason":"STOP"}],"responseId":"g1"}')))
  expect_identical(r$text, "Hi")
})

test_that("a finish reason without [DONE] still counts as complete", {
  skip_without_stream_mocks()
  local_fake_keys()
  r <- run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"Done"},"finish_reason":"length"}]}')))
  expect_identical(r$text, "Done")
  expect_identical(r$finish_reason, "length")
})

test_that("a stream that ends before completion raises with the partial text", {
  skip_without_stream_mocks()
  local_fake_keys()
  e <- expect_error(run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"Partial"}}]}'))),
    class = "llmr_stream_error")
  expect_s3_class(e, "llmr_api_error")
  expect_identical(e$partial_text, "Partial")
  expect_match(conditionMessage(e), "ended before the provider signalled completion")

  e <- expect_error(run_stream(anthropic_cfg(), mock_stream(sse(
    '{"type":"message_start","message":{"id":"m1","model":"claude-x","usage":{"input_tokens":12}}}',
    '{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"The answer is"}}'))),
    class = "llmr_stream_error")
  expect_identical(e$partial_text, "The answer is")
})

test_that("provider error events inside the stream raise typed errors", {
  skip_without_stream_mocks()
  local_fake_keys()
  e <- expect_error(run_stream(anthropic_cfg(), mock_stream(sse(
    '{"type":"message_start","message":{"id":"m1","model":"claude-x","usage":{"input_tokens":12}}}',
    '{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"The answer is"}}',
    '{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}'))),
    class = "llmr_api_server_error")
  expect_s3_class(e, "llmr_stream_error")
  expect_identical(e$code, "overloaded_error")
  expect_identical(e$partial_text, "The answer is")
  expect_match(conditionMessage(e), "Overloaded")

  e <- expect_error(run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"Partial"}}]}',
    '{"error":{"message":"The server had an error","type":"server_error"}}'))),
    class = "llmr_stream_error")
  expect_identical(e$partial_text, "Partial")
})

test_that("malformed events are counted in a warning", {
  skip_without_stream_mocks()
  local_fake_keys()
  expect_warning(
    r <- run_stream(openai_cfg(), mock_stream(sse(
      '{"id":"c1","choices":[{"index":0,"delta":{"content":"Paris "}}]}',
      '{"id":"c1","choices":[{"index":0,"delta":{"content":"is NOT',
      '{"id":"c1","choices":[{"index":0,"delta":{"content":"the capital."},"finish_reason":"stop"}]}',
      "[DONE]"))),
    "1 streamed event\\(s\\) could not be parsed")
  expect_identical(r$text, "Paris the capital.")
})

test_that("an HTTP error at connect time raises the same typed error as call_llm()", {
  skip_without_stream_mocks()
  local_fake_keys()
  e <- expect_error(run_stream(openai_cfg(), mock_stream(
    '{"error":{"message":"Rate limit reached","type":"requests","code":"rate_limit_exceeded"}}',
    status = 429, type = "application/json")),
    class = "llmr_api_rate_limit_error")
  expect_identical(e$status_code, 429L)
  expect_false(inherits(e, "llmr_stream_error"))
  expect_match(conditionMessage(e), "Rate limit reached")
})

test_that("an error raised by the callback propagates unchanged", {
  skip_without_stream_mocks()
  local_fake_keys()
  expect_error(run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"x"},"finish_reason":"stop"}]}',
    "[DONE]")), callback = function(ch) stop("callback broke")),
    "callback broke")
})

test_that("streams are logged with their request, and failures as errors", {
  skip_without_stream_mocks()
  local_fake_keys()
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_options(llmr.log_file = log, llmr.log_messages = TRUE)
  run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":"stop"}]}',
    "[DONE]")))
  try(run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c2","choices":[{"index":0,"delta":{"content":"cut"}}]}'))), silent = TRUE)
  m <- llm_log_read(log)$manifest
  expect_identical(m$kind, c("stream", "error"))
  expect_identical(m$status, c(200L, 200L))
  expect_false(anyNA(m$request_hash))
  expect_identical(m$request_hash[1], m$request_hash[2])
})

test_that("streamed usage reads cache counts in every reported shape", {
  skip_without_stream_mocks()
  local_fake_keys()
  usage_line <- function(u) sprintf(
    '{"id":"c1","choices":[],"usage":{"prompt_tokens":20,"completion_tokens":5,%s}}', u)
  shapes <- c(openai   = '"prompt_tokens_details":{"cached_tokens":12}',
              anthropic = '"cache_read_input_tokens":12',
              deepseek = '"prompt_cache_hit_tokens":12')
  for (nm in names(shapes)) {
    r <- run_stream(openai_cfg(), mock_stream(sse(
      '{"id":"c1","choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":"stop"}]}',
      usage_line(shapes[[nm]]),
      "[DONE]")))
    expect_identical(tokens(r)$cached, 12L, info = nm)
  }
  r <- run_stream(openai_cfg(), mock_stream(sse(
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":"stop"}]}',
    usage_line('"total_tokens":25'), "[DONE]")))
  expect_identical(tokens(r)$cached, NA_integer_)
})
