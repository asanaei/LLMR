library(testthat)
library(LLMR)

# llm_batch_fetch() keeps one row per submitted request and accounts for every
# result line: unreadable, unknown, duplicated, and missing results are labeled.

anthropic_line <- function(id, text = NULL, error = NULL, message = NULL) {
  result <- if (!is.null(error)) {
    list(type = "errored", error = list(message = error))
  } else {
    list(type = "succeeded", message = message %||% list(
      id = paste0("msg_", id), type = "message", role = "assistant", model = "claude-x",
      content = list(list(type = "text", text = text)), stop_reason = "end_turn",
      usage = list(input_tokens = 10, output_tokens = 3)))
  }
  as.character(jsonlite::toJSON(list(custom_id = id, result = result), auto_unbox = TRUE))
}

fetch_anthropic <- function(lines, ids = c("r1", "r2", "r3")) {
  withr::local_envvar(c(ANTHROPIC_API_KEY = "test-key-not-real"))
  job <- .llmr_batch_job("anthropic", llm_config("anthropic", "claude-x", max_tokens = 64),
                         "b1", ids)
  mock <- function(req) {
    if (grepl("/batches/b1$", req$url)) {
      httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"),
                      body = charToRaw(as.character(jsonlite::toJSON(list(
                        processing_status = "ended", results_url = "https://example.com/results"),
                        auto_unbox = TRUE))))
    } else {
      httr2::response(status_code = 200, headers = list(`Content-Type` = "application/jsonl"),
                      body = charToRaw(paste(lines, collapse = "\n")))
    }
  }
  httr2::with_mocked_responses(mock, .batch_fetch_anthropic(job))
}

test_that("every submitted request keeps a row and every anomaly is reported", {
  lines <- c(anthropic_line("r1", text = "first"),
             anthropic_line("r1", error = "late duplicate"),
             '{"custom_id": "r2", "result": ',
             anthropic_line("zz", text = "never submitted"),
             anthropic_line("r2", message = "not a message object"))
  expect_warning(res <- fetch_anthropic(lines),
                 "1 submitted request\\(s\\) were not returned.*1 result line\\(s\\) could not be parsed.*1 result\\(s\\) had an id that was not submitted.*1 duplicate")
  expect_identical(res$custom_id, c("r1", "r2", "r3"))
  expect_identical(res$success, c(TRUE, FALSE, FALSE))
  expect_identical(res$response_text[1], "first")
  expect_true(is.na(res$error_message[1]))
  expect_match(res$error_message[2], "malformed record|unreadable reply")
  expect_identical(res$error_message[3], "not returned by provider")
})

test_that("a clean batch fetch is unchanged and silent", {
  lines <- c(anthropic_line("r1", text = "a"), anthropic_line("r2", text = "b"))
  expect_no_warning(res <- fetch_anthropic(lines, ids = c("r1", "r2")))
  expect_identical(res$response_text, c("a", "b"))
  expect_identical(res$success, c(TRUE, TRUE))
  expect_identical(res$total_tokens, c(13L, 13L))
})
