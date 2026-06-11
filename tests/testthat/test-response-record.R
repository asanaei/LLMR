library(testthat)
library(LLMR)

# llm_response_record() — one flattening path from a call to a provenance row.
# Offline; fabricated llmr_response objects, no keys needed.

fake_response <- function(text = "Hello!", ...) {
  structure(
    utils::modifyList(
      list(
        text = text, provider = "openai", model = "demo",
        model_version = "demo-2025-01-01", finish_reason = "stop",
        usage = list(sent = 12L, rec = 5L, total = 17L,
                     reasoning = NA_integer_, cached = NA_integer_),
        response_id = "resp_123", duration_s = 0.012,
        raw = NULL, raw_json = NULL, thinking = NA_character_
      ),
      list(...)
    ),
    class = "llmr_response"
  )
}

test_that("a normal response flattens to one contract row", {
  rec <- llm_response_record(fake_response())
  expect_s3_class(rec, "tbl_df")
  expect_identical(nrow(rec), 1L)
  expect_true(all(c(
    "response_text", "response_id", "provider", "model", "model_version",
    "finish_reason", "sent_tokens", "rec_tokens", "total_tokens",
    "reasoning_tokens", "cached_tokens", "success", "error_message",
    "duration_s", "created_at", "request_hash"
  ) %in% names(rec)))
  expect_true(rec$success)
  expect_identical(rec$response_id, "resp_123")
  expect_identical(rec$sent_tokens, 12L)
  expect_identical(rec$total_tokens, 17L)
  expect_identical(rec$finish_reason, "stop")
})

test_that("response_id and token counts are preserved", {
  rec <- llm_response_record(
    fake_response(response_id = "abc",
                  usage = list(sent = 1L, rec = 2L, total = 3L,
                               reasoning = 4L, cached = 5L))
  )
  expect_identical(rec$response_id, "abc")
  expect_identical(rec$rec_tokens, 2L)
  expect_identical(rec$reasoning_tokens, 4L)
  expect_identical(rec$cached_tokens, 5L)
})

test_that("a caught error is a success = FALSE row, not a dropped call", {
  rec <- llm_response_record(simpleError("boom"))
  expect_identical(nrow(rec), 1L)
  expect_false(rec$success)
  expect_identical(rec$error_message, "boom")
  expect_true(is.na(rec$response_text))
})

test_that("non-response object yields a failed row noting the class", {
  rec <- llm_response_record(list(a = 1))
  expect_false(rec$success)
  expect_match(rec$error_message, "Expected an llmr_response")
})

test_that("request_hash is filled when request and config are supplied", {
  cfg <- llm_config("openai", "demo", temperature = 0)
  rec <- llm_response_record(fake_response(), request = "Hello", config = cfg)
  expect_match(rec$request_hash, "^[0-9a-f]{64}$")
  expect_identical(rec$request_hash, llm_request_hash(cfg, "Hello"))
  # absent request -> NA
  expect_true(is.na(llm_response_record(fake_response())$request_hash))
})

test_that("provider and model backfill from config on a failed call", {
  cfg <- llm_config("groq", "llama-x", temperature = 0)
  rec <- llm_response_record(simpleError("nope"), config = cfg)
  expect_identical(rec$provider, "groq")
  expect_identical(rec$model, "llama-x")
})
