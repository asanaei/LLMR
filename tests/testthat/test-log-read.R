library(testthat)
library(LLMR)

# llm_log_read() parses a JSONL audit log into records + manifest. Offline.

sample_log <- function() {
  path <- tempfile(fileext = ".jsonl")
  writeLines(c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
           '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b","status":200,',
           '"request":{"messages":[{"role":"user","content":"Label: positive?"}],',
           '"temperature":0},"usage":{"sent":5,"rec":2},',
           '"response_id":"r-1","text":"positive"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"gpt-4o-mini","status":200,',
           '"request":{"messages":[{"role":"user","content":"Label: negative?"}],',
           '"temperature":0},"usage":{"sent":6,"rec":2},',
           '"response_id":"r-2","text":"negative"}')
  ), path)
  path
}

test_that("llm_log_read returns records and a manifest with the expected shape", {
  read <- llm_log_read(sample_log())
  expect_named(read, c("records", "manifest"))
  expect_length(read$records, 2L)
  m <- read$manifest
  expect_equal(nrow(m), 2L)
  expect_true(all(c("idx", "ts", "kind", "provider", "model", "model_version",
                    "status", "schema_version", "has_payload", "request_hash",
                    "record_hash") %in% names(m)))
})

test_that("request_hash and record_hash are present and well-formed", {
  m <- llm_log_read(sample_log())$manifest
  expect_true(all(grepl("^[0-9a-f]{64}$", m$request_hash)))
  expect_true(all(grepl("^[0-9a-f]{64}$", m$record_hash)))
  # the two records are different calls -> different request hashes
  expect_false(identical(m$request_hash[1], m$request_hash[2]))
})

test_that("the manifest request_hash equals the config-side hash for the same call", {
  m <- llm_log_read(sample_log())$manifest
  cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
  h_cfg <- llm_request_hash(cfg, c(user = "Label: positive?"))
  expect_identical(m$request_hash[1], h_cfg)
})

test_that("a record without a request body gets NA request_hash", {
  path <- tempfile(fileext = ".jsonl")
  writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
    '"kind":"embedding","provider":"openai","model":"text-embedding-3-small",',
    '"status":200,"usage":{"sent":3,"rec":0}}'), path)
  m <- llm_log_read(path)$manifest
  expect_true(is.na(m$request_hash[1]))
  expect_true(grepl("^[0-9a-f]{64}$", m$record_hash[1]))
})

test_that("an empty log errors clearly", {
  path <- tempfile(fileext = ".jsonl")
  writeLines(character(0), path)
  expect_error(llm_log_read(path), "no records")
})

test_that("corrupted JSON names the offending line", {
  path <- tempfile(fileext = ".jsonl")
  writeLines(c('{"kind":"call"}', 'not json at all'), path)
  expect_error(llm_log_read(path), "line 2")
})

test_that("llm_request_from_log rebuilds a normal chat call", {
  rec <- list(provider = "openai", model = "gpt-4o-mini",
              request = list(messages = list(list(role = "user", content = "Hi")),
                             temperature = 0))
  req <- llm_request_from_log(rec)
  expect_true(req$complete)
  expect_equal(unname(req$messages), "Hi")
  expect_equal(req$config$model_params$temperature, 0)
})

test_that("llm_request_from_log fails loud on an unreconstructable body", {
  # OpenAI Responses-API shape: input/instructions, which .llmr_turns cannot read
  rec <- list(provider = "openai", model = "gpt-5-pro",
              request = list(input = "Summarize this.",
                             instructions = "Be terse.", max_output_tokens = 50))
  expect_error(llm_request_from_log(rec), "cannot fully reconstruct")
  # warn mode returns best-effort with complete = FALSE
  expect_warning(req <- llm_request_from_log(rec, on_unsupported = "warn"),
                 "cannot fully reconstruct")
  expect_false(req$complete)
  # quiet mode is silent
  expect_silent(req2 <- llm_request_from_log(rec, on_unsupported = "quiet"))
  expect_false(req2$complete)
})
