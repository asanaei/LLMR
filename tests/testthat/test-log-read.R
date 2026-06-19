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

test_that("audit logging writes directly unless parallel shard mode is enabled", {
  log_file <- tempfile(fileext = ".jsonl")
  shard <- paste0(log_file, ".", Sys.getpid())
  old <- options(llmr.log_file = NULL,
                 llmr.log_messages = TRUE,
                 llmr.log_parallel = NULL)
  on.exit({
    options(old)
    unlink(c(log_file, shard))
  }, add = TRUE)

  options(llmr.log_file = log_file, llmr.log_parallel = FALSE)
  LLMR:::.llmr_log_event(kind = "call", provider = "p", model = "m", status = 200L)

  expect_true(file.exists(log_file))
  expect_false(file.exists(shard))
  expect_length(readLines(log_file), 1L)
})

test_that("parallel audit shards merge back into the base log", {
  log_file <- tempfile(fileext = ".jsonl")
  shard <- paste0(log_file, ".", Sys.getpid())
  old <- options(llmr.log_file = NULL,
                 llmr.log_messages = TRUE,
                 llmr.log_parallel = NULL)
  on.exit({
    options(old)
    unlink(c(log_file, shard))
  }, add = TRUE)

  options(llmr.log_file = log_file, llmr.log_parallel = TRUE)
  LLMR:::.llmr_log_event(kind = "call", provider = "p", model = "m", status = 200L)

  expect_false(file.exists(log_file))
  expect_true(file.exists(shard))
  merged <- llm_log_merge(log_file)
  # the returned shard path points at the same file (separators may differ by OS)
  expect_equal(normalizePath(merged, winslash = "/", mustWork = FALSE),
               normalizePath(shard,  winslash = "/", mustWork = FALSE))
  expect_false(file.exists(shard))
  expect_true(file.exists(log_file))
  expect_length(readLines(log_file), 1L)
})

test_that("llm_log_merge creates a missing base file and escapes regex basenames", {
  log_file <- file.path(tempdir(), paste0("llmr.log+[", Sys.getpid(), "].jsonl"))
  shard <- paste0(log_file, ".10001")
  on.exit(unlink(c(log_file, shard)), add = TRUE)
  unlink(c(log_file, shard))
  writeLines('{"kind":"call","schema_version":"1.0"}', shard)

  merged <- llm_log_merge(log_file)

  expect_equal(normalizePath(merged, winslash = "/", mustWork = FALSE),
               normalizePath(shard,  winslash = "/", mustWork = FALSE))
  expect_false(file.exists(shard))
  expect_identical(readLines(log_file), '{"kind":"call","schema_version":"1.0"}')
})

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
