library(testthat)
library(LLMR)

# llm_log_read() reports malformed records by their line in the file and checks
# a record's shape before reading its fields.

good <- '{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0","kind":"call","provider":"openai","model":"m","status":200,"request":{"messages":[{"role":"user","content":"hi"}]},"text":"ok"}'

write_log <- function(lines) {
  f <- tempfile(fileext = ".jsonl")
  writeLines(lines, f)
  f
}

test_that("blank lines are skipped but counted, and the manifest records each line", {
  read <- llm_log_read(write_log(c("", good, "   ", good)))
  expect_identical(read$manifest$line, c(2L, 4L))
  expect_identical(read$manifest$idx, 1:2)
  expect_false(anyNA(read$manifest$request_hash))
})

test_that("invalid JSON is reported at its physical line", {
  f <- write_log(c(good, "", "", '{"kind": "call", '))
  expect_error(llm_log_read(f), "Log line 4 is not valid JSON")
})

test_that("a record that is not a JSON object is refused with its shape", {
  expect_error(llm_log_read(write_log(c(good, "5"))),
               "Log line 2 is a (numeric|integer) value, not a JSON object")
  expect_error(llm_log_read(write_log(c('"text"'))),
               "Log line 1 is a character value, not a JSON object")
  expect_error(llm_log_read(write_log(c(good, "", "[1, 2]"))),
               "Log line 3 is an array, not a JSON object")
})

test_that("metadata fields must be single values and status a number", {
  expect_error(
    llm_log_read(write_log(sub('"model":"m"', '"model":["a","b"]', good, fixed = TRUE))),
    "Log line 1: field `model` must be a single value, not an object or array")
  expect_error(
    llm_log_read(write_log(c(good, sub('"provider":"openai"', '"provider":{"x":1}', good, fixed = TRUE)))),
    "Log line 2: field `provider` must be a single value, not an object or array")
  expect_error(
    llm_log_read(write_log(sub('"status":200', '"status":"ok"', good, fixed = TRUE))),
    'Log line 1: field `status` must be an HTTP status number, not "ok"')
  expect_error(
    llm_log_read(write_log(sub('"request":{"messages":[{"role":"user","content":"hi"}]}',
                               '"request":"hi"', good, fixed = TRUE))),
    "Log line 1: field `request` must be a JSON object")
  # null fields and a numeric string status are accepted
  ok <- llm_log_read(write_log(sub('"status":200', '"status":"200","model_version":null',
                                   good, fixed = TRUE)))
  expect_identical(ok$manifest$status, 200L)
})
