library(testthat)
library(LLMR)

# llm_usage() / llm_failures() — pure readers over diagnostic columns. Offline.

# ---- call_llm_par-style (direct names) ------------------------------------

par_res <- tibble::tibble(
  response_text    = c("a", "b", NA, "d"),
  success          = c(TRUE, TRUE, FALSE, TRUE),
  error_message    = c(NA, NA, "HTTP 503", NA),
  finish_reason    = c("stop", "length", "error:server", "filter"),
  sent_tokens      = c(10L, 20L, NA_integer_, 30L),
  rec_tokens       = c(5L, 7L, NA_integer_, 9L),
  total_tokens     = c(15L, 27L, NA_integer_, 39L),
  reasoning_tokens = c(NA_integer_, 3L, NA_integer_, NA_integer_),
  status_code      = c(200L, 200L, 503L, 200L),
  error_code       = c(NA, NA, "server_error", NA),
  bad_param        = c(NA, NA, NA, NA),
  response_id      = c("r1", "r2", NA, "r4"),
  duration         = c(0.4, 0.5, 0.1, 0.6)
)

test_that("llm_usage summarizes a call_llm_par result", {
  u <- llm_usage(par_res)
  expect_equal(u$n, 4L)
  expect_equal(u$n_ok, 3L)
  expect_equal(u$n_failed, 1L)
  expect_equal(u$ok_rate, 0.75)
  expect_equal(u$n_truncated, 1L)     # the "length" row
  expect_equal(u$n_filtered, 1L)      # the "filter" row
  expect_equal(u$total_tokens, 15L + 27L + 39L)
  expect_equal(u$reasoning_tokens, 3L)
  expect_equal(u$duration_s, 1.6, tolerance = 1e-9)
})

test_that("llm_failures lists failed and truncated rows from a par result", {
  f <- llm_failures(par_res)
  expect_equal(f$row, c(2L, 3L, 4L))   # length, server-error, filter
  expect_equal(f$finish_reason, c("length", "error:server", "filter"))
  expect_equal(f$status_code[[2]], 503L)
})

test_that("llm_failures include= filters", {
  expect_equal(llm_failures(par_res, include = "failed")$row, 3L)
  expect_equal(llm_failures(par_res, include = "truncated")$row, c(2L, 4L))
})

test_that("llm_failures returns zero rows when all clean", {
  clean <- tibble::tibble(success = c(TRUE, TRUE),
                          finish_reason = c("stop", "stop"))
  f <- llm_failures(clean)
  expect_equal(nrow(f), 0L)
  expect_true(tibble::is_tibble(f))
})

# ---- llm_mutate-style (prefixed names) ------------------------------------

mut_res <- tibble::tibble(
  question     = c("q1", "q2", "q3"),
  answer       = c("a1", "a2", NA),
  answer_finish = c("stop", "length", "stop"),
  answer_sent  = c(10L, 20L, 30L),
  answer_rec   = c(5L, 7L, 9L),
  answer_tot   = c(15L, 27L, 39L),
  answer_reason = c(NA_integer_, NA_integer_, NA_integer_),
  answer_ok    = c(TRUE, TRUE, FALSE),
  answer_err   = c(NA, NA, "boom"),
  answer_id    = c("r1", "r2", NA),
  answer_status = c(200L, 200L, 500L),
  answer_ecode = c(NA, NA, "srv"),
  answer_param = c(NA, NA, NA),
  answer_t     = c(0.2, 0.3, 0.1)
)

test_that("llm_usage infers the prefix from a single _ok block", {
  u <- llm_usage(mut_res)
  expect_equal(u$n, 3L)
  expect_equal(u$n_ok, 2L)
  expect_equal(u$n_truncated, 1L)
  expect_equal(u$total_tokens, 81L)
})

test_that("llm_usage accepts an explicit prefix", {
  u <- llm_usage(mut_res, prefix = "answer")
  expect_equal(u$n_failed, 1L)
})

test_that("llm_failures works on a prefixed mutate result", {
  f <- llm_failures(mut_res)
  expect_equal(f$row, c(2L, 3L))    # length-truncated, and hard failure
  expect_equal(f$error_message[[2]], "boom")
})

test_that("ambiguous prefix requires an explicit choice", {
  two <- tibble::tibble(
    a_ok = c(TRUE), a_finish = "stop",
    b_ok = c(TRUE), b_finish = "stop"
  )
  expect_error(llm_usage(two), "multiple diagnostic blocks", ignore.case = TRUE)
  expect_silent(llm_usage(two, prefix = "a"))
})

test_that("missing diagnostics gives a clear error", {
  expect_error(llm_usage(tibble::tibble(x = 1:3)),
               "diagnostic columns", ignore.case = TRUE)
})

# ---- batching token attribution -------------------------------------------

test_that("llm_usage sums batched tokens correctly (tokens on first row, NA rest)", {
  # batch of 3 rows: tokens on row 1 only, NA on rows 2-3 (the real contract)
  batched <- tibble::tibble(
    success          = c(TRUE, TRUE, TRUE),
    finish_reason    = c("stop", "stop", "stop"),
    sent_tokens      = c(60L, NA_integer_, NA_integer_),
    rec_tokens       = c(30L, NA_integer_, NA_integer_),
    total_tokens     = c(90L, NA_integer_, NA_integer_),
    reasoning_tokens = c(NA_integer_, NA_integer_, NA_integer_),
    duration         = c(1.0, NA_real_, NA_real_),
    batch_id         = c(1L, 1L, 1L),
    batch_size       = c(3L, 3L, 3L),
    batch_row        = c(1L, 2L, 3L)
  )
  u <- llm_usage(batched)
  expect_equal(u$total_tokens, 90L)         # not corrupted by the NAs
  expect_equal(u$batch_calls, 1L)
  expect_equal(u$rows_per_batch_call, 3)
})

test_that("non-batched usage reports NA batch fields", {
  u <- llm_usage(par_res)
  expect_true(is.na(u$batch_calls))
  expect_true(is.na(u$rows_per_batch_call))
})

test_that("token sums report only tokens, never dollars (no cost columns)", {
  u <- llm_usage(par_res)
  expect_false(any(grepl("cost|usd|dollar|price", names(u), ignore.case = TRUE)))
})
