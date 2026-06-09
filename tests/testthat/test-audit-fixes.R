library(testthat)
library(LLMR)

# Regression tests for the audit fixes in this release. All offline.

# ---- #2: .fields = FALSE in structured/JSON mode --------------------------

test_that(".fields = FALSE keeps only structured_data (no field hoisting)", {
  df <- tibble::tibble(response_text = c('{"name":"a","score":1}',
                                         '{"name":"b","score":2}'))
  out <- LLMR::llm_parse_structured_col(df, fields = FALSE)
  expect_true("structured_data" %in% names(out))
  expect_true("structured_ok" %in% names(out))
  # FALSE must NOT be treated as a one-element field name
  expect_false("FALSE" %in% names(out))
  expect_false("score" %in% names(out))   # not hoisted
  expect_false("name"  %in% names(out))
})

test_that(".fields = FALSE does not error when the structured column is absent", {
  df <- tibble::tibble(other = c("x", "y"))
  expect_silent(out <- LLMR::llm_parse_structured_col(df, fields = FALSE))
  expect_false("FALSE" %in% names(out))
})

test_that(".fields as a real vector still hoists (no regression)", {
  df <- tibble::tibble(response_text = c('{"name":"a","score":1}'))
  out <- LLMR::llm_parse_structured_col(df, fields = c("name", "score"))
  expect_true(all(c("name", "score") %in% names(out)))
})

# ---- #6: token counts are NA (not 0) when usage is absent -----------------

test_that(".token_counts returns NA, not 0, when no usage metadata present", {
  tc <- LLMR:::.token_counts(list(choices = list(list(message =
                                  list(content = "hi")))))
  expect_true(is.na(tc$sent))
  expect_true(is.na(tc$rec))
})

test_that(".token_counts still reads OpenAI-style usage", {
  tc <- LLMR:::.token_counts(list(usage = list(prompt_tokens = 11L,
                                               completion_tokens = 4L)))
  expect_identical(tc$sent, 11L)
  expect_identical(tc$rec, 4L)
})

# ---- #7: collision-renamed diagnostic columns -----------------------------

test_that("llm_usage handles collision-renamed call_llm_par columns", {
  # Simulates a user whose input frame already had `success`/`finish_reason`,
  # so call_llm_par wrote its output to success.1 / finish_reason.1.
  x <- tibble::tibble(
    success          = c("user-A", "user-B"),    # the user's own column
    finish_reason    = c("u1", "u2"),
    success.1        = c(TRUE, FALSE),            # call_llm_par's real output
    finish_reason.1  = c("stop", "length"),
    sent_tokens      = c(10L, 20L),
    rec_tokens       = c(5L, 7L)
  )
  u <- llm_usage(x)
  expect_equal(u$n_ok, 1L)
  expect_equal(u$n_truncated, 1L)
})

test_that("llm_par_resume gives a clear error on collision-renamed success", {
  x <- tibble::tibble(success = c("a"), success.1 = c(TRUE),
                      config = list(1), messages = list(1))
  expect_error(llm_par_resume(x), "collision-renamed", ignore.case = TRUE)
})

# ---- #4: preview is tag-aware for object + batch --------------------------

test_that("llm_preview flags object+batch in plain mode but not in tag mode", {
  df <- tibble::tibble(text = c("a", "b", "c"))
  p_plain <- llm_preview(df, prompt = "{text}", .return = "object",
                         .batch_size = 2)
  expect_true(any(grepl("object", p_plain$issues[[1]])))

  p_tags <- llm_preview(df, prompt = "{text}", .return = "object",
                        .batch_size = 2, .tags = c("a", "b"))
  expect_false(any(grepl("object", unlist(p_tags$issues))))
})

# ---- #5: llm_fn routes through the shared renderer ------------------------

test_that("llm_fn message build equals the shared renderer (bare vector)", {
  x <- c("alpha", "beta")
  # what llm_fn now builds internally:
  shared <- LLMR:::.llm_build_messages_df(
    data.frame(x = x, stringsAsFactors = FALSE),
    prompt = "Q: {x}", .system_prompt = "S")
  # the frozen old inline assembly:
  old_txt <- glue::glue_data(list(x = x), "Q: {x}", .na = "")
  old <- lapply(as.character(old_txt),
                function(txt) c(system = "S", user = txt))
  expect_identical(shared, old)
})

# ---- #1: llm_api_key_env is exported --------------------------------------

test_that("llm_api_key_env is exported and usable", {
  expect_true("llm_api_key_env" %in% getNamespaceExports("LLMR"))
  h <- llm_api_key_env("SOME_VAR")
  expect_s3_class(h, "llmr_secret")
})
