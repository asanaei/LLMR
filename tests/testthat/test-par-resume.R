library(testthat)
library(LLMR)

# llm_par_resume() re-runs only rows whose call failed. A truncated reply is a
# success and is kept as it is; derived columns are not refreshed.

resume_frame <- function() {
  cfg <- llm_config("openai", "gpt-4o-mini")
  tibble::tibble(
    config        = list(cfg, cfg, cfg),
    messages      = list(c(user = "a"), c(user = "b"), c(user = "c")),
    response_text = c("ok", NA, "cut off"),
    success       = c(TRUE, FALSE, TRUE),
    finish_reason = c("stop", "error:server", "length"),
    error_message = c(NA, "HTTP 503", NA)
  )
}

rerun <- function(text) {
  function(experiments, ...) {
    tibble::tibble(config = experiments$config, messages = experiments$messages,
                   response_text = text, success = TRUE,
                   finish_reason = "stop", error_message = NA_character_)
  }
}

test_that("only failed rows are re-run; truncated rows are kept", {
  seen <- NULL
  local_mocked_bindings(call_llm_par = function(experiments, ...) {
    seen <<- experiments$messages
    rerun("fixed")(experiments)
  })
  expect_message(out <- llm_par_resume(resume_frame()), "Found 1 failed")
  expect_identical(seen, list(c(user = "b")))
  expect_identical(out$response_text, c("ok", "fixed", "cut off"))
  expect_identical(out$finish_reason, c("stop", "stop", "length"))
})

test_that("structured diagnostics are refreshed; derived columns draw warnings", {
  local_mocked_bindings(call_llm_par = rerun('{"a": 1}'))
  st <- resume_frame()
  st$structured_ok <- c(TRUE, FALSE, FALSE)
  st$structured_data <- list(list(a = 1), NULL, NULL)
  expect_warning(out <- suppressMessages(llm_par_resume(st)), "not re-extracted")
  expect_identical(out$structured_ok, c(TRUE, TRUE, FALSE))
  tg <- resume_frame()
  tg$tags_ok <- c(TRUE, FALSE, FALSE)
  tg$tags_data <- list(list(), NULL, NULL)
  expect_warning(suppressMessages(llm_par_resume(tg)), "not re-parsed")
})

test_that("a frame with no failed rows is returned unchanged", {
  ok <- resume_frame()[c(1, 3), ]
  expect_message(out <- llm_par_resume(ok), "Nothing to resume")
  expect_identical(out, ok)
})
