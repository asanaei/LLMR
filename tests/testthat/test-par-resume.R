library(testthat)
library(LLMR)

resume_frame <- function() {
  cfg <- llm_config("openai", "gpt-4o-mini")
  tibble::tibble(
    config = rep(list(cfg), 5),
    messages = lapply(letters[1:5], function(x) c(user = x)),
    response_text = c("ok", NA, "cut off", "filtered", NA),
    success = c(TRUE, FALSE, TRUE, TRUE, NA),
    finish_reason = c("stop", "error:server", "length", "filter", NA),
    error_message = c(NA, "HTTP 503", NA, NA, NA)
  )
}

rerun <- function(text) {
  function(experiments, ...) {
    tibble::tibble(config = experiments$config, messages = experiments$messages,
                   response_text = rep_len(text, nrow(experiments)), success = TRUE,
                   finish_reason = "stop", error_message = NA_character_)
  }
}

test_that("FALSE and NA success rows are retried with tries and simplify forwarded", {
  seen <- NULL
  local_mocked_bindings(call_llm_par = function(experiments, tries, simplify, ...) {
    seen <<- list(messages = experiments$messages, tries = tries, simplify = simplify)
    rerun("fixed")(experiments)
  })
  original <- resume_frame()
  expect_message(out <- llm_par_resume(original, tries = 7L), "Found 2 failed")
  expect_identical(seen, list(messages = list(c(user = "b"), c(user = "e")),
                              tries = 7L, simplify = FALSE))
  expect_identical(out$response_text, c("ok", "fixed", "cut off", "filtered", "fixed"))
  expect_identical(out$finish_reason, c("stop", "stop", "length", "filter", "stop"))
  expect_identical(out[3:4, names(original)],
                   structure(original[3:4, ], class = class(out)))
  expect_error(suppressMessages(llm_par_resume(original, simplify = TRUE)),
               "matched by multiple actual arguments")
})

test_that("structured diagnostics refresh while extracted fields require removal and reparsing", {
  local_mocked_bindings(call_llm_par = rerun('{"a": 1}'))
  st <- resume_frame()
  st$structured_ok <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  st$structured_data <- list(list(a = 9), NULL, NULL, NULL, NULL)
  st$a <- c(9, NA, NA, NA, NA)
  expect_warning(out <- suppressMessages(llm_par_resume(st)), "remove their old columns")
  expect_identical(out$a, st$a)
  expect_identical(out$structured_ok, c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(out$structured_data[[2]], list(a = 1))
  expect_equal(out$structured_data[[5]], list(a = 1))
  expect_identical(out$structured_data[[1]], st$structured_data[[1]])
  expect_warning(kept <- llm_parse_structured_col(out, fields = "a"), "already exists")
  expect_identical(kept$a, st$a)
  expect_equal(kept$a_1[c(2, 5)], c(1, 1))
  out$a <- NULL
  refreshed <- llm_parse_structured_col(out, fields = "a")
  expect_equal(refreshed$a[c(2, 5)], c(1, 1))
  expect_false("a_1" %in% names(refreshed))
  st$a <- NULL
  expect_warning(suppressMessages(llm_par_resume(st)), "not re-extracted")
  st$structured_data <- NULL
  expect_no_warning(suppressMessages(llm_par_resume(st)))
})

test_that("tag diagnostics and extracted tags stay unchanged until reparsed", {
  local_mocked_bindings(call_llm_par = rerun('<a>fixed</a>'))
  tg <- resume_frame()
  tg$tags_ok <- rep(FALSE, nrow(tg))
  tg$tags_data <- rep(list(NULL), nrow(tg))
  tg$a <- rep(NA_character_, nrow(tg))
  expect_warning(out <- suppressMessages(llm_par_resume(tg)), "remove old extracted columns")
  expect_identical(out$tags_ok, tg$tags_ok)
  expect_identical(out$tags_data, tg$tags_data)
  expect_identical(out$a, tg$a)
  expect_warning(kept <- llm_parse_tags_col(out, tags = "a"), "already exists")
  expect_identical(kept$a, tg$a)
  expect_identical(kept$a_1[c(2, 5)], c("fixed", "fixed"))
  out$a <- NULL
  refreshed <- llm_parse_tags_col(out, tags = "a")
  expect_identical(refreshed$a[c(2, 5)], c("fixed", "fixed"))
  tg$tags_data <- NULL
  expect_no_warning(suppressMessages(llm_par_resume(tg)))
})

test_that("repeated failures remain failed and new result columns are not added", {
  local_mocked_bindings(call_llm_par = function(experiments, ...) {
    out <- rerun(NA_character_)(experiments)
    out$success <- FALSE
    out$error_message <- "still unavailable"
    out$new_diagnostic <- 1L
    out
  })
  out <- suppressMessages(llm_par_resume(resume_frame()))
  expect_identical(out$success, c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(out$error_message[c(2, 5)], rep("still unavailable", 2))
  expect_false("new_diagnostic" %in% names(out))
  expect_s3_class(out, "llmr_experiment")
})

test_that("validation and collision guards precede the no-failures return", {
  ok <- resume_frame()[c(1, 3, 4), ]
  expect_message(out <- llm_par_resume(ok), "Nothing to resume")
  expect_identical(out, ok)
  for (name in c("success.1", "duration.1", "response_text.2")) {
    collision <- ok
    collision[[name]] <- NA
    expect_error(llm_par_resume(collision), "collision-renamed")
  }
  expect_error(llm_par_resume(ok["success"]), "containing 'config', 'messages', and 'success'")
  expect_error(llm_par_resume(NULL), "results must be the output")
})
