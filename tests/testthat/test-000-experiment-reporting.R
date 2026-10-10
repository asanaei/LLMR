library(testthat)
library(LLMR)

experiment_result <- function() {
  out <- tibble::tibble(
    model = c("model-a", "model-a"),
    provider = c("provider-a", "provider-a"),
    temperature = c(0, 0),
    success = c(TRUE, FALSE),
    finish_reason = c("stop", "error:server"),
    sent_tokens = c(10L, NA_integer_),
    rec_tokens = c(4L, NA_integer_),
    total_tokens = c(14L, NA_integer_),
    reasoning_tokens = c(NA_integer_, NA_integer_),
    duration = c(0.2, 0.1)
  )
  class(out) <- unique(c("llmr_experiment", class(out)))
  out
}

test_that("report covers experiment results without invented run metadata", {
  txt <- report(experiment_result(), task = "to classify short texts")

  expect_match(txt, "model-a")
  expect_match(txt, "provider-a")
  expect_match(txt, "to classify short texts")
  expect_match(txt, "temperature = 0")
  expect_match(txt, "1 call\\(s\\) failed")
  expect_false(grepl(as.character(Sys.Date()), txt, fixed = TRUE))
  expect_false(grepl(
    paste0("version ", utils::packageVersion("LLMR")), txt, fixed = TRUE
  ))
})

test_that("llmr_experiment prints a concise status line", {
  printed <- capture.output(print(experiment_result()))

  expect_length(printed, 1L)
  expect_match(printed, "<llmr_experiment: 2 runs")
  expect_match(printed, "1/2 successful")
  expect_match(printed, "model-a")
  expect_false(grepl("response_text", printed, fixed = TRUE))
})

test_that("zero-row experiment paths preserve the experiment class", {
  empty <- tibble::tibble(config = list(), messages = list())
  expect_warning(core <- call_llm_par(empty), "No experiments provided")
  expect_s3_class(core, "llmr_experiment")
  expect_match(capture.output(print(core)), "0 runs")
  expect_match(report(core), "contains no calls")

  cfg <- llm_config("openai", "model-a")
  expect_warning(
    sweep <- call_llm_sweep(cfg, "temperature", numeric(0), "hi"),
    "No parameter values"
  )
  expect_s3_class(sweep, "llmr_experiment")
  expect_match(report(sweep), "contains no calls")

  expect_warning(compare <- call_llm_compare(list(), "hi"), "No configs")
  expect_s3_class(compare, "llmr_experiment")
  expect_match(report(compare), "contains no calls")
})
