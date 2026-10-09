library(testthat)
library(LLMR)

# call_llm_par_structured() and call_llm_par_tags() fill {column} placeholders
# from the experiments frame without evaluating anything inside braces.

capture_sent <- function(runner, ex, ...) {
  sent <- NULL
  fake_par <- function(experiments, ...) {
    sent <<- experiments$messages
    experiments$response_text <- '{"label":"pos"}'
    experiments$success <- TRUE
    experiments$finish_reason <- "stop"
    experiments
  }
  testthat::local_mocked_bindings(call_llm_par = fake_par, .package = "LLMR")
  runner(ex, ...)
  sent
}

make_ex <- function(messages, ...) {
  cfg <- llm_config("openai", "gpt-4o-mini")
  tibble::tibble(config = rep(list(cfg), length(messages)), messages = messages, ...)
}

test_that("a JSON example does not stop {column} substitution", {
  ex <- make_ex(list('Answer as JSON like {"label": "pos"}. Text: {text}'),
                text = "I love it")
  sent <- capture_sent(call_llm_par_structured, ex)
  expect_identical(sent[[1]], 'Answer as JSON like {"label": "pos"}. Text: I love it')
})

test_that("braces that name no column are sent as written", {
  ex <- make_ex(list('Solve \\frac{1}{2} + x^{2}', 'Nested {"a": {"b": 1}} for {id}'),
                id = c("r1", "r2"))
  sent <- capture_sent(call_llm_par_structured, ex)
  expect_identical(sent[[1]], 'Solve \\frac{1}{2} + x^{2}')
  expect_identical(sent[[2]], 'Nested {"a": {"b": 1}} for r2')
})

test_that("code inside braces is never evaluated", {
  canary <- new.env()
  assign("ran", FALSE, envir = canary)
  assign("canary_env", canary, envir = globalenv())
  on.exit(rm("canary_env", envir = globalenv()), add = TRUE)
  msg <- "Data: {assign('ran', TRUE, envir = canary_env); 'x'} and {nchar('abc')}"
  ex <- make_ex(list(msg))
  sent <- capture_sent(call_llm_par_structured, ex)
  expect_false(get("ran", envir = canary))
  expect_identical(sent[[1]], msg)
})

test_that("values from the data are inserted once, not re-read as placeholders", {
  ex <- make_ex(list("Rate: {text}"), text = "Great value {T} for {id}", id = "r1")
  sent <- capture_sent(call_llm_par_structured, ex)
  expect_identical(sent[[1]], "Rate: Great value {T} for {id}")
})

test_that("a doubled pair sends the literal placeholder", {
  ex <- make_ex(list("Rate: {{text}} and {{other}}"), text = "I love it")
  sent <- capture_sent(call_llm_par_structured, ex)
  expect_identical(sent[[1]], "Rate: {text} and {other}")
})

test_that("missing values become empty text and factors their labels", {
  ex <- make_ex(list("A={a} B={b} C={ c }"),
                a = NA_character_, b = factor("level2"), c = 3.5)
  sent <- capture_sent(call_llm_par_structured, ex)
  expect_identical(sent[[1]], "A= B=level2 C=3.5")
})

test_that("message roles are kept and non-character messages pass through", {
  ex <- make_ex(list(c(system = "You rate {topic}.", user = "Rate: {text}"),
                     list(list(role = "user", content = "Rate: {text}"))),
                topic = c("films", "books"), text = c("good", "bad"))
  sent <- capture_sent(call_llm_par_structured, ex)
  expect_identical(sent[[1]], c(system = "You rate films.", user = "Rate: good"))
  expect_identical(sent[[2]], list(list(role = "user", content = "Rate: {text}")))
})

test_that("a placeholder that matches no column warns and is sent as written", {
  ex <- make_ex(list("Rate: {txet}"), text = "good")
  expect_warning(sent <- capture_sent(call_llm_par_structured, ex),
                 "\\{txet\\}.*match no metadata column")
  expect_identical(sent[[1]], "Rate: {txet}")
})

test_that("JSON keys and LaTeX arguments do not trigger the warning", {
  ex <- make_ex(list('Like {"label": 1} and \\frac{1}{2}: {text}'), text = "ok")
  expect_no_warning(capture_sent(call_llm_par_structured, ex))
})

test_that("call_llm_par_tags fills placeholders before adding tag instructions", {
  ex <- make_ex(list('Like {"label": 1}. Text: {text}'), text = "I love it")
  sent <- capture_sent(call_llm_par_tags, ex, .tags = "label")
  user_text <- if (is.character(sent[[1]]) && "user" %in% names(sent[[1]])) {
    sent[[1]][["user"]]
  } else {
    paste(unlist(sent[[1]]), collapse = "\n")
  }
  expect_match(user_text, 'Like {"label": 1}. Text: I love it', fixed = TRUE)
  expect_false(grepl("{text}", paste(unlist(sent[[1]]), collapse = "\n"), fixed = TRUE))
})
