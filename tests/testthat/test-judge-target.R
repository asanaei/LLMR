library(testthat)
library(LLMR)

# llm_judge() warns when its prompt never references the text being judged.

judge_prompt_warns <- function(prompt, data = data.frame(text = "a brief")) {
  testthat::local_mocked_bindings(
    llm_mutate_tags = function(.data, ...) .data, .package = "LLMR")
  warned <- FALSE
  withCallingHandlers(
    llm_judge(data, .target = text, .config = llm_config("groq", "m"),
              prompt = prompt),
    warning = function(w) {
      if (grepl("does not see the text", conditionMessage(w))) warned <<- TRUE
      invokeRestart("muffleWarning")
    })
  warned
}

test_that("a prompt with the target placeholder does not warn", {
  expect_false(judge_prompt_warns("Rate this: {.target}"))
  expect_false(judge_prompt_warns("Rate this: { .target }"))
  expect_false(judge_prompt_warns("Rate this: {text}"))
  expect_false(judge_prompt_warns(c(system = "You are a judge.", user = "Rate {.target}")))
})

test_that("a prompt without the target warns", {
  expect_true(judge_prompt_warns("Rate this persona brief on a 0-1 scale."))
  expect_true(judge_prompt_warns("Rate {other} only"))
  # a column whose name has regex metacharacters is matched literally
  d <- data.frame(`a.b` = "x", check.names = FALSE)
  testthat::local_mocked_bindings(
    llm_mutate_tags = function(.data, ...) .data, .package = "LLMR")
  expect_no_warning(llm_judge(d, .target = `a.b`, .config = llm_config("groq", "m"),
                              prompt = "Rate {a.b}"))
  expect_warning(llm_judge(d, .target = `a.b`, .config = llm_config("groq", "m"),
                           prompt = "Rate {aXb}"), "does not see the text")
})
