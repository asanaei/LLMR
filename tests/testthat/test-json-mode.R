test_that("enable_structured_output sets response_format for OpenAI-compatible", {
  cfg <- llm_config("openai","gpt-4o-mini")  # no API call; no key needed
  cfg2 <- enable_structured_output(cfg) # no schema
  expect_true(!is.null(cfg2$model_params$response_format))
  expect_identical(cfg2$model_params$response_format$type, "json_object")
})

test_that("llm_parse_structured recovers from wrapped text", {
  s <- "Answer:\n```json\n{\"a\":1}\n```"
  out <- llm_parse_structured(s)
  expect_true(is.list(out))
  expect_identical(out$a, 1)
})

test_that("llm_parse_structured_col adds flags", {
  df <- data.frame(response_text = '{"score":5}', stringsAsFactors = FALSE)
  out <- llm_parse_structured_col(df, fields = "score")
  expect_true(all(c("structured_ok","structured_data","score") %in% names(out)))
  expect_true(out$structured_ok[1])
  expect_equal(out$score[1], 5)
})



