test_that("enable_structured_output sets response_format for OpenAI-compatible", {
  cfg <- llm_config("openai","gpt-4.1-nano")  # no API call; no key needed
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

test_that("llm_parse_tags extracts XML-like tags", {
  s <- "Answer:\n```xml\n<age>21</age>\n<job>student</job>\n```"
  out <- llm_parse_tags(s, tags = c("age", "job"))
  expect_identical(out$age, "21")
  expect_identical(out$job, "student")
})

test_that("llm_parse_tags_col adds flags and typed fields", {
  df <- data.frame(response_text = "<age>21</age><job>student</job>", stringsAsFactors = FALSE)
  out <- llm_parse_tags_col(df, tags = c("age", "job"))
  expect_true(all(c("tags_ok", "tags_data", "age", "job") %in% names(out)))
  expect_true(out$tags_ok[[1]])
  expect_equal(out$age[[1]], 21)
  expect_identical(out$job[[1]], "student")
})

test_that("llm_parse_tags_col supports renamed fields and missing tags", {
  df <- data.frame(response_text = c("<age>21</age>", "no tags"), stringsAsFactors = FALSE)
  out <- llm_parse_tags_col(df, tags = c("age", "job"), fields = c(person_age = "age"))
  expect_false(out$tags_ok[[2]])
  expect_equal(out$person_age[[1]], 21)
  expect_true(is.na(out$person_age[[2]]))
})
