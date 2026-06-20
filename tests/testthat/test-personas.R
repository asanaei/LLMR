# Offline tests for the persona-frame contract helpers and the bundled dataset.

test_that("anes_2024_personas loads and is well-formed", {
  data(anes_2024_personas, package = "LLMR")
  expect_s3_class(anes_2024_personas, "data.frame")
  expect_equal(nrow(anes_2024_personas), 100L)
  expect_true("ideology_score" %in% names(anes_2024_personas))
  expect_false(is.unsorted(anes_2024_personas$ideology_score))  # liberal -> conservative
  expect_false(any(grepl("V240001|case ?id", names(anes_2024_personas),
                         ignore.case = TRUE)))                  # no respondent ids
})

test_that("llm_persona_dictionary and demographic_fields read the attributes", {
  data(anes_2024_personas, package = "LLMR")
  d <- llm_persona_dictionary(anes_2024_personas)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("handle", "question") %in% names(d)))
  df <- llm_persona_demographic_fields(anes_2024_personas)
  expect_true(length(df) >= 10)
  expect_true(all(df %in% names(anes_2024_personas)))
})

test_that("llm_persona_split keys by question wording and drops the score", {
  data(anes_2024_personas, package = "LLMR")
  s <- llm_persona_split(anes_2024_personas, 1)
  expect_named(s, c("demographics", "responses"))
  # question wording, not the tidy handle
  expect_true("Party identification" %in% names(s$responses))
  expect_false(any(grepl("^att_|^demo_", names(s$responses))))
  # the score is metadata, never an answer
  expect_false("ideology_score" %in% names(s$responses))
  expect_false(any(is.na(s$demographics)))   # NA fields dropped
})

test_that("the helpers degrade gracefully on a plain frame (no attributes)", {
  pf <- data.frame(age = c("30", "40"), party = c("D", "R"),
                   stringsAsFactors = FALSE)
  # age is a common demographic name; party falls to responses
  s <- llm_persona_split(pf, 1)
  expect_equal(unname(s$demographics["age"]), "30")
  expect_true("party" %in% names(s$responses))
  # handles stand in for questions when there is no dictionary
  expect_null(llm_persona_dictionary(pf))
})

test_that("llm_persona_overview returns a compact display frame", {
  data(anes_2024_personas, package = "LLMR")
  ov <- llm_persona_overview(anes_2024_personas)
  expect_equal(nrow(ov), 100L)
  expect_true("ideology" %in% names(ov))
  # caller-specified columns by question wording resolve to handles
  ov2 <- llm_persona_overview(anes_2024_personas,
                              columns = c("Party identification", "age"))
  expect_equal(nrow(ov2), 100L)
  expect_true(ncol(ov2) >= 1)
})

test_that("llm_validate_persona_frame is tolerant but rejects non-frames", {
  data(anes_2024_personas, package = "LLMR")
  expect_true(llm_validate_persona_frame(anes_2024_personas))
  expect_true(llm_validate_persona_frame(data.frame(a = 1)))   # minimal but valid
  expect_error(llm_validate_persona_frame(list(1, 2)))
  expect_error(llm_validate_persona_frame(anes_2024_personas[0, ]))  # no rows
})
