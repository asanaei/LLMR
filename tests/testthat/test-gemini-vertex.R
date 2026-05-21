test_that("Gemini Vertex mode builds Vertex endpoints without a provider alias", {
  vertex_endpoint <- getFromNamespace(".gemini_vertex_endpoint", "LLMR")

  cfg <- llm_config(
    provider = "gemini",
    model = "gemini-2.5-flash-lite",
    vertex = TRUE,
    project = "test-project",
    location = "europe-west4"
  )

  expect_equal(
    vertex_endpoint(cfg, "generateContent"),
    "https://europe-west4-aiplatform.googleapis.com/v1/projects/test-project/locations/europe-west4/publishers/google/models/gemini-2.5-flash-lite:generateContent"
  )
  expect_equal(
    vertex_endpoint(cfg, "embedContent"),
    "https://europe-west4-aiplatform.googleapis.com/v1/projects/test-project/locations/europe-west4/publishers/google/models/gemini-2.5-flash-lite:embedContent"
  )
  expect_equal(cfg$api_key$ref, "VERTEX_ACCESS_TOKEN")
})

test_that("Gemini auth headers switch only in Vertex mode", {
  auth_headers <- getFromNamespace(".gemini_auth_headers", "LLMR")
  old_vertex <- Sys.getenv("LLMR_TEST_VERTEX_TOKEN", unset = NA_character_)
  old_gemini <- Sys.getenv("LLMR_TEST_GEMINI_TOKEN", unset = NA_character_)
  on.exit({
    if (is.na(old_vertex)) Sys.unsetenv("LLMR_TEST_VERTEX_TOKEN") else Sys.setenv(LLMR_TEST_VERTEX_TOKEN = old_vertex)
    if (is.na(old_gemini)) Sys.unsetenv("LLMR_TEST_GEMINI_TOKEN") else Sys.setenv(LLMR_TEST_GEMINI_TOKEN = old_gemini)
  }, add = TRUE)

  Sys.setenv(
    LLMR_TEST_VERTEX_TOKEN = "test-token",
    LLMR_TEST_GEMINI_TOKEN = "test-token"
  )

  cfg_vertex <- llm_config(
    provider = "gemini",
    model = "gemini-2.5-flash-lite",
    vertex = TRUE,
    project = "test-project",
    api_key = "LLMR_TEST_VERTEX_TOKEN"
  )
  cfg_dev <- llm_config(
    provider = "gemini",
    model = "gemini-2.5-flash-lite",
    api_key = "LLMR_TEST_GEMINI_TOKEN"
  )

  expect_true("Authorization" %in% names(auth_headers(cfg_vertex)))
  expect_false("x-goog-api-key" %in% names(auth_headers(cfg_vertex)))
  expect_true("x-goog-api-key" %in% names(auth_headers(cfg_dev)))
  expect_false("Authorization" %in% names(auth_headers(cfg_dev)))
})

test_that("Gemini Vertex mode requires a project", {
  vertex_endpoint <- getFromNamespace(".gemini_vertex_endpoint", "LLMR")
  cfg <- llm_config(
    provider = "gemini",
    model = "gemini-2.5-flash-lite",
    vertex = TRUE
  )
  expect_error(vertex_endpoint(cfg), "requires `project`")
})
