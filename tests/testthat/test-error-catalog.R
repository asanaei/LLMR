# The provider error catalog: shared status mapping, per-provider code
# strings, wording-based billing detection, body-shape tolerance, and the
# no-retry contract for billing blocks.

test_that("the shared status map covers the documented codes", {
  expect_identical(LLMR:::.llmr_classify_error("openai", 400), "param")
  expect_identical(LLMR:::.llmr_classify_error("openai", 401), "auth")
  expect_identical(LLMR:::.llmr_classify_error("deepseek", 402), "billing")
  expect_identical(LLMR:::.llmr_classify_error("xai", 403), "auth")
  expect_identical(LLMR:::.llmr_classify_error("ollama", 404), "param")
  expect_identical(LLMR:::.llmr_classify_error("anthropic", 409), "server")
  expect_identical(LLMR:::.llmr_classify_error("anthropic", 413), "param")
  expect_identical(LLMR:::.llmr_classify_error("deepseek", 422), "param")
  expect_identical(LLMR:::.llmr_classify_error("groq", 429), "rate_limit")
  expect_identical(LLMR:::.llmr_classify_error("groq", 498), "server")
  expect_identical(LLMR:::.llmr_classify_error("together", 524), "server")
  expect_identical(LLMR:::.llmr_classify_error("anthropic", 529), "server")
})

test_that("provider code strings override the status where documented", {
  # The same code string means opposite things at these two providers.
  expect_identical(
    LLMR:::.llmr_classify_error("openai", 429, code = "insufficient_quota"),
    "billing"
  )
  expect_identical(
    LLMR:::.llmr_classify_error("alibaba", 429, code = "insufficient_quota"),
    "rate_limit"
  )
  # Moonshot's three causes behind one status.
  expect_identical(
    LLMR:::.llmr_classify_error("moonshot", 429, code = "exceeded_current_quota_error"),
    "billing"
  )
  expect_identical(
    LLMR:::.llmr_classify_error("moonshot", 429, code = "engine_overloaded_error"),
    "server"
  )
  expect_identical(
    LLMR:::.llmr_classify_error("gemini", 400, code = "FAILED_PRECONDITION"),
    "billing"
  )
  expect_identical(
    LLMR:::.llmr_classify_error("zhipu", 429, code = "1113"),
    "billing"
  )
  expect_identical(
    LLMR:::.llmr_classify_error("alibaba", 400, code = "Arrearage"),
    "billing"
  )
  # Together documents 403 as context-window overflow, not a permission issue.
  expect_identical(LLMR:::.llmr_classify_error("together", 403), "param")
})

test_that("spend-cap wording reclassifies generic 4xx but never a 429", {
  expect_identical(
    LLMR:::.llmr_classify_error(
      "anthropic", 400, code = "invalid_request_error",
      message = "You have reached your specified API usage limits."
    ),
    "billing"
  )
  # Gemini per-minute quotas arrive as 429 with quota wording; they recover.
  expect_identical(
    LLMR:::.llmr_classify_error(
      "gemini", 429, code = "RESOURCE_EXHAUSTED",
      message = "You exceeded your current quota. Please try again later."
    ),
    "rate_limit"
  )
})

test_that("error bodies of every documented shape yield a readable reason", {
  # OpenAI and compatibles: an error object with message/type/param/code.
  f <- LLMR:::.llmr_error_fields(list(error = list(
    message = "Invalid 'max_tokens'.", type = "invalid_request_error",
    param = "max_tokens", code = "integer_below_min_value"
  )))
  expect_identical(f$reason, "Invalid 'max_tokens'.")
  expect_identical(f$code, "integer_below_min_value")
  expect_identical(f$param, "max_tokens")

  # Gemini: numeric code plus a status string; the string must win.
  f <- LLMR:::.llmr_error_fields(list(error = list(
    code = 429L, message = "Quota exceeded.", status = "RESOURCE_EXHAUSTED"
  )))
  expect_identical(f$code, "RESOURCE_EXHAUSTED")

  # Ollama: a bare string under `error`.
  f <- LLMR:::.llmr_error_fields(list(error = "model 'nope' not found"))
  expect_identical(f$reason, "model 'nope' not found")

  # Voyage: a top-level `detail` string.
  f <- LLMR:::.llmr_error_fields(list(detail = "Provided API key is invalid."))
  expect_identical(f$reason, "Provided API key is invalid.")

  # DashScope native: top-level code/message.
  f <- LLMR:::.llmr_error_fields(list(code = "Arrearage", message = "Account in arrears."))
  expect_identical(f$reason, "Account in arrears.")
  expect_identical(f$code, "Arrearage")

  # Unparseable body: the raw tail is the reason.
  f <- LLMR:::.llmr_error_fields(try(stop("x"), silent = TRUE), raw_tail = "<html>502</html>")
  expect_identical(f$reason, "<html>502</html>")
})

test_that("billing errors fail fast instead of burning retries", {
  skip_if_not_installed("withr")
  attempts <- 0L
  testthat::local_mocked_bindings(
    call_llm = function(config, messages, verbose = FALSE) {
      attempts <<- attempts + 1L
      LLMR:::.llmr_error(
        "LLM API request failed.\nHTTP status: 429\nReason: out of credit.",
        category = "billing", status_code = 429L, provider = "openai",
        model = "m", code = "insufficient_quota"
      )
    },
    .package = "LLMR"
  )
  withr::local_envvar(OPENAI_API_KEY = "fake-key-for-offline-test")
  cfg <- llm_config("openai", "m")
  err <- tryCatch(
    suppressMessages(call_llm_robust(cfg, "hi", tries = 4, wait_seconds = 0.01)),
    error = identity
  )
  expect_s3_class(err, "llmr_api_billing_error")
  expect_identical(attempts, 1L)

  # The same wrapper still retries a rate limit.
  attempts <- 0L
  testthat::local_mocked_bindings(
    call_llm = function(config, messages, verbose = FALSE) {
      attempts <<- attempts + 1L
      LLMR:::.llmr_error(
        "LLM API request failed.", category = "rate_limit",
        status_code = 429L, provider = "openai", model = "m"
      )
    },
    .package = "LLMR"
  )
  err <- tryCatch(
    suppressMessages(call_llm_robust(cfg, "hi", tries = 2, wait_seconds = 0.01)),
    error = identity
  )
  expect_s3_class(err, "llmr_api_rate_limit_error")
  expect_identical(attempts, 2L)
})
