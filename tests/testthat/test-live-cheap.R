library(testthat)
library(LLMR)

# Live smoke tests on inexpensive providers. Each block is gated on its API
# key being present and never runs on CRAN. Models are deliberately small:
# gpt-oss-20b (Groq), deepseek-chat, kimi (Moonshot), qwen-flash (Alibaba),
# and gemini-2.5-flash-lite.

test_that("groq: structured output, tools, streaming, logprobs", {
  skip_if_no_env("GROQ_API_KEY")
  skip_on_cran()
  cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 0,
                    max_tokens = 4000)

  # structured output with schema
  schema <- list(
    type = "object",
    properties = list(answer = list(type = "string"),
                      confidence = list(type = "number")),
    required = list("answer", "confidence"),
    additionalProperties = FALSE
  )
  r <- call_llm_robust(enable_structured_output(cfg, schema = schema),
                       "What is the capital of Chile?")
  parsed <- llm_parse_structured(r)
  expect_true(is.list(parsed))
  expect_match(parsed$answer, "Santiago", ignore.case = TRUE)

  # tool loop
  lookup <- llm_tool(
    function(country) if (identical(tolower(country), "chile")) "Santiago" else "unknown",
    name = "capital_lookup",
    description = "Returns the capital city of a country.",
    parameters = list(country = list(type = "string", description = "Country name"))
  )
  rt <- call_llm_tools(cfg, "Use the capital_lookup tool to find the capital of Chile, then answer in one word.",
                       tools = lookup)
  expect_s3_class(rt, "llmr_response")
  hist <- attr(rt, "tool_history")
  expect_gte(nrow(hist), 1L)
  expect_match(as.character(rt), "Santiago", ignore.case = TRUE)

  # streaming
  chunks <- character(0)
  rs <- call_llm_stream(cfg, "Count from 1 to 5, digits only.",
                        callback = function(ch) chunks <<- c(chunks, ch))
  expect_s3_class(rs, "llmr_response")
  expect_gt(length(chunks), 0L)
  expect_match(as.character(rs), "5")
})

test_that("deepseek: basic call and cached-token plumbing", {
  skip_if_no_env("DEEPSEEK_API_KEY")
  skip_on_cran()
  cfg <- llm_config("deepseek", "deepseek-chat", temperature = 0,
                    max_tokens = 100)
  r <- call_llm_robust(cfg, "Reply with exactly one word: ready")
  expect_s3_class(r, "llmr_response")
  expect_match(as.character(r), "ready", ignore.case = TRUE)
  expect_true(is.list(tokens(r)) && "cached" %in% names(tokens(r)))
})

test_that("moonshot (kimi): basic call", {
  skip_if_no_env("MOONSHOT_API_KEY")
  skip_on_cran()
  cfg <- llm_config("moonshot", "moonshot-v1-8k", temperature = 0,
                    max_tokens = 100)
  r <- call_llm_robust(cfg, "Reply with exactly one word: ready")
  expect_s3_class(r, "llmr_response")
  expect_true(nzchar(as.character(r)))
})

test_that("alibaba (qwen): basic call", {
  skip_if_no_env("ALIBABA_API_KEY")
  skip_on_cran()
  cfg <- llm_config("alibaba", "qwen-flash", temperature = 0,
                    max_tokens = 100, enable_thinking = FALSE)
  r <- call_llm_robust(cfg, "Reply with exactly one word: ready")
  expect_s3_class(r, "llmr_response")
  expect_true(nzchar(as.character(r)))
})

test_that("gemini: thinking budget, schema, and embeddings", {
  skip_if_no_env("GEMINI_API_KEY")
  skip_on_cran()

  cfg <- llm_config("gemini", "gemini-2.5-flash-lite", temperature = 0,
                    max_tokens = 2000, thinking_budget = 512)
  r <- call_llm_robust(cfg, "What is 12 * 12? Answer with the number only.")
  expect_s3_class(r, "llmr_response")
  expect_match(as.character(r), "144")

  # schema via responseJsonSchema
  schema <- list(type = "object",
                 properties = list(city = list(type = "string")),
                 required = list("city"))
  r2 <- call_llm_robust(
    enable_structured_output(cfg, schema = schema),
    "Return the capital of Japan."
  )
  parsed <- llm_parse_structured(r2)
  expect_match(parsed$city, "Tokyo", ignore.case = TRUE)

  # embeddings through the batched endpoint (one HTTP call for both texts)
  ecfg <- llm_config("gemini", "gemini-embedding-001", embedding = TRUE)
  emb <- get_batched_embeddings(c("political science", "sociology"), ecfg)
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 2L)
  expect_gt(ncol(emb), 10L)
})

test_that("groq: batch API submit/status/cancel round trip", {
  skip_if_no_env("GROQ_API_KEY")
  skip_on_cran()
  skip_if(!nzchar(Sys.getenv("LLMR_RUN_BATCH_TESTS")),
          "Set LLMR_RUN_BATCH_TESTS=true to run batch-API tests")
  cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 0,
                    max_tokens = 20)
  job <- llm_batch_submit(cfg, c("2+2? Digits only.", "3+3? Digits only."))
  expect_s3_class(job, "llmr_batch_job")
  st <- llm_batch_status(job)
  expect_identical(st$batch_id, job$batch_id)
  expect_true(nzchar(st$status))
  # do not wait for completion in a test; cancel to clean up
  expect_no_error(llm_batch_cancel(job))
})
