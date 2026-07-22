library(testthat)
library(LLMR)

# llm_request_hash() -- stable identity of a call. Offline; no keys needed.

test_that("request hash is stable under list reorder and class", {
  cfg1 <- llm_config("openai", "gpt-4.1-mini", temperature = 0, top_p = 1)
  cfg2 <- llm_config("openai", "gpt-4.1-mini", top_p = 1, temperature = 0)
  expect_identical(
    llm_request_hash(cfg1, "Hello"),
    llm_request_hash(cfg2, "Hello")
  )
})

test_that("the plain-chat request hash remains byte-identical", {
  cfg <- llm_config("openai", "gpt-4o-mini", temperature = 0)
  expect_identical(
    llm_request_hash(cfg, c(system = "Be terse.", user = "Hello")),
    "a35e70aafb008c3875246eee63844231a93ac9ec27d20b2b3440f3b0a713ca88"
  )
})

test_that("request hash changes with generation params", {
  cfg0 <- llm_config("openai", "gpt-4.1-mini", temperature = 0)
  cfg1 <- llm_config("openai", "gpt-4.1-mini", temperature = 1)
  expect_false(identical(
    llm_request_hash(cfg0, "Hello"),
    llm_request_hash(cfg1, "Hello")
  ))
})

test_that("request hash changes with messages, schema, and tools", {
  cfg <- llm_config("openai", "gpt-4.1-mini", temperature = 0)
  base <- llm_request_hash(cfg, "Hello")
  expect_false(identical(base, llm_request_hash(cfg, "Goodbye")))
  expect_false(identical(
    base,
    llm_request_hash(cfg, "Hello", schema = list(type = "object"))
  ))
  expect_false(identical(
    base,
    llm_request_hash(cfg, "Hello", tools = list(list(name = "lookup")))
  ))
})

test_that("transport-only knobs do not change the hash", {
  cfg_a <- llm_config("openai", "gpt-4.1-mini", temperature = 0)
  cfg_b <- llm_config("openai", "gpt-4.1-mini", temperature = 0,
                      timeout = 99, max_tries = 7)
  expect_identical(
    llm_request_hash(cfg_a, "Hello"),
    llm_request_hash(cfg_b, "Hello")
  )
})

test_that("config-free call via direct provider/model and extra params hashes", {
  # The log side has no config; it supplies provider/model directly and the
  # generation parameters through extra$params.
  h <- llm_request_hash(
    provider = "groq", model = "x", messages = "Hello",
    extra = list(params = list(seed = 110))
  )
  expect_match(h, "^[0-9a-f]{64}$")
  expect_false(identical(
    h,
    llm_request_hash(provider = "groq", model = "x", messages = "Hello",
                     extra = list(params = list(seed = 7)))
  ))
})

test_that("message shape does not change the hash (canonicalization)", {
  cfg <- llm_config("openai", "gpt-4.1-mini", temperature = 0)
  expect_identical(
    llm_request_hash(cfg, "Hello"),
    llm_request_hash(cfg, c(user = "Hello"))
  )
  expect_identical(
    llm_request_hash(cfg, c(user = "Hello")),
    llm_request_hash(cfg, list(list(role = "user", content = "Hello")))
  )
})

test_that("structured-output configs hash like their transmitted bodies", {
  # enable_structured_output() stores local bookkeeping (json_schema,
  # llmr_schema_tool) beside the provider-ready field; only the latter is
  # transmitted, so only the latter may enter the identity.
  schema <- list(type = "object",
                 properties = list(label = list(type = "string")),
                 required = list("label"))
  for (provider_model in list(c("openai", "gpt-4o-mini"),
                              c("anthropic", "claude-sonnet-4-5"),
                              c("gemini", "gemini-2.5-flash"))) {
    cfg <- enable_structured_output(
      llm_config(provider_model[1], provider_model[2], temperature = 0),
      schema = schema)
    body_side <- cfg$model_params
    body_side$json_schema <- NULL
    body_side$llmr_schema_tool <- NULL
    expect_identical(
      llm_request_hash(cfg, "Label this."),
      llm_request_hash(NULL, "Label this.",
                       provider = cfg$provider, model = cfg$model,
                       extra = list(params = body_side)),
      info = provider_model[1]
    )
  }
})

test_that("different schemas still hash differently on schema providers", {
  s1 <- list(type = "object", properties = list(a = list(type = "string")))
  s2 <- list(type = "object", properties = list(b = list(type = "number")))
  c1 <- enable_structured_output(llm_config("openai", "gpt-4o-mini"), schema = s1)
  c2 <- enable_structured_output(llm_config("openai", "gpt-4o-mini"), schema = s2)
  expect_false(identical(llm_request_hash(c1, "x"), llm_request_hash(c2, "x")))
})
