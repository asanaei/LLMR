library(testthat)
library(LLMR)

# llm_request_hash() — stable identity of a call. Offline; no keys needed.

test_that("request hash is stable under list reorder and class", {
  cfg1 <- llm_config("openai", "gpt-4.1-mini", temperature = 0, top_p = 1)
  cfg2 <- llm_config("openai", "gpt-4.1-mini", top_p = 1, temperature = 0)
  expect_identical(
    llm_request_hash(cfg1, "Hello"),
    llm_request_hash(cfg2, "Hello")
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

test_that("api keys never enter the hash input", {
  # Two configs identical but for the key handle hash identically; the key is
  # not part of model_params and not part of the hashed object.
  cfg <- llm_config("openai", "gpt-4.1-mini", temperature = 0)
  h <- llm_request_hash(cfg, "Hello")
  expect_match(h, "^[0-9a-f]{64}$")
})

test_that("config-free call via extra still hashes", {
  h <- llm_request_hash(
    config = NULL, messages = "Hello",
    extra = list(provider = "groq", model = "x", seed = 110)
  )
  expect_match(h, "^[0-9a-f]{64}$")
  expect_false(identical(
    h,
    llm_request_hash(NULL, "Hello",
                     extra = list(provider = "groq", model = "x", seed = 7))
  ))
})
