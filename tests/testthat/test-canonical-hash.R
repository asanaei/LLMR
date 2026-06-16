library(testthat)
library(LLMR)

# The unification invariant: a call described by a config + messages and the same
# call read from a logged provider body must hash IDENTICALLY. Offline.

# log side: hash a logged provider request body the way llm_log_read() does.
log_side_hash <- function(provider, model, request) {
  llm_request_hash(
    provider = provider, model = model,
    messages = LLMR:::.llmr_messages_from_turns(
      LLMR:::.llmr_turns(provider = provider, request = request)),
    extra = list(params = LLMR:::.llmr_body_params(request)))
}

test_that("config-side and log-side hashes are identical for the same call", {
  cfg <- llm_config("openai", "gpt-4o-mini", temperature = 0)
  h_cfg <- llm_request_hash(cfg, c(user = "Capital of France?"))
  body <- list(messages = list(list(role = "user", content = "Capital of France?")),
               temperature = 0)
  h_log <- log_side_hash("openai", "gpt-4o-mini", body)
  expect_identical(h_cfg, h_log)
})

test_that("temperature still distinguishes calls", {
  base <- llm_request_hash(llm_config("openai", "gpt-4o-mini", temperature = 0),
                           c(user = "Capital of France?"))
  hot  <- llm_request_hash(llm_config("openai", "gpt-4o-mini", temperature = 1),
                           c(user = "Capital of France?"))
  expect_false(identical(base, hot))
})

test_that("message shape no longer changes the hash", {
  cfg <- llm_config("openai", "gpt-4o-mini", temperature = 0)
  hA <- llm_request_hash(cfg, c(user = "hi"))
  hB <- llm_request_hash(cfg, list(list(role = "user", content = "hi")))
  expect_identical(hA, hB)
})

test_that("a non-4-tuple param (presence_penalty) is NOT lost", {
  # the old 4-tuple replay key would have wrongly collided these
  h0 <- llm_request_hash(llm_config("openai", "gpt-4o-mini", temperature = 0,
                                    presence_penalty = 0), c(user = "hi"))
  h2 <- llm_request_hash(llm_config("openai", "gpt-4o-mini", temperature = 0,
                                    presence_penalty = 2), c(user = "hi"))
  expect_false(identical(h0, h2))
})

test_that("the log side also picks up a body-only param, and agrees with config", {
  hl0 <- log_side_hash("openai", "gpt-4o-mini",
    list(messages = list(list(role = "user", content = "hi")),
         temperature = 0, presence_penalty = 0))
  hl2 <- log_side_hash("openai", "gpt-4o-mini",
    list(messages = list(list(role = "user", content = "hi")),
         temperature = 0, presence_penalty = 2))
  expect_false(identical(hl0, hl2))
  hc2 <- llm_request_hash(llm_config("openai", "gpt-4o-mini", temperature = 0,
                                     presence_penalty = 2), c(user = "hi"))
  expect_identical(hc2, hl2)
})

test_that("a Gemini-style body hashes the same as its OpenAI-style twin", {
  # same question + temperature, two provider body shapes -> same identity
  open_body <- list(messages = list(list(role = "user", content = "hi")),
                    temperature = 0)
  gem_body  <- list(contents = list(list(role = "user",
                      parts = list(list(text = "hi")))),
                    generationConfig = list(temperature = 0))
  expect_identical(
    log_side_hash("openai", "demo", open_body),
    log_side_hash("openai", "demo", gem_body))
})

test_that("config takes precedence over extra params and they are not double-counted", {
  cfg <- llm_config("openai", "gpt-4o-mini", temperature = 0)
  # supplying extra$params alongside a config must not change the hash (config
  # wins; params are consumed, never hashed twice)
  h_cfg <- llm_request_hash(cfg, "hi")
  h_both <- llm_request_hash(cfg, "hi", extra = list(params = list(temperature = 9)))
  expect_identical(h_cfg, h_both)
})

test_that("a tool-result turn does not collapse to a user turn", {
  base <- llm_request_hash(provider = "openai", model = "demo",
    messages = list(list(role = "user", content = "x")))
  with_tool <- llm_request_hash(provider = "openai", model = "demo",
    messages = list(list(role = "user", content = "x"),
                    list(role = "tool", content = "result")))
  as_user <- llm_request_hash(provider = "openai", model = "demo",
    messages = list(list(role = "user", content = "x"),
                    list(role = "user", content = "result")))
  expect_false(identical(with_tool, as_user))
  expect_false(identical(base, with_tool))
})

test_that("a scalar stop and a one-element stop array agree", {
  hs <- llm_request_hash(provider = "openai", model = "demo", messages = "hi",
                         extra = list(params = list(stop = "END")))
  hl <- llm_request_hash(provider = "openai", model = "demo", messages = "hi",
                         extra = list(params = list(stop = list("END"))))
  expect_identical(hs, hl)
})

test_that("transport and internal knobs never enter the hash", {
  base <- llm_request_hash(llm_config("openai", "gpt-4o-mini", temperature = 0), "hi")
  for (knob in list(list(request_modifier = identity), list(cache = TRUE),
                    list(use_responses_api = TRUE), list(vertex = TRUE),
                    list(stream = TRUE))) {
    cfg <- do.call(llm_config,
      c(list("openai", "gpt-4o-mini", temperature = 0), knob))
    expect_identical(llm_request_hash(cfg, "hi"), base,
                     info = paste("knob:", names(knob)))
  }
})
