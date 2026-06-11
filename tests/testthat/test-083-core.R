library(testthat)
library(LLMR)

# Offline regression tests for the 0.8.3 core-path fixes. The request builders
# are exercised directly (they build httr2 requests without performing them),
# so every provider body can be inspected without a network call.

# ---- request builders: OpenAI-compatible -----------------------------------

test_that("compat builder forwards canonical params, seed, and logprobs", {
  cfg <- llm_config("groq", "openai/gpt-oss-20b",
                    temperature = 0.2, top_p = 0.9, max_tokens = 50,
                    seed = 110, logprobs = TRUE, top_logprobs = 3)
  req <- LLMR:::.compat_chat_request(cfg, "hi",
            endpoint = "https://api.groq.com/openai/v1/chat/completions",
            auth_style = "none")
  b <- req$body$data
  expect_identical(b$model, "openai/gpt-oss-20b")
  expect_equal(b$temperature, 0.2)
  expect_equal(b$seed, 110)
  expect_true(isTRUE(b$logprobs))
  expect_equal(b$top_logprobs, 3)
  expect_equal(b$max_tokens, 50)
})

test_that("compat builder drops provider-unsupported params with a note", {
  cfg <- llm_config("groq", "openai/gpt-oss-20b", top_k = 5)
  req <- LLMR:::.compat_chat_request(cfg, "hi",
            endpoint = "http://x", auth_style = "none",
            drop_params = c("top_k", "repetition_penalty"))
  expect_null(req$body$data$top_k)
})

test_that("compat builder honors max_completion_tokens directly", {
  cfg <- llm_config("deepseek", "deepseek-chat", max_completion_tokens = 99)
  req <- LLMR:::.compat_chat_request(cfg, "hi", endpoint = "http://x",
                                     auth_style = "none")
  expect_equal(req$body$data$max_completion_tokens, 99)
})

test_that("compat builder converts multimodal file parts to data URLs", {
  tmp <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), tmp)
  on.exit(unlink(tmp))
  cfg <- llm_config("groq", "llava")
  msg <- c(user = "describe", file = tmp)
  req <- LLMR:::.compat_chat_request(cfg, msg, endpoint = "http://x",
                                     auth_style = "none")
  parts <- req$body$data$messages[[1]]$content
  types <- vapply(parts, `[[`, "", "type")
  expect_setequal(types, c("text", "image_url"))
  img <- parts[[which(types == "image_url")]]
  expect_match(img$image_url$url, "^data:image/png;base64,")
})

test_that("compat builder forwards unknown extras verbatim", {
  cfg <- llm_config("alibaba", "qwen-flash", enable_thinking = FALSE)
  req <- LLMR:::.compat_chat_request(cfg, "hi", endpoint = "http://x",
                                     auth_style = "none")
  expect_false(isTRUE(req$body$data$enable_thinking))
  expect_true("enable_thinking" %in% names(req$body$data))
})

test_that("timeout/cache/hook keys never leak into a request body", {
  cfg <- llm_config("groq", "m", timeout = 30, cache = TRUE,
                    req_builder = identity, response_modifier = identity,
                    request_modifier = NULL)
  req <- LLMR:::.compat_chat_request(cfg, "hi", endpoint = "http://x",
                                     auth_style = "none")
  expect_false(any(c("timeout", "cache", "req_builder", "response_modifier") %in%
                     names(req$body$data)))
})

# ---- request builders: Anthropic --------------------------------------------

test_that("anthropic builder keeps top_k and validates thinking budget", {
  withr_env <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA)
  Sys.setenv(ANTHROPIC_API_KEY = "test-key-not-real")
  on.exit(if (is.na(withr_env)) Sys.unsetenv("ANTHROPIC_API_KEY")
          else Sys.setenv(ANTHROPIC_API_KEY = withr_env), add = TRUE)

  cfg <- llm_config("anthropic", "claude-x", max_tokens = 300, top_k = 40)
  req <- LLMR:::.anthropic_chat_request(cfg, "hi")
  expect_equal(req$body$data$top_k, 40)

  cfg2 <- llm_config("anthropic", "claude-x", max_tokens = 100,
                     thinking_budget = 5000)
  expect_warning(req2 <- LLMR:::.anthropic_chat_request(cfg2, "hi"),
                 "max_tokens > thinking_budget")
  expect_identical(req2$body$data$thinking$type, "enabled")
  expect_equal(req2$body$data$thinking$budget_tokens, 5000)
})

test_that("anthropic cache = TRUE marks the system prompt as cacheable", {
  Sys.setenv(ANTHROPIC_API_KEY = "test-key-not-real")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"), add = TRUE)
  cfg <- llm_config("anthropic", "claude-x", max_tokens = 50, cache = TRUE)
  req <- LLMR:::.anthropic_chat_request(cfg, c(system = "Long shared prefix.",
                                               user = "hi"))
  sys <- req$body$data$system
  expect_true(is.list(sys))
  expect_identical(sys[[1]]$cache_control$type, "ephemeral")
})

test_that("anthropic builder passes typed blocks (tool_result) through", {
  Sys.setenv(ANTHROPIC_API_KEY = "test-key-not-real")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"), add = TRUE)
  cfg <- llm_config("anthropic", "claude-x", max_tokens = 50)
  msgs <- list(
    list(role = "user", content = "hi"),
    list(role = "user", content = list(
      list(type = "tool_result", tool_use_id = "t1", content = "42")
    ))
  )
  req <- LLMR:::.anthropic_chat_request(cfg, msgs)
  blocks <- req$body$data$messages[[2]]$content
  expect_identical(blocks[[1]]$type, "tool_result")
  expect_identical(blocks[[1]]$tool_use_id, "t1")
})

# ---- request builders: Gemini ------------------------------------------------

test_that("gemini builder consumes thinking, seed, penalties, and logprobs", {
  Sys.setenv(GEMINI_API_KEY = "test-key-not-real")
  on.exit(Sys.unsetenv("GEMINI_API_KEY"), add = TRUE)
  cfg <- llm_config("gemini", "gemini-2.5-flash-lite",
                    max_tokens = 64, thinking_budget = 128,
                    include_thoughts = TRUE, seed = 110,
                    presence_penalty = 0.1, logprobs = TRUE, top_logprobs = 2)
  req <- LLMR:::.gemini_chat_request(cfg, "hi")
  gc <- req$body$data$generationConfig
  expect_equal(gc$maxOutputTokens, 64)
  expect_equal(gc$thinkingConfig$thinkingBudget, 128)
  expect_true(isTRUE(gc$thinkingConfig$includeThoughts))
  expect_equal(gc$seed, 110)
  expect_equal(gc$presencePenalty, 0.1)
  expect_true(isTRUE(gc$responseLogprobs))
  expect_equal(gc$logprobs, 2)
})

test_that("gemini schema is sent by default via responseJsonSchema", {
  Sys.setenv(GEMINI_API_KEY = "test-key-not-real")
  on.exit(Sys.unsetenv("GEMINI_API_KEY"), add = TRUE)
  schema <- list(type = "object", properties = list(x = list(type = "string")))
  cfg <- enable_structured_output(llm_config("gemini", "gemini-2.5-flash-lite"),
                                  schema = schema)
  req <- LLMR:::.gemini_chat_request(cfg, "hi")
  gc <- req$body$data$generationConfig
  expect_identical(gc$responseMimeType, "application/json")
  expect_identical(gc$responseJsonSchema$type, "object")

  cfg2 <- llm_config("gemini", "gemini-1.5-flash",
                     gemini_enable_response_schema = FALSE)
  cfg2 <- enable_structured_output(cfg2, schema = schema)
  req2 <- LLMR:::.gemini_chat_request(cfg2, "hi")
  expect_null(req2$body$data$generationConfig$responseJsonSchema)
})

test_that("gemini no_change keeps canonical spellings usable", {
  Sys.setenv(GEMINI_API_KEY = "test-key-not-real")
  on.exit(Sys.unsetenv("GEMINI_API_KEY"), add = TRUE)
  cfg <- llm_config("gemini", "gemini-2.5-flash-lite", max_tokens = 33,
                    no_change = TRUE)
  req <- LLMR:::.gemini_chat_request(cfg, "hi")
  expect_equal(req$body$data$generationConfig$maxOutputTokens, 33)
})

# ---- param translation --------------------------------------------------------

test_that("anthropic no longer drops top_k; include_thoughts is dropped with note", {
  mp <- LLMR:::.translate_params("anthropic",
          list(top_k = 7, include_thoughts = TRUE, thinking_budget = 100))
  expect_equal(mp$top_k, 7)
  expect_null(mp$include_thoughts)
  expect_equal(mp$budget_tokens, 100)
})

test_that("gemini penalties are renamed, not dropped", {
  mp <- LLMR:::.translate_params("gemini",
          list(presence_penalty = 0.5, frequency_penalty = 0.2,
               repetition_penalty = 1.1))
  expect_equal(mp$presencePenalty, 0.5)
  expect_equal(mp$frequencyPenalty, 0.2)
  expect_null(mp$repetition_penalty)   # genuinely unsupported -> dropped
})

# ---- key handling --------------------------------------------------------------

test_that("empty-string api_key falls back to provider defaults with a warning", {
  expect_warning(cfg <- llm_config("openai", "m", api_key = ""),
                 "empty string")
  expect_true(inherits(cfg$api_key, "llmr_secret_env"))
})

test_that("NA api_key is rejected at config time", {
  expect_error(llm_config("openai", "m", api_key = NA_character_), "NA")
})

test_that("vector api_key becomes an ordered env reference", {
  cfg <- llm_config("groq", "m", api_key = c("GROQ_API_KEY", "GROQ_KEY"))
  expect_true(inherits(cfg$api_key, "llmr_secret_env"))
  expect_identical(cfg$api_key$ref, c("GROQ_API_KEY", "GROQ_KEY"))
})

test_that("legacy literal key strings resolve without leaking into errors", {
  # pre-secret-era configs stored the key itself as a string
  expect_identical(LLMR:::.resolve_api_key("sk-legacy-literal-key",
                                           provider = "openai"),
                   "sk-legacy-literal-key")
  # empty legacy strings produce a clean error that does not echo a key
  err <- tryCatch(LLMR:::.resolve_api_key("", provider = "openai"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Empty API key")
})

test_that("empty literal secrets error cleanly at resolve time", {
  bad <- structure(list(kind = "literal", value = ""),
                   class = c("llmr_secret", "llmr_secret_literal"))
  expect_error(LLMR:::.resolve_api_key(bad, provider = "openai"),
               "Empty or invalid literal")
})

test_that("print.llm_config masks the key", {
  suppressWarnings(cfg <- llm_config("openai", "m", api_key = "sk-very-secret-123"))
  printed <- paste(capture.output(print(cfg)), collapse = "\n")
  expect_false(grepl("sk-very-secret-123", printed, fixed = TRUE))
  expect_match(printed, "llmr_secret")
})

# ---- response parsing -----------------------------------------------------------

test_that("refusals map to filter and surface their text", {
  content <- list(choices = list(list(
    message = list(content = NULL, refusal = "I cannot help with that."),
    finish_reason = "stop"
  )))
  expect_identical(LLMR:::.std_finish_reason(content), "filter")
  expect_identical(LLMR:::extract_text(content), "I cannot help with that.")
})

test_that("gemini safety verdicts map to filter", {
  content <- list(candidates = list(list(finishReason = "RECITATION")))
  expect_identical(LLMR:::.std_finish_reason(content), "filter")
})

test_that("cached tokens are extracted across provider shapes", {
  expect_equal(LLMR:::.cached_tokens_from(
    list(usage = list(prompt_tokens_details = list(cached_tokens = 42)))), 42L)
  expect_equal(LLMR:::.cached_tokens_from(
    list(usage = list(cache_read_input_tokens = 7))), 7L)
  expect_equal(LLMR:::.cached_tokens_from(
    list(usage = list(prompt_cache_hit_tokens = 5))), 5L)
  expect_equal(LLMR:::.cached_tokens_from(
    list(usageMetadata = list(cachedContentTokenCount = 9))), 9L)
  expect_true(is.na(LLMR:::.cached_tokens_from(list(usage = list()))))
})

test_that("model version and thinking are extracted", {
  expect_identical(LLMR:::.model_version_from(list(model = "m-2026-01-01")),
                   "m-2026-01-01")
  expect_identical(LLMR:::.thinking_from(list(content = list(
    list(type = "thinking", thinking = "step 1"),
    list(type = "text", text = "answer")))), "step 1")
  expect_identical(LLMR:::.thinking_from(list(choices = list(list(
    message = list(reasoning_content = "because..."))))), "because...")
})

test_that("gemini thought parts stay out of the answer text", {
  content <- list(candidates = list(list(content = list(parts = list(
    list(text = "internal musings", thought = TRUE),
    list(text = "final answer")
  )))))
  expect_identical(LLMR:::extract_text(content), "final answer")
  expect_identical(LLMR:::.thinking_from(content), "internal musings")
})

# ---- retry layer -----------------------------------------------------------------

test_that("retry does not sleep after the final attempt", {
  calls <- 0L
  t0 <- Sys.time()
  expect_error(
    LLMR:::retry_with_backoff(
      function() { calls <<- calls + 1L; stop("always fails") },
      tries = 2, initial_wait = 0.2, backoff_factor = 1,
      error_filter_func = function(e) TRUE
    ),
    "always fails"
  ) |> suppressMessages()
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_equal(calls, 2L)
  # one inter-attempt wait (~0.2 s with jitter), not two
  expect_lt(elapsed, 1.0)
})

test_that("a legitimate NULL return is not treated as failure", {
  out <- LLMR:::retry_with_backoff(function() NULL, tries = 3,
                                   initial_wait = 0.01)
  expect_null(out)
})

test_that("permanent typed errors are not retried", {
  calls <- 0L
  fake_call <- function() {
    calls <<- calls + 1L
    LLMR:::.llmr_error("context_length_exceeded: too long",
                       category = "param", status_code = 400L)
  }
  ns <- asNamespace("LLMR")
  orig <- get("call_llm", ns)
  unlockBinding("call_llm", ns)
  assign("call_llm", function(config, messages, verbose = FALSE) fake_call(), ns)
  on.exit({ assign("call_llm", orig, ns); lockBinding("call_llm", ns) }, add = TRUE)

  cfg <- structure(list(provider = "openai", model = "m"),
                   class = c("llm_config", "openai"))
  expect_error(suppressMessages(
    call_llm_robust(cfg, "hi", tries = 5, wait_seconds = 0.01)
  ))
  expect_equal(calls, 1L)
})

test_that("retry honors a provider Retry-After value", {
  calls <- 0L
  t0 <- Sys.time()
  expect_error(suppressMessages(
    LLMR:::retry_with_backoff(
      function() {
        calls <<- calls + 1L
        cond <- structure(
          class = c("llmr_api_rate_limit_error", "llmr_api_error", "error", "condition"),
          list(message = "429", call = NULL, retry_after = 0.3)
        )
        stop(cond)
      },
      tries = 2, initial_wait = 60, backoff_factor = 5,
      error_filter_func = function(e) TRUE
    )
  ))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_equal(calls, 2L)
  expect_lt(elapsed, 5)   # used 0.3 s, not the 60 s schedule
})

# ---- misc fixes -----------------------------------------------------------------

test_that("expand_llm_config keeps S3 dispatch in sync with swept provider", {
  base <- llm_config("openai", "gpt-4.1-nano")
  cfgs <- expand_llm_config(base, provider = c("openai", "anthropic"))
  expect_identical(class(cfgs[[2]]), c("llm_config", "anthropic"))
})

test_that("balanced-segment recovery is string-aware", {
  s <- 'Sure: {"note":"use { sparingly","x":1} hope that helps'
  out <- llm_parse_structured(s)
  expect_identical(out$note, "use { sparingly")
  expect_equal(out$x, 1)
})

test_that("hoisting never silently overwrites an existing column", {
  df <- tibble::tibble(score = c(100, 200),
                       response_text = c('{"score": 1}', '{"score": 2}'))
  expect_warning(out <- llm_parse_structured_col(df, fields = "score"),
                 "already exists")
  expect_identical(out$score, c(100, 200))
  expect_equal(out$score_1, c(1, 2))
})

test_that("tag extraction never silently overwrites an existing column", {
  df <- tibble::tibble(age = c(99, 98),
                       response_text = c("<age>1</age>", "<age>2</age>"))
  expect_warning(out <- llm_parse_tags_col(df, tags = "age"), "already exists")
  expect_identical(out$age, c(99, 98))
  expect_equal(out$age_1, c(1, 2))
})

test_that("row-like tag names are rejected in batched tag mode", {
  cfg <- llm_config("openai", "m")
  expect_error(
    llm_fn_tags(c("a", "b"), "{x}", .config = cfg,
                .tags = c("row_1", "other"), .batch_size = 2),
    "row_N|<row_"
  )
})

test_that("batch instructions state the actual item count", {
  expect_match(LLMR:::.batch_instruction_plain(7), "7 independent items")
  expect_match(LLMR:::.batch_tag_prompt("age", 4), "up to <row_4>")
  expect_match(LLMR:::.batch_struct_instruction(3), "exactly 3 array elements")
})

test_that("assistant turns are rejected by the batchability guard", {
  msgs <- list(c(system = "s", assistant = "prior", user = "u"))
  expect_error(LLMR:::.assert_batchable(msgs), "assistant turns")
})

test_that("strict mode hardens schemas as the providers require", {
  schema <- list(
    type = "object",
    properties = list(
      vote = list(type = "string"),
      detail = list(type = "object",
                    properties = list(why = list(type = "string"))),
      items = list(type = "array",
                   items = list(type = "object",
                                properties = list(x = list(type = "number"))))
    )
  )
  cfg <- enable_structured_output(llm_config("groq", "m"), schema = schema)
  sent <- cfg$model_params$response_format$json_schema$schema
  expect_false(isTRUE(sent$additionalProperties))   # set to FALSE, not TRUE
  expect_identical(sent$additionalProperties, FALSE)
  expect_setequal(unlist(sent$required), c("vote", "detail", "items"))
  expect_identical(sent$properties$detail$additionalProperties, FALSE)
  expect_identical(sent$properties$items$items$additionalProperties, FALSE)
  # the user's original schema is preserved for local validation
  expect_null(cfg$model_params$json_schema$additionalProperties)
  # explicit user choices are never overridden
  s2 <- list(type = "object", additionalProperties = TRUE,
             properties = list(a = list(type = "string")), required = list())
  h2 <- LLMR:::.strict_harden(s2)
  expect_true(isTRUE(h2$additionalProperties))
  expect_length(h2$required, 0L)
})

# ---- provider-path regression fixes ------------------------------------------

test_that("gemini requests label assistant turns as 'model'", {
  old_key <- Sys.getenv("GEMINI_API_KEY", unset = NA)
  Sys.setenv(GEMINI_API_KEY = "test-key-not-real")
  on.exit(if (is.na(old_key)) Sys.unsetenv("GEMINI_API_KEY")
          else Sys.setenv(GEMINI_API_KEY = old_key), add = TRUE)
  cfg <- llm_config("gemini", "gemini-2.5-flash-lite")
  req <- LLMR:::.gemini_chat_request(cfg, list(
    list(role = "user", content = "a"),
    list(role = "assistant", content = "b"),
    list(role = "user", content = "c")
  ))
  roles <- vapply(req$body$data$contents, `[[`, "", "role")
  expect_identical(roles, c("user", "model", "user"))
})

test_that("extract_text concatenates all anthropic text blocks", {
  content <- list(content = list(
    list(type = "text", text = "First."),
    list(type = "thinking", thinking = "internal reasoning"),
    list(type = "text", text = "Second.")
  ))
  expect_identical(extract_text(content), "First.\nSecond.")
})

test_that("anthropic stop_sequence maps to finish reason 'stop'", {
  expect_identical(LLMR:::.std_finish_reason(list(stop_reason = "stop_sequence")),
                   "stop")
})

test_that("partially named .messages templates default empty names to user", {
  msgs <- LLMR:::.llm_build_messages_df(
    data.frame(x = "w"), .messages = c(system = "be terse", "{x}?"))
  expect_identical(names(msgs[[1]]), c("system", "user"))
  expect_identical(unname(msgs[[1]]["user"]), "w?")
})

test_that("schema = NULL still enables JSON-object mode in tidy structured verbs", {
  cfg <- enable_structured_output(llm_config("openai", "gpt-4o"), schema = NULL)
  expect_identical(cfg$model_params$response_format$type, "json_object")
})

test_that("an empty broadcast returns the full diagnostic schema", {
  cfg <- llm_config("groq", "m")
  expect_warning(res <- call_llm_broadcast(cfg, list()), "No experiments")
  expect_true(all(c("response_text", "success", "finish_reason", "sent_tokens",
                    "cached_tokens", "duration", "response") %in% names(res)))
  expect_identical(nrow(res), 0L)
})

test_that("call_llm_tools aggregates loop spend and enforces max_tool_calls", {
  mk_resp <- function(text, sent, rec, tool_call = FALSE) {
    raw <- if (tool_call) {
      list(choices = list(list(message = list(
        role = "assistant", content = NULL,
        tool_calls = list(list(id = "c1", type = "function",
                               `function` = list(name = "dbl",
                                                 arguments = '{"x": 2}')))))))
    } else {
      list(choices = list(list(message = list(role = "assistant", content = text))))
    }
    structure(list(
      text = text, provider = "openai", model = "m", finish_reason = "stop",
      usage = list(sent = sent, rec = rec, total = sent + rec,
                   reasoning = NA_integer_, cached = NA_integer_),
      response_id = "r", duration_s = 0.01, raw = raw, raw_json = "{}"
    ), class = "llmr_response")
  }
  dbl <- llm_tool(function(x) as.character(as.numeric(x) * 2),
                  name = "dbl", description = "doubles",
                  parameters = list(x = list(type = "number")))
  n <- 0L
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) {
      n <<- n + 1L
      if (n == 1L) mk_resp("", 10L, 2L, tool_call = TRUE) else mk_resp("4", 5L, 1L)
    },
    .package = "LLMR"
  )
  cfg <- llm_config("openai", "m")
  r <- call_llm_tools(cfg, "double 2", tools = dbl)
  loop <- attr(r, "tool_loop")
  expect_identical(loop$model_calls, 2L)
  expect_identical(loop$sent, 15L)
  expect_identical(loop$rec, 3L)
  expect_identical(loop$tool_calls, 1L)
  expect_identical(nrow(attr(r, "tool_history")), 1L)

  n <- 0L
  expect_error(call_llm_tools(cfg, "double 2", tools = dbl, max_tool_calls = 0),
               class = "llmr_tool_limit")
})

test_that("llm_hash is canonical, order-blind, and pinned across versions", {
  # the pinned value guards the convention itself: if this test ever fails,
  # the hash function changed and every downstream identifier broke with it
  expect_identical(
    llm_hash(list(model = "gpt-oss-20b", temperature = 0)),
    "7c5ffbb0b308f20bf188a3efd962a2895f45ad202307234ee1965d86abc0606c")
  expect_identical(llm_hash(list(a = 1, b = "x")),
                   llm_hash(list(b = "x", a = 1)))      # order-blind
  expect_false(identical(llm_hash(list(a = 1)), llm_hash(list(a = 2))))
  s3 <- structure(list(a = 1), class = "whatever")
  expect_identical(llm_hash(s3), llm_hash(list(a = 1))) # class-blind
  expect_identical(llm_hash(function(x) x + 1),
                   llm_hash(function(x) x + 1))         # functions by source
  expect_match(llm_hash("plain string"), "^[a-f0-9]{64}$")
})
