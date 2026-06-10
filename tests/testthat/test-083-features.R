library(testthat)
library(LLMR)

# Offline tests for the 0.8.3 features: tools, agreement, usage upgrades,
# audit log, preview checks, NA policy, and batch-API plumbing.

# helper: stub call_llm_broadcast (same seam as test-batch_mode.R)
with_stub_broadcast2 <- function(stub, expr) {
  ns <- asNamespace("LLMR")
  orig <- get("call_llm_broadcast", ns)
  unlockBinding("call_llm_broadcast", ns)
  assign("call_llm_broadcast", stub, ns)
  on.exit({
    assign("call_llm_broadcast", orig, ns)
    lockBinding("call_llm_broadcast", ns)
  }, add = TRUE)
  force(expr)
}

mk_res <- function(n, text = paste0("A", seq_len(n))) {
  tibble::tibble(
    message_index = seq_len(n), provider = "mock", model = "mock",
    response_text = text, raw_response_json = NA_character_,
    success = TRUE, error_message = NA_character_, finish_reason = "stop",
    sent_tokens = 10L, rec_tokens = 5L, total_tokens = 15L,
    reasoning_tokens = NA_integer_, cached_tokens = 4L,
    response_id = "r", duration = 0.1, status_code = NA_integer_,
    error_code = NA_character_, bad_param = NA_character_,
    response = replicate(n, NULL, simplify = FALSE)
  )
}

# ---- tools ---------------------------------------------------------------------

test_that("llm_tool builds a schema and provider shapes convert", {
  t1 <- llm_tool(function(city) city, "get_weather", "Weather for a city.",
                 parameters = list(city = list(type = "string")))
  expect_s3_class(t1, "llmr_tool")
  oa <- LLMR:::.llmr_tools_openai(list(t1))
  expect_identical(oa[[1]]$`function`$name, "get_weather")
  expect_identical(oa[[1]]$`function`$parameters$required, list("city"))
  an <- LLMR:::.llmr_tools_anthropic(list(t1))
  expect_identical(an[[1]]$input_schema$type, "object")
})

test_that("tool_calls extracts openai, anthropic, and gemini shapes", {
  mk_resp <- function(raw) structure(list(raw = raw, text = NA), class = "llmr_response")
  oa <- mk_resp(list(choices = list(list(message = list(tool_calls = list(
    list(id = "c1", type = "function",
         `function` = list(name = "f", arguments = '{"x": 2}'))))))))
  out <- tool_calls(oa)
  expect_identical(out[[1]]$name, "f")
  expect_equal(out[[1]]$arguments$x, 2)

  an <- mk_resp(list(content = list(
    list(type = "text", text = "let me check"),
    list(type = "tool_use", id = "t1", name = "g", input = list(y = "z")))))
  out2 <- tool_calls(an)
  expect_identical(out2[[1]]$id, "t1")
  expect_identical(out2[[1]]$arguments$y, "z")

  ge <- mk_resp(list(candidates = list(list(content = list(parts = list(
    list(functionCall = list(name = "h", args = list(k = 1)))))))))
  out3 <- tool_calls(ge)
  expect_identical(out3[[1]]$name, "h")
})

test_that("tool execution survives a crashing tool", {
  t1 <- llm_tool(function() stop("boom"), "bad", "Always fails.")
  expect_match(LLMR:::.llmr_run_tool(t1, list()), "^ERROR: boom")
})

# ---- agreement -------------------------------------------------------------------

test_that("llm_agreement reports perfect and imperfect agreement", {
  df <- tibble::tibble(lab_1 = c("a", "b", "c"),
                       lab_2 = c("a", "b", "c"),
                       lab_3 = c("a", "b", "c"))
  ag <- llm_agreement(df, prefix = "lab")
  expect_equal(ag$summary$krippendorff_alpha, 1)
  expect_equal(ag$summary$mean_pairwise_agreement, 1)
  expect_true(all(ag$by_row$unanimous))

  df2 <- tibble::tibble(lab_1 = c("a", "a"), lab_2 = c("b", "a"))
  ag2 <- llm_agreement(df2, prefix = "lab")
  expect_lt(ag2$summary$krippendorff_alpha, 1)
  expect_true(ag2$by_row$tie[1])
  expect_identical(ag2$by_row$majority[2], "a")
})

test_that("llm_agreement normalizes case and whitespace by default", {
  df <- tibble::tibble(x_1 = c("Positive "), x_2 = c("positive"))
  ag <- llm_agreement(df, prefix = "x")
  expect_equal(ag$summary$mean_pairwise_agreement, 1)
})

test_that("alpha handles missing replicates", {
  df <- tibble::tibble(x_1 = c("a", "a", NA), x_2 = c("a", "b", "c"),
                       x_3 = c("a", NA, "c"))
  ag <- llm_agreement(df, prefix = "x")
  expect_true(is.finite(ag$summary$krippendorff_alpha))
  expect_equal(ag$by_row$n_missing, c(0L, 1L, 1L))
})

# ---- usage upgrades ---------------------------------------------------------------

test_that("llm_usage reports cached tokens and a price estimate", {
  res <- tibble::tibble(
    model = c("m1", "m1"),
    success = c(TRUE, TRUE), finish_reason = c("stop", "stop"),
    sent_tokens = c(100L, 100L), rec_tokens = c(50L, 50L),
    total_tokens = c(150L, 150L), reasoning_tokens = NA_integer_,
    cached_tokens = c(80L, NA), duration = c(0.4, 0.5)
  )
  u <- llm_usage(res)
  expect_equal(u$cached_tokens, 80L)

  prices <- data.frame(model = "m1", input = 1, output = 2, cached = 0.1)
  u2 <- llm_usage(res, price_table = prices)
  # fresh input: (100-80) + 100 = 120; cached: 80; output: 100
  expect_equal(u2$cost_estimate, (120 * 1 + 80 * 0.1 + 100 * 2) / 1e6)
})

test_that("llm_methods_text drafts a faithful paragraph", {
  res <- tibble::tibble(
    model = "openai/gpt-oss-20b", provider = "groq", temperature = 0,
    success = c(TRUE, FALSE), finish_reason = c("stop", "error:server"),
    sent_tokens = c(10L, NA), rec_tokens = c(5L, NA),
    total_tokens = c(15L, NA), reasoning_tokens = NA_integer_,
    duration = c(0.4, 0.1)
  )
  txt <- llm_methods_text(res, task = "to classify items")
  expect_match(txt, "gpt-oss-20b")
  expect_match(txt, "groq")
  expect_match(txt, "to classify items")
  expect_match(txt, "temperature = 0")
  expect_match(txt, "1 call\\(s\\) failed")
})

# ---- audit log -------------------------------------------------------------------

test_that("the audit log writes one JSON record per event", {
  log_file <- tempfile(fileext = ".jsonl")
  on.exit({ llm_log_disable(); unlink(log_file) }, add = TRUE)
  llm_log_enable(log_file)

  resp <- LLMR:::new_llmr_response(
    text = "hello", provider = "groq", model = "m",
    finish_reason = "stop",
    usage = list(sent = 3L, rec = 2L, total = 5L,
                 reasoning = NA_integer_, cached = NA_integer_),
    response_id = "id-1", duration_s = 0.2,
    model_version = "m-v2"
  )
  LLMR:::.llmr_log_event(kind = "call", provider = "groq", model = "m",
                         status = 200L, response = resp)
  lines <- readLines(log_file)
  expect_length(lines, 1L)
  rec <- jsonlite::fromJSON(lines[[1]])
  expect_identical(rec$kind, "call")
  expect_identical(rec$schema_version, "1.0")
  expect_identical(rec$text, "hello")
  expect_identical(rec$model_version, "m-v2")
  expect_equal(rec$usage$total, 5)

  # metadata-only mode drops text
  llm_log_enable(log_file, include_messages = FALSE)
  LLMR:::.llmr_log_event(kind = "call", provider = "groq", model = "m",
                         status = 200L, response = resp)
  rec2 <- jsonlite::fromJSON(readLines(log_file)[[2]])
  expect_null(rec2$text)
})

test_that("log scrubbing replaces long inline payloads", {
  long_b64 <- paste0("data:image/png;base64,", strrep("QUJD", 2000))
  out <- LLMR:::.llmr_log_scrub(list(url = long_b64, keep = "short"))
  expect_match(out$url, "inline data omitted")
  expect_identical(out$keep, "short")
})

# ---- tidy layer: NA policy, collisions, outcome note --------------------------------

test_that(".na_action = 'skip' avoids calls for NA rows and labels them", {
  seen <- NULL
  stub <- function(config, messages, ...) {
    seen <<- length(messages)
    mk_res(length(messages))
  }
  df <- tibble::tibble(x = c("one", NA, "three"))
  cfg <- llm_config("openai", "m")
  out <- suppressMessages(with_stub_broadcast2(stub,
    llm_mutate(df, ans, prompt = "Q: {x}", .config = cfg, .na_action = "skip")))
  expect_equal(seen, 2L)             # only the two non-NA rows were sent
  expect_true(is.na(out$ans[2]))
  expect_identical(out$ans_finish[2], "skipped")
  expect_identical(out$ans[c(1, 3)], c("A1", "A2"))
})

test_that(".na_action = 'error' stops before any call", {
  df <- tibble::tibble(x = c("one", NA))
  cfg <- llm_config("openai", "m")
  expect_error(
    llm_mutate(df, ans, prompt = "Q: {x}", .config = cfg, .na_action = "error"),
    "NA in their prompt template"
  )
})

test_that("llm_mutate replaces an existing output column with a notice", {
  stub <- function(config, messages, ...) mk_res(length(messages))
  df <- tibble::tibble(x = c("a", "b"), ans = c("old1", "old2"))
  cfg <- llm_config("openai", "m")
  out <- with_stub_broadcast2(stub, suppressMessages(
    llm_mutate(df, ans, prompt = "{x}", .config = cfg)))
  expect_identical(out$ans, c("A1", "A2"))
  expect_false(any(grepl("^ans\\.\\.\\.", names(out))))  # no bind_cols mangling
})

test_that("a failure summary note is emitted in text mode", {
  stub <- function(config, messages, ...) {
    r <- mk_res(length(messages))
    r$success[1] <- FALSE
    r$finish_reason[1] <- "error:server"
    r
  }
  df <- tibble::tibble(x = c("a", "b"))
  cfg <- llm_config("openai", "m")
  expect_message(
    with_stub_broadcast2(stub,
      llm_mutate(df, ans, prompt = "{x}", .config = cfg, .return = "text")),
    "1 call failed"
  )
})

test_that("cached tokens flow into llm_mutate diagnostic columns", {
  stub <- function(config, messages, ...) mk_res(length(messages))
  df <- tibble::tibble(x = "a")
  cfg <- llm_config("openai", "m")
  out <- with_stub_broadcast2(stub,
    llm_mutate(df, ans, prompt = "{x}", .config = cfg))
  expect_equal(out$ans_cached, 4L)
})

# ---- preview additions ----------------------------------------------------------

test_that("preview flags NA templates and empty prompts", {
  df <- tibble::tibble(x = c("ok", NA))
  pv <- llm_preview(df, prompt = "{x}")
  expect_true(pv$ok[1])
  expect_false(pv$ok[2])
  expect_match(paste(pv$issues[[2]], collapse = " "), "NA values|empty")
})

test_that("preview flags row-like tags under batching", {
  df <- tibble::tibble(x = "a")
  pv <- llm_preview(df, prompt = "{x}", .tags = c("row_1"), .batch_size = 2)
  expect_match(paste(pv$issues[[1]], collapse = " "), "row_N|<row_N>")
})

# ---- batch API plumbing (offline) -------------------------------------------------

test_that("batch jobs round-trip through RDS without secrets", {
  cfg <- llm_config("groq", "openai/gpt-oss-20b")
  job <- LLMR:::.llmr_batch_job("groq", cfg, "batch_123",
                                c("llmr-000001", "llmr-000002"))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  LLMR:::.llmr_batch_save(job, path)
  job2 <- LLMR:::.llmr_batch_load(path)
  expect_identical(job2$batch_id, "batch_123")
  expect_identical(job2$n, 2L)
  expect_true(inherits(job2$config$api_key, "llmr_secret_env"))
})

test_that("batch results synthesize call_llm_par-style rows", {
  content <- list(
    id = "resp-1",
    choices = list(list(message = list(content = "four"), finish_reason = "stop")),
    usage = list(prompt_tokens = 8, completion_tokens = 2, total_tokens = 10)
  )
  row <- LLMR:::.batch_row_from_content(content)
  expect_identical(row$response_text, "four")
  expect_identical(row$finish_reason, "stop")
  expect_equal(row$sent_tokens, 8L)
  expect_equal(row$total_tokens, 10L)
})

# ---- logprobs --------------------------------------------------------------------

test_that("llm_logprobs parses openai and gemini shapes", {
  mk_resp <- function(raw) structure(list(raw = raw), class = "llmr_response")
  oa <- mk_resp(list(choices = list(list(logprobs = list(content = list(
    list(token = "four", logprob = -0.01,
         top_logprobs = list(list(token = "four", logprob = -0.01),
                             list(token = "4", logprob = -4.2)))))))))
  out <- llm_logprobs(oa)
  expect_equal(nrow(out), 1L)
  expect_identical(out$token, "four")
  expect_equal(nrow(out$top_logprobs[[1]]), 2L)

  ge <- mk_resp(list(candidates = list(list(logprobsResult = list(
    chosenCandidates = list(list(token = "x", logProbability = -0.5)),
    topCandidates = list(list(candidates = list(
      list(token = "x", logProbability = -0.5)))))))))
  out2 <- llm_logprobs(ge)
  expect_equal(out2$logprob, -0.5)
})

# ---- chat session fixes -----------------------------------------------------------

test_that("NA-safe accumulation helper treats NA as zero", {
  expect_equal(LLMR:::.add0(NA_integer_), 0L)
  expect_equal(LLMR:::.add0(NULL), 0L)
  expect_equal(LLMR:::.add0(7L), 7L)
})

test_that("chat content strings render multimodal parts readably", {
  parts <- list(list(type = "text", text = "look at this"),
                list(type = "file", path = "img.png"))
  expect_identical(LLMR:::.chat_content_string(parts),
                   "look at this [file: img.png]")
})

# ---- streaming helpers -------------------------------------------------------------

test_that("stream finish reasons reuse the standard vocabulary", {
  expect_identical(LLMR:::.map_stream_finish("stop"), "stop")
  expect_identical(LLMR:::.map_stream_finish("length"), "length")
  expect_identical(LLMR:::.map_stream_finish(NULL), "stop")
})

test_that("stream specs cover every compat provider", {
  for (p in c("openai", "groq", "together", "deepseek", "xiaomi", "alibaba",
              "zhipu", "moonshot", "xai", "ollama")) {
    spec <- LLMR:::.compat_stream_spec(p)
    expect_true(nzchar(spec$endpoint))
    expect_true(spec$auth %in% c("bearer", "api-key", "none"))
  }
})
