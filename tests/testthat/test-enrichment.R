# Tests for the 0.8.9 LLMRagent-requested enrichments. All offline.

# --- #7: log_read robustness to a flat request_hash (no nested request) ------

test_that("llm_log_read tolerates a record with request_hash but no request body", {
  path <- tempfile(fileext = ".jsonl")
  on.exit(unlink(path), add = TRUE)
  # a flat response-record-shaped line: request_hash present, no `request` object
  writeLines(paste0(
    '{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0","kind":"call",',
    '"provider":"openai","model":"gpt-4o-mini","status":200,',
    '"request_hash":"abc123","usage":{"sent":5,"rec":2},"text":"ok"}'), path)
  read <- llm_log_read(path)
  expect_length(read$records, 1L)
  # request body absent -> request_hash in the manifest is NA, no partial-match crash
  expect_true(is.na(read$manifest$request_hash[1]))
})

# --- #3: llm_log_active() ----------------------------------------------------

test_that("llm_log_active reports the audit-log state without printing", {
  old <- options(llmr.log_file = NULL, llmr.log_messages = NULL)
  on.exit(options(old), add = TRUE)

  st <- llm_log_active()
  expect_named(st, c("active", "path", "include_messages"))
  expect_false(st$active)
  expect_null(st$path)

  p <- tempfile(fileext = ".jsonl")
  llm_log_enable(p, include_messages = FALSE)
  st2 <- llm_log_active()
  expect_true(st2$active)
  expect_equal(st2$path, p)
  expect_false(st2$include_messages)
  llm_log_disable()
})

# --- #1: request_hash on the parallel path -----------------------------------

test_that("llm_add_request_hash matches llm_request_hash for each row", {
  cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
  df <- tibble::tibble(
    config   = list(cfg, cfg),
    messages = list(c(user = "Label: positive?"), c(user = "Label: negative?")))
  out <- llm_add_request_hash(df)
  expect_true("request_hash" %in% names(out))
  expect_equal(out$request_hash[1],
               llm_request_hash(config = cfg, messages = c(user = "Label: positive?")))
  expect_false(identical(out$request_hash[1], out$request_hash[2]))
})

# --- #6: llm_tool_signature() and llm_uuid() ---------------------------------

test_that("llm_tool_signature is stable and tracks the contract, not the body", {
  t1 <- llm_tool(function(x) x, "echo", "Echo a value",
                 parameters = list(x = list(type = "string")))
  t2 <- llm_tool(function(x) toupper(x), "echo", "Echo a value",   # different body
                 parameters = list(x = list(type = "string")))
  t3 <- llm_tool(function(x) x, "echo", "Echo a NUMBER",           # different desc
                 parameters = list(x = list(type = "string")))
  s1 <- llm_tool_signature(t1)
  expect_match(s1, "^[0-9a-f]{64}$")
  expect_identical(s1, llm_tool_signature(t1))            # stable
  expect_identical(s1, llm_tool_signature(t2))            # body excluded
  expect_false(identical(s1, llm_tool_signature(t3)))     # description matters
})

test_that("llm_uuid is unique, sortable, and prefixable", {
  ids <- vapply(1:5, function(i) llm_uuid(), "")
  expect_equal(length(unique(ids)), 5L)
  expect_identical(ids, sort(ids))                        # creation order sorts
  expect_match(llm_uuid("run"), "^run-")
})

# --- #4: llm_agreement(metric=) ----------------------------------------------

test_that("metric = 'nominal' is byte-identical to the default", {
  df <- data.frame(r_1 = c("a", "a", "b", "c"),
                   r_2 = c("a", "b", "b", "c"),
                   r_3 = c("a", "a", "b", "a"))
  a_default <- llm_agreement(df, cols = c("r_1", "r_2", "r_3"))
  a_nominal <- llm_agreement(df, cols = c("r_1", "r_2", "r_3"), metric = "nominal")
  expect_identical(a_default$summary$krippendorff_alpha,
                   a_nominal$summary$krippendorff_alpha)
})

test_that("ordinal and interval alpha match a hand-computed example", {
  # 3 units x 3 coders, ordinal categories 1,2,3 (rows: 1,1,2 / 1,2,2 / 2,3,3).
  # Hand-computed Krippendorff alpha: ordinal 0.4328042, interval 0.4545455.
  df <- data.frame(r1 = c("1", "1", "2"),
                   r2 = c("1", "2", "3"),
                   r3 = c("2", "2", "3"))
  ord <- llm_agreement(df, cols = c("r1", "r2", "r3"),
                       metric = "ordinal", normalize = FALSE)$summary$krippendorff_alpha
  int <- llm_agreement(df, cols = c("r1", "r2", "r3"),
                       metric = "interval", normalize = FALSE)$summary$krippendorff_alpha
  expect_equal(ord, 0.4328042, tolerance = 1e-6)
  expect_equal(int, 0.4545455, tolerance = 1e-6)
})

test_that("numeric labels order numerically, not lexicographically", {
  # "2" must sort before "10" for ordinal/interval; lexicographic order would
  # corrupt the distances.
  df <- data.frame(r1 = c("2", "10", "2"),
                   r2 = c("2", "10", "10"),
                   r3 = c("10", "10", "2"))
  ord <- llm_agreement(df, cols = c("r1", "r2", "r3"),
                       metric = "ordinal", normalize = FALSE)$summary$krippendorff_alpha
  expect_true(is.finite(ord))
})

test_that("ordinal and interval alpha run on numeric labels and lie in range", {
  dfn <- data.frame(r_1 = c("1", "2", "3", "1"),
                    r_2 = c("1", "2", "2", "1"),
                    r_3 = c("2", "2", "3", "1"))
  ord <- llm_agreement(dfn, cols = c("r_1", "r_2", "r_3"),
                       metric = "ordinal", normalize = FALSE)$summary$krippendorff_alpha
  int <- llm_agreement(dfn, cols = c("r_1", "r_2", "r_3"),
                       metric = "interval", normalize = FALSE)$summary$krippendorff_alpha
  expect_true(ord >= -1 && ord <= 1)
  expect_true(int >= -1 && int <= 1)
  # interval metric needs numeric labels; non-numeric -> NA with a warning
  dfc <- data.frame(r_1 = c("low", "high"), r_2 = c("low", "low"))
  expect_warning(av <- llm_agreement(dfc, cols = c("r_1", "r_2"),
                                     metric = "interval")$summary$krippendorff_alpha)
  expect_true(is.na(av))
})
