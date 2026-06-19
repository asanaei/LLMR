library(testthat)
library(LLMR)

# All tests here are OFFLINE: they drive .run_batched() through its `.broadcast`
# seam with scripted responses, so the real partition, scanner, recovery, and
# reassembly run without any network access.

# ---- mock helpers ---------------------------------------------------------

mk_res <- function(text, success = TRUE, finish = "stop",
                   error_message = NA_character_, error_code = NA_character_,
                   status_code = NA_integer_) {
  tibble::tibble(
    message_index = 1L, provider = "mock", model = "mock",
    response_text = text, raw_response_json = NA_character_,
    success = success,
    error_message = if (success) NA_character_ else error_message,
    finish_reason = finish,
    sent_tokens = 10L, rec_tokens = 20L, total_tokens = 30L,
    reasoning_tokens = NA_integer_, response_id = "rid", duration = 0.1,
    status_code = status_code, error_code = error_code, bad_param = NA_character_,
    response = list(NULL)
  )
}
.get_user <- function(msg) if (is.null(names(msg))) msg[[1]] else msg[["user"]]
.local_ids <- function(usr) {
  h <- regmatches(usr, gregexpr("<row_([0-9]+)>", usr, perl = TRUE))[[1]]
  sort(unique(as.integer(sub("<row_([0-9]+)>", "\\1", h))))
}
.global_ids <- function(usr) {
  n <- regmatches(usr, gregexpr("person ([0-9]+)", usr))[[1]]
  as.integer(sub("person ", "", n))
}
CFG <- llm_config("openai", "x")
TXT <- paste0("person ", 1:5)

# perfect tag echo: maps each local id to its global identity (read from content)
echo_tags <- function(config, messages, ...) {
  usr <- .get_user(messages[[1]])
  if (grepl("<row_", usr)) {
    locs <- .local_ids(usr); globs <- .global_ids(usr)
    mk_res(paste0("<row_", locs, "><age>", 20 + globs, "</age><job>j",
                  globs, "</job></row_", locs, ">", collapse = "\n"))
  } else {
    g <- .global_ids(usr)[1]
    mk_res(paste0("<age>", 20 + g, "</age><job>j", g, "</job>"))
  }
}

# ---- partition determinism ------------------------------------------------

test_that(".batch_partition is a deterministic pure function of (n,k)", {
  P <- LLMR:::.batch_partition
  expect_identical(P(7, 3), list(1:3, 4:6, 7L))
  expect_identical(P(7, Inf), list(1:7))
  expect_identical(P(5, 1), as.list(1:5))
  expect_identical(P(0, 3), list())
  expect_identical(P(7, 3), P(7, 3))  # repeatable
})

# ---- escape / payload safety ----------------------------------------------

test_that("xml escape blocks boundary forging but keeps glue braces literal", {
  e <- LLMR:::.xml_escape("regex </row_2> and {1+1} & <job>")
  expect_false(grepl("<row", e))
  expect_true(grepl("{1+1}", e, fixed = TRUE))       # no second glue pass
  expect_identical(LLMR:::.decode_tag_entities(e), "regex </row_2> and {1+1} & <job>")
})

test_that(".assemble_payload wraps each row with within-batch indices", {
  pl <- LLMR:::.assemble_payload(c("a", "b"))
  expect_match(pl, "<row_1>\na\n</row_1>", fixed = TRUE)
  expect_match(pl, "<row_2>\nb\n</row_2>", fixed = TRUE)
})

# ---- adversarial scanner corpus -------------------------------------------

test_that(".batch_split_rows handles the adversarial corpus", {
  S <- LLMR:::.batch_split_rows
  # happy
  expect_identical(S("<row_1>a</row_1><row_2>b</row_2>", 2)$report$found, c(1L, 2L))
  # reordered + dropped
  r <- S("<row_3>c</row_3><row_1>a</row_1>", 3)
  expect_identical(r$report$found, c(1L, 3L))
  expect_identical(trimws(r$blocks[[1]]), "a")
  expect_true(is.null(r$blocks[[2]]) || is.na(r$blocks[[2]]))
  # hallucinated id beyond m -> ignored
  expect_identical(S("<row_1>a</row_1><row_9>z</row_9>", 2)$report$found, 1L)
  # truncated tail
  expect_identical(S("<row_1>a</row_1><row_2>bbbb", 2)$report$found, 1L)
  # accidental nesting -> outer demoted, nested NOT committed as sibling
  r <- S("<row_1>x<row_2>y</row_2></row_1>", 2)
  expect_true(r$report$nesting)
  expect_false(1L %in% r$report$found)
  # leading zero / caps / no underscore
  expect_identical(S("<row_01>a</row_01><Row_2>b</Row_2><row3>c</row3>", 3)$report$found,
                   c(1L, 2L, 3L))
  # duplicate: first non-empty wins
  expect_identical(trimws(S("<row_1>first</row_1><row_1>second</row_1>", 1)$blocks[[1]]),
                   "first")
})

test_that("llm_parse_rowpack_tags equals per-row flat parse", {
  txt <- "<row_1><age>21</age><job>barista</job></row_1>\n<row_2><age>34</age><job>welder</job></row_2>"
  pp <- llm_parse_rowpack_tags(txt, c("age", "job"), 2)
  f1 <- LLMR:::llm_parse_tags("<age>21</age><job>barista</job>", c("age", "job"))
  expect_identical(pp[[1]], f1)
  expect_identical(pp[[2]]$age, "34")
})

# ---- reassembly invariants ------------------------------------------------

test_that("engine returns n rows, original order, correct global remap", {
  for (k in c(2, 3, 5, Inf)) {
    res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags",
      tags = c("age", "job"), rows_per_prompt = k, rowpack_payload = "user",
      rowpack_recovery = "halve_recursive", .broadcast = echo_tags)
    expect_identical(nrow(res), 5L)
    p <- LLMR:::llm_parse_tags_col(res, tags = c("age", "job"),
                                   tags_col = "response_text")
    expect_identical(as.numeric(p$age), c(21, 22, 23, 24, 25))
    expect_true(all(p$tags_ok))
  }
})

test_that("token totals attributed once per batch (no k-times overcount)", {
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags",
    tags = c("age", "job"), rows_per_prompt = 2, rowpack_payload = "user",
    rowpack_recovery = "halve_recursive", .broadcast = echo_tags)
  per_batch <- tapply(res$total_tokens, res$rowpack_id,
                      function(x) sum(!is.na(x)))
  expect_true(all(per_batch == 1L))
})

test_that("result column set matches call_llm_par plus batch columns", {
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags",
    tags = c("age", "job"), rows_per_prompt = 2, rowpack_payload = "user",
    rowpack_recovery = "halve_recursive", .broadcast = echo_tags)
  core <- c("response_text", "raw_response_json", "success", "error_message",
            "finish_reason", "sent_tokens", "rec_tokens", "total_tokens",
            "reasoning_tokens", "response_id", "duration", "status_code",
            "error_code", "bad_param", "response")
  expect_true(all(core %in% names(res)))
  expect_true(all(c("rowpack_id", "rows_per_prompt", "rowpack_row") %in% names(res)))
})

# ---- recovery -------------------------------------------------------------

test_that("partial batch recovers the dropped rows", {
  drop_local2 <- function(config, messages, ...) {
    usr <- .get_user(messages[[1]])
    if (grepl("<row_", usr)) {
      locs <- .local_ids(usr); globs <- .global_ids(usr)
      keep <- if (length(locs) > 1L) seq_along(locs)[-2] else seq_along(locs)
      mk_res(paste0("<row_", locs[keep], "><age>", 20 + globs[keep],
                    "</age><job>j", globs[keep], "</job></row_", locs[keep], ">",
                    collapse = "\n"))
    } else {
      g <- .global_ids(usr)[1]
      mk_res(paste0("<age>", 20 + g, "</age><job>j", g, "</job>"))
    }
  }
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags",
    tags = c("age", "job"), rows_per_prompt = 2, rowpack_payload = "user",
    rowpack_recovery = "halve_recursive", .broadcast = drop_local2)
  expect_true(all(res$success))
  expect_true(any(grepl("\\.", res$rowpack_id)))  # split lineage present
})

test_that("recovery = none leaves dropped rows failed", {
  drop_local2 <- function(config, messages, ...) {
    usr <- .get_user(messages[[1]]); locs <- .local_ids(usr); globs <- .global_ids(usr)
    keep <- if (length(locs) > 1L) seq_along(locs)[-2] else seq_along(locs)
    mk_res(paste0("<row_", locs[keep], "><age>", 20 + globs[keep],
                  "</age><job>j", globs[keep], "</job></row_", locs[keep], ">",
                  collapse = "\n"))
  }
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags",
    tags = c("age", "job"), rows_per_prompt = 5, rowpack_payload = "user",
    rowpack_recovery = "none", .broadcast = drop_local2)
  expect_equal(sum(!res$success), 1L)
  expect_match(res$finish_reason[!res$success], "^error:")
})

test_that("whole-batch transport failure recovers via split", {
  fail_big <- function(config, messages, ...) {
    usr <- .get_user(messages[[1]])
    if (grepl("<row_", usr))
      return(mk_res(NA_character_, success = FALSE, finish = "error:server",
                    error_message = "boom"))
    g <- .global_ids(usr)[1]
    mk_res(paste0("<age>", 20 + g, "</age><job>j", g, "</job>"))
  }
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags",
    tags = c("age", "job"), rows_per_prompt = 5, rowpack_payload = "user",
    rowpack_recovery = "halve_recursive", .broadcast = fail_big)
  expect_true(all(res$success))
})

test_that("recovered tags singleton carries the field instruction, unwrapped (B4)", {
  # Force a cascade to singletons (multi-row call fails), capture every call's
  # messages, and assert the singleton ASKS for field tags (system-side) while
  # its USER payload stays unwrapped (no <row_>). Pre-fix the singleton carried
  # no field instruction at all.
  seen <- new.env(); seen$msgs <- list()
  spy <- function(config, messages, ...) {
    seen$msgs[[length(seen$msgs) + 1L]] <- messages[[1]]
    usr <- .get_user(messages[[1]])
    if (grepl("<row_", usr))
      return(mk_res(NA_character_, success = FALSE, finish = "error:server",
                    error_message = "boom"))
    g <- .global_ids(usr)[1]
    mk_res(paste0("<age>", 20 + g, "</age>"))   # bare field content, no <row_1>
  }
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags", tags = c("age"),
    rows_per_prompt = 5, rowpack_payload = "user",
    rowpack_recovery = "halve_recursive", .broadcast = spy)
  expect_true(all(res$success))
  # the singleton calls are those whose user payload has no <row_> wrapper
  singletons <- Filter(function(m) !grepl("<row_", .get_user(m)), seen$msgs)
  expect_gt(length(singletons), 0L)
  sys_of <- function(m) if (!is.null(names(m)) && "system" %in% names(m)) m[["system"]] else ""
  # every singleton's SYSTEM side asks for XML-like field tags...
  expect_true(all(vapply(singletons, function(m) grepl("tag", sys_of(m), ignore.case = TRUE), logical(1))))
  # ...and no singleton USER payload was wrapped in <row_i>
  expect_false(any(vapply(singletons, function(m) grepl("<row_", .get_user(m)), logical(1))))
})

test_that("call budget bounds pathological recursion", {
  spy <- new.env(); spy$calls <- 0L
  always_drop <- function(config, messages, ...) {
    spy$calls <- spy$calls + 1L
    usr <- .get_user(messages[[1]])
    if (grepl("<row_", usr)) {
      locs <- .local_ids(usr); globs <- .global_ids(usr)
      keep <- if (length(locs) > 1L) seq_along(locs)[-length(locs)] else integer(0)
      if (!length(keep)) return(mk_res(""))
      mk_res(paste0("<row_", locs[keep], "><age>", 20 + globs[keep],
                    "</age></row_", locs[keep], ">", collapse = "\n"))
    } else {
      mk_res("")  # singleton also returns nothing -> stays failed
    }
  }
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags", tags = c("age"),
    rows_per_prompt = 5, rowpack_payload = "user", rowpack_recovery = "halve_recursive",
    .broadcast = always_drop)
  expect_identical(nrow(res), 5L)
  expect_lte(spy$calls, 3L * 5L)   # <= 3n budget
})

# ---- truncation -----------------------------------------------------------

test_that("finish_reason length distrusts the last present block", {
  trunc_echo <- function(config, messages, ...) {
    usr <- .get_user(messages[[1]])
    if (grepl("<row_", usr)) {
      locs <- .local_ids(usr); globs <- .global_ids(usr)
      mk_res(paste0("<row_", locs, "><age>", 20 + globs, "</age></row_", locs,
                    ">", collapse = "\n"), finish = "length")
    } else {
      g <- .global_ids(usr)[1]
      mk_res(paste0("<age>", 20 + g, "</age>"))   # singleton clean
    }
  }
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "tags", tags = c("age"),
    rows_per_prompt = 5, rowpack_payload = "user", rowpack_recovery = "halve_recursive",
    .broadcast = trunc_echo)
  # all rows eventually resolve (last block recovered as a singleton)
  expect_true(all(res$success))
})

# ---- structured de-multiplex ----------------------------------------------

test_that("structured batch de-multiplexes by integer row", {
  echo_struct <- function(config, messages, ...) {
    usr <- .get_user(messages[[1]])
    if (grepl("<row_", usr)) {
      locs <- .local_ids(usr); globs <- .global_ids(usr)
      els <- paste0('{"row":', locs, ',"age":', 20 + globs, ',"job":"j',
                    globs, '"}', collapse = ",")
      mk_res(paste0('{"results":[', els, ']}'))
    } else {
      g <- .global_ids(usr)[1]
      mk_res(paste0('{"age":', 20 + g, ',"job":"j', g, '"}'))
    }
  }
  res <- LLMR:::.run_batched(CFG, TXT, NULL, mode = "structured",
    rows_per_prompt = 3, rowpack_payload = "user", rowpack_recovery = "halve_recursive",
    .broadcast = echo_struct)
  expect_identical(nrow(res), 5L)
  expect_true(all(res$success))
  # each response_text is a single JSON object (row key stripped)
  expect_false(any(grepl('"row"', res$response_text)))
  expect_match(res$response_text[1], '"age"')
})

# ---- empty input ----------------------------------------------------------

test_that("n = 0 returns an empty frame without error", {
  res <- LLMR:::.run_batched(CFG, character(0), NULL, mode = "tags",
    tags = c("age"), rows_per_prompt = 3, rowpack_payload = "user",
    rowpack_recovery = "halve_recursive", .broadcast = echo_tags)
  expect_identical(nrow(res), 0L)
})

# ---- argument guards ------------------------------------------------------

test_that(".rows_per_prompt validation rejects bad values", {
  expect_false(LLMR:::.validate_rows_per_prompt(1))
  expect_true(LLMR:::.validate_rows_per_prompt(2))
  expect_true(LLMR:::.validate_rows_per_prompt(Inf))
  expect_error(LLMR:::.validate_rows_per_prompt(0), ">= 1")
  expect_error(LLMR:::.validate_rows_per_prompt(c(1, 2)), ">= 1")
  expect_error(LLMR:::.validate_rows_per_prompt(NA), ">= 1")
  expect_error(LLMR:::.validate_rows_per_prompt("x"), ">= 1")
})

test_that("embedding config + batching is a hard error", {
  ecfg <- llm_config("openai", "text-embedding-3-small")
  expect_error(LLMR:::.assert_batch_not_embedding(ecfg), "embedding")
  expect_error(llm_fn(c("a", "b"), "{x}", .config = ecfg, .rows_per_prompt = 2),
               "embedding")
})

# ---- entry-point no-op + batched paths (stubbed broadcast) ----------------

with_stub_broadcast <- function(stub, expr) {
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

test_that("default .rows_per_prompt = 1 keeps the legacy llm_mutate columns", {
  mk <- function(config, messages, ...) {
    n <- length(messages)
    tibble::tibble(message_index = seq_len(n), provider = "mock", model = "mock",
      response_text = paste0("A", seq_len(n)), raw_response_json = NA_character_,
      success = TRUE, error_message = NA_character_, finish_reason = "stop",
      sent_tokens = 1L, rec_tokens = 2L, total_tokens = 3L,
      reasoning_tokens = NA_integer_, response_id = "r", duration = 0.1,
      status_code = NA_integer_, error_code = NA_character_,
      bad_param = NA_character_, response = replicate(n, NULL, simplify = FALSE))
  }
  df <- tibble::tibble(city = c("a", "b"))
  cfg <- llm_config("openai", "gpt-4.1-nano")
  out <- with_stub_broadcast(mk, llm_mutate(df, ans, prompt = "{city}", .config = cfg))
  expect_identical(setdiff(names(out), names(df)),
    c("ans", "ans_finish", "ans_sent", "ans_rec", "ans_tot", "ans_reason",
      "ans_cached", "ans_ok", "ans_err", "ans_id", "ans_status", "ans_ecode",
      "ans_param", "ans_t"))
  expect_false(any(grepl("_rowpack|_rpn|_rpi", names(out))))
})

test_that("batched llm_mutate adds batch columns and resolves rows", {
  gu <- function(m) if (is.null(names(m))) m[[1]] else m[["user"]]
  lo <- function(u) { h <- regmatches(u, gregexpr("<row_([0-9]+)>", u, perl = TRUE))[[1]]
    sort(unique(as.integer(sub("<row_([0-9]+)>", "\\1", h)))) }
  gi <- function(u) as.integer(sub("C", "", regmatches(u, gregexpr("C([0-9]+)", u))[[1]]))
  stub <- function(config, messages, ...) {
    u <- gu(messages[[1]])
    txt <- if (grepl("<row_", u)) {
      L <- lo(u); G <- gi(u)
      paste0("<row_", L, ">ANS", G, "</row_", L, ">", collapse = "\n")
    } else paste0("ANS", gi(u)[1])
    tibble::tibble(message_index = 1L, provider = "mock", model = "mock",
      response_text = txt, raw_response_json = NA_character_, success = TRUE,
      error_message = NA_character_, finish_reason = "stop", sent_tokens = 9L,
      rec_tokens = 9L, total_tokens = 18L, reasoning_tokens = NA_integer_,
      response_id = "r", duration = 0.1, status_code = NA_integer_,
      error_code = NA_character_, bad_param = NA_character_, response = list(NULL))
  }
  df <- tibble::tibble(city = paste0("C", 1:5))
  cfg <- llm_config("openai", "gpt-4.1-nano")
  out <- with_stub_broadcast(stub,
    llm_mutate(df, ans, prompt = "{city}", .config = cfg, .rows_per_prompt = 2))
  expect_identical(out$ans, paste0("ANS", 1:5))
  expect_true(all(c("ans_rowpack", "ans_rpn", "ans_rpi") %in% names(out)))
  # token totals attributed once per batch
  per_batch <- tapply(out$ans_tot, out$ans_rowpack, function(x) sum(!is.na(x)))
  expect_true(all(per_batch == 1L))
})
