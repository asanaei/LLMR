library(testthat)
library(LLMR)

test_that("Voyage is routed to embeddings by provider identity", {
  expect_true(LLMR:::.is_embedding_config(
    llm_config("voyage", "voyage-3.5-lite")))
  expect_true(LLMR:::.is_embedding_config(
    llm_config("voyage", "voyage-3.5-lite", embedding = FALSE)))
})

test_that("batched embeddings propagate permanent and transport errors", {
  withr::local_envvar(c(VOYAGE_API_KEY = "test-key-not-real"))
  cfg <- llm_config("voyage", "voyage-3.5-lite")
  kind <- "auth"
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) {
      if (kind == "auth") {
        LLMR:::.llmr_error("Unauthorized", category = "auth", status_code = 401L)
      }
      stop("connection reset")
    },
    .package = "LLMR"
  )

  expect_error(get_batched_embeddings("x", cfg, tries = 1),
               class = "llmr_api_auth_error")
  kind <- "transport"
  expect_error(get_batched_embeddings("x", cfg, tries = 1), "connection reset")
})

test_that("batched embeddings error when every transient batch fails", {
  withr::local_envvar(c(VOYAGE_API_KEY = "test-key-not-real"))
  cfg <- llm_config("voyage", "voyage-3.5-lite")
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) {
      LLMR:::.llmr_error("Service unavailable", category = "server",
                         status_code = 503L)
    },
    .package = "LLMR"
  )
  expect_error(get_batched_embeddings(c("a", "b"), cfg, batch_size = 1,
                                      tries = 1),
               class = "llmr_api_server_error")
})

test_that("batched embeddings use NA only for a genuine partial outcome", {
  withr::local_envvar(c(VOYAGE_API_KEY = "test-key-not-real"))
  cfg <- llm_config("voyage", "voyage-3.5-lite")
  calls <- 0L
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(list(data = list(list(embedding = c(1, 2, 3)))))
      }
      LLMR:::.llmr_error("Service unavailable", category = "server",
                         status_code = 503L)
    },
    .package = "LLMR"
  )
  out <- get_batched_embeddings(c(first = "a", second = "b"), cfg,
                                batch_size = 1, tries = 1)
  expect_identical(dim(out), c(2L, 3L))
  expect_equal(unname(out[1, ]), c(1, 2, 3))
  expect_true(all(is.na(out[2, ])))
  expect_identical(rownames(out), c("first", "second"))
})

test_that("Anthropic rejects an impossible thinking budget before a call", {
  withr::local_envvar(c(ANTHROPIC_API_KEY = "test-key-not-real"))
  called <- FALSE
  testthat::local_mocked_bindings(
    perform_request = function(...) {
      called <<- TRUE
      stop("request should not be performed")
    },
    .package = "LLMR"
  )
  cfg <- llm_config("anthropic", "claude-x", max_tokens = 100,
                    thinking_budget = 100)
  expect_error(call_llm(cfg, "hi"),
               "max_tokens must exceed thinking_budget")
  expect_false(called)
})

test_that("structured output exposes only implemented controls", {
  expect_false("method" %in% names(formals(enable_structured_output)))
})

test_that("audit logging refuses an unwritable destination at enable time", {
  withr::local_options(list(llmr.log_file = NULL, llmr.log_messages = NULL))
  parent <- tempfile("missing-log-dir-")
  path <- file.path(parent, "calls.jsonl")
  expect_error(llm_log_enable(path), "Cannot open audit log for appending")
  expect_false(llm_log_active()$active)
  expect_false(dir.exists(parent))
})
