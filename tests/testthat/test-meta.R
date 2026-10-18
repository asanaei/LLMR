library(testthat)
library(LLMR)

# The Meta Model API (Muse models): dispatch, key lookup, Contributor-mode
# model selection, structured output, and streaming, all offline.

capture_meta_request <- function(cfg, messages = "hi") {
  seen <- NULL
  testthat::local_mocked_bindings(
    perform_request = function(req, verbose, provider = NULL, model = NULL, config = NULL) {
      seen <<- req
      structure(list(text = "ok"), class = "llmr_response")
    },
    .package = "LLMR")
  invisible(call_llm(cfg, messages))
  seen
}

test_that("meta dispatches to api.meta.ai with Bearer auth", {
  withr::local_envvar(c(META_API_KEY = "test-key-not-real"))
  cfg <- llm_config("meta", "muse-spark-1.3", temperature = 0,
                    reasoning_effort = "low", top_k = 5)
  req <- suppressMessages(capture_meta_request(cfg))
  expect_identical(req$url, "https://api.meta.ai/v1/chat/completions")
  expect_true("Authorization" %in% names(req$headers))
  expect_identical(req$body$data$model, "muse-spark-1.3")
  # reasoning_effort is forwarded verbatim; top_k is not a Meta parameter
  expect_identical(req$body$data$reasoning_effort, "low")
  expect_null(req$body$data$top_k)

  emb <- llm_config("meta", "muse-embed", embedding = TRUE)
  expect_error(call_llm(emb, "hi"), "not currently supported for Meta")
})

test_that("meta reads META_API_KEY first and falls back to MODEL_API_KEY", {
  expect_identical(LLMR:::.default_api_key_env("meta"),
                   c("META_API_KEY", "META_KEY", "MODEL_API_KEY"))
  # other providers keep the two formulaic names only
  expect_identical(LLMR:::.default_api_key_env("groq"),
                   c("GROQ_API_KEY", "GROQ_KEY"))

  withr::local_envvar(c(META_API_KEY = NA, META_KEY = NA,
                        MODEL_API_KEY = "test-key-not-real"))
  cfg <- llm_config("meta", "muse-spark-1.3")
  expect_identical(
    LLMR:::.resolve_api_key(cfg$api_key, provider = "meta", model = cfg$model),
    "test-key-not-real")

  withr::local_envvar(c(MODEL_API_KEY = NA))
  expect_error(
    LLMR:::.resolve_api_key(cfg$api_key, provider = "meta", model = cfg$model),
    class = "llmr_api_auth_error")
})

test_that("Contributor model ids pass through unchanged", {
  withr::local_options(llmr.quiet = TRUE)
  expect_identical(llm_config("meta", "muse-spark-1.3-contributor")$model,
                   "muse-spark-1.3-contributor")
  expect_identical(llm_config("meta", "muse-spark-1.3")$model, "muse-spark-1.3")
  # positional arguments keep their meaning
  cfg <- llm_config("meta", "muse-spark-1.3", NULL, TRUE)
  expect_true(cfg$troubleshooting)
})

test_that("a Contributor model draws a training-use notice", {
  withr::local_options(rlib_message_verbosity = "verbose", llmr.quiet = NULL)
  expect_message(llm_config("meta", "muse-spark-1.3-contributor"),
                 "train its models")
  expect_no_message(llm_config("meta", "muse-spark-1.3"))
  # the suffix means Contributor mode only on Meta
  expect_no_message(llm_config("openai", "gpt-x-contributor"))
  withr::local_options(llmr.quiet = TRUE)
  expect_no_message(llm_config("meta", "muse-spark-1.3-contributor"))
})

test_that("meta takes server-side json_schema and is a compat provider", {
  cfg <- llm_config("meta", "muse-spark-1.3")
  sc <- enable_structured_output(cfg,
    schema = list(type = "object", properties = list(a = list(type = "string"))))
  expect_identical(sc$model_params$response_format$type, "json_schema")
})

test_that("meta streams against its own host and asks for final usage", {
  spec <- LLMR:::.compat_stream_spec("meta")
  expect_identical(spec$endpoint, "https://api.meta.ai/v1/chat/completions")
  expect_identical(spec$auth, "bearer")
  expect_true(spec$usage_opt)
  expect_true(all(c("top_k", "repetition_penalty") %in% spec$drop))
})

test_that("a streamed Meta reply keeps reasoning and cache-read counts", {
  skip_if(utils::packageVersion("httr2") < "1.2.0",
          "mocking req_perform_connection() needs httr2 >= 1.2.0")
  withr::local_envvar(c(META_API_KEY = "test-key-not-real"))
  sse <- function(...) paste0(paste0("data: ", c(...), "\n\n"), collapse = "")
  body <- sse(
    '{"id":"c1","model":"muse-spark-1.3","choices":[{"index":0,"delta":{"reasoning":"Think."}}]}',
    '{"id":"c1","choices":[{"index":0,"delta":{"content":"Paris."},"finish_reason":"stop"}]}',
    '{"id":"c1","choices":[],"usage":{"prompt_tokens":20,"completion_tokens":5,"total_tokens":25,"cache_read_input_tokens":12,"cache_creation_input_tokens":0}}',
    "[DONE]")
  sent <- NULL
  mock <- function(req) {
    sent <<- req
    httr2::response(status_code = 200,
                    headers = list(`Content-Type` = "text/event-stream"),
                    body = httr2::StreamingBody$new(rawConnection(charToRaw(body))))
  }
  r <- httr2::with_mocked_responses(mock,
    call_llm_stream(llm_config("meta", "muse-spark-1.3"), "hi",
                    callback = function(chunk) invisible(NULL)))
  expect_identical(sent$url, "https://api.meta.ai/v1/chat/completions")
  expect_true(isTRUE(sent$body$data$stream_options$include_usage))
  expect_identical(r$text, "Paris.")
  expect_identical(r$thinking, "Think.")
  expect_identical(tokens(r)$sent, 20L)
  expect_identical(tokens(r)$cached, 12L)
})

test_that("a Meta reply keeps reasoning text and cache-read counts", {
  withr::local_envvar(c(META_API_KEY = "test-key-not-real"))
  payload <- list(
    id = "chatcmpl-1", object = "chat.completion", model = "muse-spark-1.3",
    choices = list(list(index = 0,
      message = list(role = "assistant", content = "Paris.", reasoning = "Think."),
      finish_reason = "stop")),
    usage = list(prompt_tokens = 20, completion_tokens = 5, total_tokens = 25,
                 cache_read_input_tokens = 12, cache_creation_input_tokens = 0))
  mock <- function(req) {
    httr2::response(status_code = 200,
                    headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE)))
  }
  r <- httr2::with_mocked_responses(mock,
    call_llm(llm_config("meta", "muse-spark-1.3"), "hi"))
  expect_identical(as.character(r), "Paris.")
  expect_identical(r$thinking, "Think.")
  expect_identical(finish_reason(r), "stop")
  expect_identical(tokens(r)$cached, 12L)
  expect_identical(tokens(r)$total, 25L)
})
