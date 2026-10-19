library(testthat)
library(LLMR)

# A real llama.cpp server with the smallest registry model. Runs only when
# LLMR_LOCAL_TESTS=true; it downloads about 160 MB into llm_local_dir() on
# first use (set LLMR_LOCAL_DIR to choose the place) and stops its server.

skip_unless_local <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("LLMR_LOCAL_TESTS"), "true"),
              "set LLMR_LOCAL_TESTS=true to run the local-model tests")
}

test_that("a local model installs, serves, and answers token-level questions", {
  skip_unless_local()
  llm_local_install("smollm2-135m", accept_license = TRUE, quiet = TRUE)
  expect_true(llm_local_ready("smollm2-135m"))
  llm_local_start("smollm2-135m", threads = 2, device = "cpu")
  on.exit(llm_local_stop(), add = TRUE)
  cfg <- llm_config("llamacpp", "smollm2-135m", temperature = 0)

  nt <- llm_next_token(cfg, prompt = "The capital of France is", top_n = 10)
  expect_identical(nrow(nt), 10L)
  expect_true(all(diff(nt$logprob) <= 0))
  fv <- llm_next_token(cfg, prompt = "The capital of France is", full = TRUE)
  expect_equal(sum(fv$prob), 1, tolerance = 1e-3)

  # scoring by token ids agrees with the distribution read from the text
  s <- llm_score(cfg, " Paris", prompt = "The capital of France is")
  # (single-precision values; they agree to about 3e-4)
  expect_equal(s$logprob[1], fv$logprob[fv$id == s$id[1]], tolerance = 1e-3)

  a <- llm_continue(cfg, prompt = "Once upon a time", n = 3, max_tokens = 8, seed = 110, temperature = 1)
  b <- llm_continue(cfg, prompt = "Once upon a time", n = 3, max_tokens = 8, seed = 110, temperature = 1)
  expect_identical(a$continuation, b$continuation)
  expect_gt(length(unique(a$continuation)), 1L)

  # Persian text, whose tokens can be parts of a character, scores in full
  p <- llm_score(cfg, " \u0628\u0648\u062f\u062c\u0647\u0654 \u0645\u0644\u06cc", prompt = "\u062f\u0648\u0644\u062a")
  expect_true(all(is.finite(p$logprob)))

  r <- call_llm(cfg, "Reply with one word: the capital of France?")
  expect_s3_class(r, "llmr_response")
  expect_true(nzchar(as.character(r)))
  tk <- llm_tokenize(cfg, "The capital of France is Paris.")
  expect_identical(llm_detokenize(cfg, tk$id), "The capital of France is Paris.")
})
