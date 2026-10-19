library(testthat)
library(LLMR)

# Local models (provider "llamacpp"): registry, installer, provider plumbing,
# and the token-level functions, all offline. A real server is exercised only
# in test-local-live.R, which runs when LLMR_LOCAL_TESTS=true.

hex64 <- "^[0-9a-f]{64}$"

test_that("every registry entry is pinned and licensed", {
  reg <- LLMR:::.llmr_local_registry_list()
  expect_true(length(reg) >= 5)
  for (nm in names(reg)) {
    e <- reg[[nm]]
    expect_match(e$sha256, hex64, info = nm)
    expect_match(e$revision, "^[0-9a-f]{40}$", info = nm)
    expect_true(is.numeric(e$size) && e$size > 1e6 && e$size < 1e9, info = nm)
    expect_true(e$kind %in% c("chat", "embedding"), info = nm)
    expect_true(nzchar(e$license) && nzchar(e$license_url), info = nm)
    expect_match(e$file, "[.]gguf$", info = nm)
    if (e$kind == "embedding") {
      expect_true(is.character(e$prefixes), info = nm)
      if (length(e$prefixes)) expect_true(e$default_task %in% names(e$prefixes), info = nm)
    }
  }
  for (k in names(LLMR:::.llmr_local_runtime$assets)) {
    a <- LLMR:::.llmr_local_runtime$assets[[k]]
    expect_match(a$sha256, hex64, info = k)
    expect_match(a$file, LLMR:::.llmr_local_runtime$build, fixed = TRUE, info = k)
  }
})

test_that("llm_local_models lists the registry with nothing installed", {
  m <- llm_local_models(dir = tempfile("llmr-empty-"))
  expect_s3_class(m, "tbl_df")
  expect_true(all(c("qwen3-0.6b", "bge-m3", "smollm2-135m") %in% m$name))
  expect_false(any(m$installed))
  expect_false(llm_local_ready(dir = tempfile("llmr-empty-")))
})

test_that("the local directory follows the option, then the environment", {
  withr::local_options(llmr.local_dir = NULL)
  withr::local_envvar(LLMR_LOCAL_DIR = "")
  expect_match(llm_local_dir(), "LLMR")
  withr::local_envvar(LLMR_LOCAL_DIR = file.path(tempdir(), "from-env"))
  expect_match(llm_local_dir(), "from-env$")
  withr::local_options(llmr.local_dir = file.path(tempdir(), "from-option"))
  expect_match(llm_local_dir(), "from-option$")
})

test_that("each platform maps to its pinned asset", {
  p <- function(...) LLMR:::.llmr_local_platform(...)$key
  expect_identical(p(sys = "Darwin", machine = "arm64"), "macos-arm64")
  expect_identical(p(sys = "Darwin", machine = "x86_64", rosetta = TRUE), "macos-arm64")
  expect_identical(p(sys = "Darwin", machine = "x86_64", rosetta = FALSE), "macos-x64")
  expect_identical(p(sys = "Linux", machine = "x86_64"), "ubuntu-x64")
  expect_identical(p(sys = "Linux", machine = "aarch64"), "ubuntu-arm64")
  expect_identical(p("vulkan", sys = "Linux", machine = "x86_64"), "ubuntu-vulkan-x64")
  withr::local_envvar(PROCESSOR_ARCHITECTURE = "AMD64")
  expect_identical(p(sys = "Windows", machine = "x86-64"), "win-cpu-x64")
  expect_identical(p("vulkan", sys = "Windows", machine = "x86-64"), "win-vulkan-x64")
  expect_error(p("vulkan", sys = "Darwin", machine = "arm64"), "Linux and Windows")
  expect_error(p(sys = "SunOS", machine = "sparc"), "not available on SunOS")
})

test_that("a download is kept only when its size and sha256 match", {
  src <- tempfile(fileext = ".bin")
  writeBin(as.raw(seq_len(255)), src)
  url <- paste0("file://", normalizePath(src, winslash = "/"))
  good <- digest::digest(file = src, algo = "sha256")
  dest <- file.path(tempfile("dl-"), "x.bin")
  LLMR:::.llmr_local_download(url, dest, good, 255, quiet = TRUE)
  expect_true(file.exists(dest))
  expect_false(file.exists(paste0(dest, ".part")))

  dest2 <- file.path(tempfile("dl-"), "y.bin")
  expect_error(LLMR:::.llmr_local_download(url, dest2, strrep("0", 64), 255, quiet = TRUE),
               "failed its checksum")
  expect_false(file.exists(dest2))
  expect_false(file.exists(paste0(dest2, ".part")))
  expect_error(LLMR:::.llmr_local_download(url, dest2, good, 999, quiet = TRUE), "were expected")
  expect_false(file.exists(paste0(dest2, ".part")))
})

test_that("install asks for agreement and writes nothing without it", {
  skip_on_os("windows")  # the Visual C++ runtime check runs first there
  dir <- tempfile("llmr-local-")
  testthat::local_mocked_bindings(
    .llmr_local_download = function(...) stop("no download expected"),
    .package = "LLMR")
  expect_error(llm_local_install("smollm2-135m", dir = dir), "accept_license = TRUE")
  expect_false(dir.exists(dir) && length(list.files(dir, recursive = TRUE)) > 0)
})

test_that("install makes no request when the files are present", {
  skip_on_os("windows")
  dir <- tempfile("llmr-local-")
  plat <- LLMR:::.llmr_local_platform()
  e <- LLMR:::.llmr_local_registry_list()[["smollm2-135m"]]
  model_rel <- file.path("models", "smollm2-135m", e$file)
  dir.create(file.path(dir, dirname(model_rel)), recursive = TRUE)
  con <- file(file.path(dir, model_rel), "wb"); seek(con, e$size - 1); writeBin(as.raw(0), con); close(con)
  srv_rel <- file.path("runtime", "fake", "llama-server")
  dir.create(file.path(dir, dirname(srv_rel)), recursive = TRUE)
  writeLines("", file.path(dir, srv_rel))
  man <- list(runtime = list(build = LLMR:::.llmr_local_runtime$build, asset = plat$key,
                             gpu = "none", archive = "runtime/a.tar.gz", server = srv_rel),
              models = list("smollm2-135m" = list(path = model_rel, size = e$size, sha256 = e$sha256,
                                                  kind = "chat", file = e$file)))
  LLMR:::.llmr_local_manifest_write(man, dir)
  testthat::local_mocked_bindings(
    .llmr_local_download = function(...) stop("no download expected"),
    .package = "LLMR")
  expect_no_error(llm_local_install("smollm2-135m", dir = dir, quiet = TRUE))
  expect_true(llm_local_ready("smollm2-135m", dir = dir))
  expect_false(llm_local_ready("qwen3-0.6b", dir = dir))
  # a truncated file is not counted as installed
  writeBin(as.raw(1:10), file.path(dir, model_rel))
  expect_false(llm_local_ready("smollm2-135m", dir = dir))
})

test_that("custom model specifications are checked", {
  s <- llm_local_model("someone/Some-GGUF", "Some-Model-Q8_0.gguf")
  expect_s3_class(s, "llmr_local_model")
  expect_identical(s$name, "some-model-q8_0")
  expect_error(llm_local_model("not-a-repo", "x.gguf"))
  expect_error(llm_local_model("a/b", "x.bin"))
  expect_error(llm_local_model("a/b", "x.gguf", sha256 = "abc"), "64 lower-case")
  expect_error(llm_local_model("a/b", "x.gguf", name = "bad name"), "letters, digits")
  expect_error(llm_local_model("a/b", "x.gguf", prefixes = c("q: ")), "named character")
})

test_that("embedding models are recognized by their registry kind", {
  expect_true(LLMR:::.is_embedding_config(llm_config("llamacpp", "bge-m3")))
  expect_true(LLMR:::.is_embedding_config(llm_config("llamacpp", "embeddinggemma-2")))
  expect_false(LLMR:::.is_embedding_config(llm_config("llamacpp", "qwen3-0.6b")))
  expect_false(LLMR:::.is_embedding_config(llm_config("llamacpp", "bge-m3", embedding = FALSE)))
})

test_that("task prefixes are applied, refused, or overridden as documented", {
  pre <- function(...) LLMR:::.llmr_local_prefix(llm_config("llamacpp", ...))$prefix
  expect_identical(pre("bge-m3"), "")
  expect_error(pre("bge-m3", task = "similarity"), "takes no task prefix")
  expect_identical(pre("bge-m3", task = "none"), "")
  expect_identical(pre("embeddinggemma-2"), "task: sentence similarity | query: ")
  expect_identical(pre("embeddinggemma-2", task = "document"), "title: none | text: ")
  expect_identical(pre("embeddinggemma-2", task = "none"), "")
  expect_identical(pre("embeddinggemma-2", prefix = "custom: "), "custom: ")
  expect_error(pre("embeddinggemma-2", task = "nonsense"), "Unknown task")
})

test_that("binding a config adds the key, the endpoint, and fresh prompts", {
  fake <- list(model = "qwen3-0.6b", url = "http://127.0.0.1:1", key = "k123")
  testthat::local_mocked_bindings(.llmr_local_ensure = function(model, ...) fake, .package = "LLMR")
  b <- LLMR:::.llmr_local_bind(llm_config("llamacpp", "qwen3-0.6b", thinking = FALSE),
                               "/v1/chat/completions")
  mp <- b$config$model_params
  expect_identical(mp$api_url, "http://127.0.0.1:1/v1/chat/completions")
  expect_false(mp$cache_prompt)
  expect_false(mp$chat_template_kwargs$enable_thinking)
  expect_null(mp$thinking)
  req <- mp$req_builder(httr2::request("http://x"))
  expect_true("Authorization" %in% names(req$headers))
  # a server the user runs is left alone
  withr::local_envvar(LLAMACPP_API_KEY = "")
  u <- LLMR:::.llmr_local_bind(llm_config("llamacpp", "x", api_url = "http://h:9/v1/chat/completions"),
                               "/v1/chat/completions")
  expect_null(u$srv)
  expect_null(u$config$model_params$req_builder)
})

test_that("request identity ignores the session and both thinking spellings agree", {
  cfg <- llm_config("llamacpp", "qwen3-0.6b", thinking = FALSE, temperature = 0)
  h <- llm_request_hash(config = cfg, messages = "hi")
  bound <- cfg
  bound$model_params$api_url <- "http://127.0.0.1:5555/v1/chat/completions"
  bound$model_params$cache_prompt <- FALSE
  expect_identical(llm_request_hash(config = bound, messages = "hi"), h)
  body <- list(model = "qwen3-0.6b", messages = list(list(role = "user", content = "hi")),
               temperature = 0, cache_prompt = FALSE,
               chat_template_kwargs = list(enable_thinking = FALSE))
  expect_identical(LLMR:::.llmr_body_params(body),
                   LLMR:::.llmr_drop_transport(cfg$model_params))
})

test_that("session keys and ports leave the user's random stream alone", {
  set.seed(110); a <- runif(3)
  set.seed(110); invisible(LLMR:::.llmr_local_key()); invisible(LLMR:::.llmr_free_port()); b <- runif(3)
  expect_identical(a, b)
  expect_match(LLMR:::.llmr_local_key(), "^[0-9a-f]{40}$")
})

# ---- token-level functions against canned server replies ---------------------

fake_srv <- list(model = "qwen3-0.6b", url = "http://127.0.0.1:1", key = "k", device = "cpu",
                 build = "b11669", sha256 = strrep("a", 64), ctx = 4096L, n_vocab = 3L)

top3 <- list(list(id = 10L, token = " Paris", bytes = list(32, 80), logprob = log(0.7)),
             list(id = 11L, token = " Lyon", bytes = list(32, 76), logprob = log(0.2)),
             list(id = 12L, token = " the", bytes = list(32, 116), logprob = log(0.1)))

local_fake_server <- function(env = parent.frame()) {
  sent <- new.env()
  sent$calls <- list()
  testthat::local_mocked_bindings(
    .llmr_local_research_server = function(config) fake_srv,
    .llmr_local_request = function(srv, path, body = NULL, method = "POST", log_kind = NULL) {
      sent$calls[[length(sent$calls) + 1L]] <- list(path = path, body = body)
      switch(path,
        "/apply-template" = {
          last <- body$messages[[length(body$messages)]]
          p <- "<|im_start|>user\nQ<|im_end|>\n<|im_start|>assistant\n"
          if (identical(last$role, "assistant")) p <- paste0(p, last$content)
          list(prompt = p)
        },
        "/tokenize" = if (identical(body$content, "\n")) list(tokens = list(198L))
          else if (isTRUE(body$with_pieces))
            list(tokens = list(list(id = 11L, piece = " Lyon"), list(id = 13L, piece = ".")))
          else list(tokens = list(1L, 2L, 3L)),
        "/completion" = {
          forced <- if (length(body$logit_bias)) body$logit_bias[[1]][[1]] else 10L
          list(content = " is fine.", stop_type = "eos", tokens_predicted = 3L,
               tokens_evaluated = 5L,
               completion_probabilities = list(list(id = forced, token = "t",
                                                    logprob = if (forced == 11L) log(0.2) else log(0.5),
                                                    top_logprobs = top3)))
        },
        "/detokenize" = list(content = "decoded"),
        stop("unexpected path ", path))
    },
    .package = "LLMR", .env = env)
  sent
}

test_that("llm_next_token returns the distribution with provenance", {
  sent <- local_fake_server()
  cfg <- llm_config("llamacpp", "qwen3-0.6b")
  nt <- llm_next_token(cfg, prompt = "The capital of France is", top_n = 3)
  expect_identical(nt$token, c(" Paris", " Lyon", " the"))
  expect_equal(nt$prob, c(0.7, 0.2, 0.1))
  expect_identical(nt$bytes[[1]], as.raw(c(32, 80)))
  comp <- Filter(function(x) x$path == "/completion", sent$calls)
  b <- comp[[1]]$body
  expect_identical(b$prompt, "The capital of France is")
  expect_identical(b$n_probs, 3L)
  expect_false(b$cache_prompt)
  # a complete token (newline) is forced so the list is never withheld
  expect_identical(b$logit_bias[[1]][[1]], 198L)
  prov <- attr(nt, "llmr_local")
  expect_identical(prov$device, "cpu")
  expect_identical(prov$context, "The capital of France is")
  # full = TRUE asks for the whole vocabulary
  invisible(llm_next_token(cfg, prompt = "x", full = TRUE))
  comp <- Filter(function(x) x$path == "/completion", sent$calls)
  expect_identical(comp[[2]]$body$n_probs, 3L)
})

test_that("contexts need exactly one of messages and prompt, and end with the prefill", {
  sent <- local_fake_server()
  cfg <- llm_config("llamacpp", "qwen3-0.6b")
  expect_error(llm_next_token(cfg), "exactly one")
  expect_error(llm_next_token(cfg, messages = "Q", prompt = "P"), "exactly one")
  invisible(llm_next_token(cfg, messages = "Q", prefill = "Honestly,"))
  tmpl <- sent$calls[[1]]$body
  expect_identical(tmpl$messages[[2]], list(role = "assistant", content = "Honestly,"))
  expect_false(tmpl$chat_template_kwargs$enable_thinking)
  comp <- Filter(function(x) x$path == "/completion", sent$calls)
  expect_true(endsWith(comp[[1]]$body$prompt, "Honestly,"))
  expect_true(endsWith(llm_render_template(cfg, "Q", prefill = "Yes,"), "Yes,"))
})

test_that("llm_continue samples from the full distribution and seeds each draw", {
  sent <- local_fake_server()
  cfg <- llm_config("llamacpp", "qwen3-0.6b")
  out <- llm_continue(cfg, prompt = "Once", prefill = " upon", n = 3, seed = 110, max_tokens = 5)
  expect_identical(nrow(out), 3L)
  expect_identical(out$seed, c(110, 111, 112))
  expect_identical(out$text[1], " upon is fine.")
  expect_identical(out$finish_reason[1], "stop")
  b <- sent$calls[[1]]$body
  expect_identical(c(b$temperature, b$top_p, b$min_p, b$repeat_penalty), c(1, 1, 0, 1))
  expect_identical(b$top_k, 0L)
  expect_identical(c(b$mirostat, b$dynatemp_range, b$top_n_sigma), c(0, 0, -1))
  expect_false(b$cache_prompt)
  # unseeded draws send no seed; config sampling settings are used
  out2 <- llm_continue(llm_config("llamacpp", "qwen3-0.6b", temperature = 0.3), prompt = "x")
  last <- sent$calls[[length(sent$calls)]]$body
  expect_null(last$seed)
  expect_identical(last$temperature, 0.3)
  expect_true(is.na(out2$seed))
  expect_warning(llm_continue(cfg, prompt = "x", n = 2, temperature = 0),
                 "single most probable")
  expect_no_warning(llm_continue(cfg, prompt = "x", n = 1, temperature = 0))
  expect_error(llm_continue(cfg, prompt = "x", n = 0), "at least 1")
  expect_error(llm_continue(cfg, prompt = "x", seed = 1.5), "0 to 4294967294")
  expect_error(llm_continue(cfg, prompt = "x", seed = 4294967295), "0 to 4294967294")
  # seeds above the integer range stay seeded
  big <- llm_continue(cfg, prompt = "x", n = 2, seed = 3e9)
  expect_identical(big$seed, c(3e9, 3e9 + 1))
  last <- sent$calls[[length(sent$calls)]]$body
  expect_identical(last$seed, 3e9 + 1)
})

test_that("llm_score forces each token, records its rank, and checks the reply", {
  sent <- local_fake_server()
  cfg <- llm_config("llamacpp", "qwen3-0.6b")
  s <- llm_score(cfg, " Lyon.", prompt = "The capital of France is")
  expect_identical(s$token, c(" Lyon", "."))
  expect_identical(s$bytes[[1]], charToRaw(" Lyon"))
  expect_equal(s$logprob[1], log(0.2))
  expect_identical(s$rank, c(2L, NA))
  expect_equal(attr(s, "total_logprob"), log(0.2) + log(0.5))
  comp <- Filter(function(x) x$path == "/completion", sent$calls)
  expect_length(comp, 2L)
  expect_identical(unlist(comp[[1]]$body$prompt), 1:3)
  expect_identical(unlist(comp[[2]]$body$prompt), c(1:3, 11L))
  expect_identical(comp[[2]]$body$logit_bias[[1]][[1]], 13L)
  expect_error(llm_score(cfg, "", prompt = "x"), "non-empty")
})

test_that("llm_choice_probs renormalizes over the options", {
  local_fake_server()
  cfg <- llm_config("llamacpp", "qwen3-0.6b")
  cp <- llm_choice_probs(cfg, c(" Lyon", " Lyon."), prompt = "x")
  expect_equal(sum(cp$prob), 1)
  expect_error(llm_choice_probs(cfg, " only", prompt = "x"), "at least two")
})

test_that("token-level functions refuse other providers and embedding models", {
  expect_error(llm_next_token(llm_config("openai", "gpt-4o-mini"), prompt = "x"), "local model")
  expect_error(llm_next_token(llm_config("llamacpp", "bge-m3"), prompt = "x"), "embedding model")
})

test_that("tokenize and detokenize return ids, pieces, and text", {
  local_fake_server()
  cfg <- llm_config("llamacpp", "qwen3-0.6b")
  tk <- llm_tokenize(cfg, "Lyon.")
  expect_identical(tk$id, c(11L, 13L))
  expect_identical(tk$bytes[[2]], charToRaw("."))
  expect_identical(llm_detokenize(cfg, tk$id), "decoded")
})

test_that("thinking follows the config, and with a prefill opens the turn itself", {
  sent <- local_fake_server()
  cfg <- llm_config("llamacpp", "qwen3-0.6b", thinking = TRUE)
  invisible(llm_next_token(cfg, messages = "Q", prefill = "<think>\nLet me"))
  tmpl <- sent$calls[[1]]$body
  expect_true(tmpl$chat_template_kwargs$enable_thinking)
  # no trailing assistant message: the prefill goes where the turn begins
  expect_length(tmpl$messages, 1L)
  comp <- Filter(function(x) x$path == "/completion", sent$calls)
  expect_true(endsWith(comp[[1]]$body$prompt, "assistant\n<think>\nLet me"))
  # an explicit argument wins over the config
  invisible(llm_next_token(cfg, messages = "Q", thinking = FALSE))
  tmpls <- Filter(function(x) x$path == "/apply-template", sent$calls)
  expect_false(tmpls[[length(tmpls)]]$body$chat_template_kwargs$enable_thinking)
  expect_error(llm_next_token(cfg, messages = "Q", thinking = NA), "TRUE or FALSE")
})

test_that("a token that is part of a character is scored from the whole distribution", {
  calls <- list()
  testthat::local_mocked_bindings(
    .llmr_local_research_server = function(config) fake_srv,
    .llmr_local_request = function(srv, path, body = NULL, method = "POST", log_kind = NULL) {
      calls[[length(calls) + 1L]] <<- list(path = path, body = body)
      if (path == "/tokenize") {
        if (identical(body$content, "\n")) return(list(tokens = list(198L)))
        if (isTRUE(body$with_pieces)) return(list(tokens = list(list(id = 12L, piece = list(219, 140)))))
        return(list(tokens = list(1L)))
      }
      forced <- body$logit_bias[[1]][[1]]
      if (forced == 12L) return(list(completion_probabilities = list()))
      # forcing the newline returns the full list, which holds the byte token
      list(completion_probabilities = list(list(id = 198L, token = "\n", logprob = -1,
                                                top_logprobs = top3)))
    }, .package = "LLMR")
  s <- llm_score(llm_config("llamacpp", "qwen3-0.6b"), "x", prompt = "p", top_n = 2)
  expect_equal(s$logprob, log(0.1))
  expect_identical(s$bytes[[1]], as.raw(c(219, 140)))
  expect_identical(nrow(s$top[[1]]), 2L)
  fb <- calls[[length(calls)]]$body
  expect_identical(fb$n_probs, fake_srv$n_vocab)
  expect_identical(fb$logit_bias[[1]][[1]], 198L)
})

test_that("uninstall touches only a directory with an LLMR manifest", {
  stray <- tempfile("not-models-")
  dir.create(stray)
  writeLines("draft", file.path(stray, "thesis.tex"))
  expect_error(llm_local_uninstall(all = TRUE, dir = stray), "no LLMR manifest")
  expect_true(file.exists(file.path(stray, "thesis.tex")))

  d <- tempfile("llmr-local-")
  dir.create(file.path(d, "models", "m"), recursive = TRUE)
  writeLines("x", file.path(d, "models", "m", "m.gguf"))
  writeLines("keep", file.path(d, "notes.txt"))
  LLMR:::.llmr_local_manifest_write(list(runtime = NULL, models = list()), d)
  expect_message(llm_local_uninstall(all = TRUE, dir = d), "Removed")
  expect_false(dir.exists(file.path(d, "models")))
  expect_true(file.exists(file.path(d, "notes.txt")))
})

test_that("custom model names and file paths cannot reach outside their folder", {
  expect_error(llm_local_model("a/b", "x.gguf", name = "."), "start with a letter")
  expect_error(llm_local_model("a/b", "x.gguf", name = ".."), "start with a letter")
  expect_error(llm_local_model("a/b", "../x.gguf"), "inside the repository")
  expect_error(llm_local_model("a/b", "/x.gguf"), "inside the repository")
  expect_identical(llm_local_model("a/b", "Q8_0/Model-Q8_0.gguf")$name, "model-q8_0")
})

test_that("a complete partial download is checked, not requested again", {
  src <- tempfile(fileext = ".bin")
  writeBin(as.raw(seq_len(200)), src)
  sha <- digest::digest(file = src, algo = "sha256")
  dest <- file.path(tempfile("dl-"), "z.bin")
  dir.create(dirname(dest))
  file.copy(src, paste0(dest, ".part"))
  # the URL is unreachable: success shows no request was made
  LLMR:::.llmr_local_download("http://127.0.0.1:9/never", dest, sha, 200, quiet = TRUE)
  expect_true(file.exists(dest))
})

test_that("the server environment drops the user's LLAMA_ settings", {
  withr::local_envvar(LLAMA_ARG_N_PREDICT = "3", LLAMA_ARG_CTX_SIZE = "16", HOME_KEEP = "yes")
  env <- LLMR:::.llmr_local_child_env("k1")
  expect_false(any(c("LLAMA_ARG_N_PREDICT", "LLAMA_ARG_CTX_SIZE") %in% names(env)))
  expect_identical(env[["LLAMA_API_KEY"]], "k1")
  expect_identical(env[["HOME_KEEP"]], "yes")
})

test_that("parallel runs bind local configs once, without storing the key in results", {
  fake <- list(model = "embeddinggemma-2", url = "http://127.0.0.1:1", key = "k9")
  testthat::local_mocked_bindings(.llmr_local_ensure = function(model, ...) fake, .package = "LLMR")
  cfgs <- list(llm_config("llamacpp", "embeddinggemma-2", task = "document"),
               llm_config("openai", "gpt-4o-mini"))
  b <- LLMR:::.llmr_local_prebind(cfgs)
  expect_identical(b[[1]]$model_params$api_url, "http://127.0.0.1:1/v1/embeddings")
  expect_true(b[[1]]$embedding)
  expect_identical(b[[1]]$model_params$prefix, "title: none | text: ")
  expect_null(b[[1]]$model_params$task)
  expect_identical(b[[2]], cfgs[[2]])
  expect_null(cfgs[[1]]$model_params$api_url)
})

test_that("a user-run server gets the key from LLAMACPP_API_KEY", {
  withr::local_envvar(LLAMACPP_API_KEY = "user-key")
  u <- LLMR:::.llmr_local_bind(llm_config("llamacpp", "x", api_url = "http://h:9/v1/chat/completions"),
                               "/v1/chat/completions")
  req <- u$config$model_params$req_builder(httr2::request("http://h:9"))
  expect_true("Authorization" %in% names(req$headers))
})

test_that("an unreadable manifest is reported, not read as an empty one", {
  d <- tempfile("llmr-local-")
  dir.create(d)
  writeLines("{ not json", file.path(d, "manifest.json"))
  withr::local_options(llmr.local_dir = d)
  expect_error(LLMR:::.llmr_local_lookup("my-custom-model"), "cannot be read")
  # registry models do not depend on the manifest
  expect_identical(LLMR:::.llmr_local_lookup("bge-m3")$kind, "embedding")
})
