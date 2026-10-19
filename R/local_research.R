# local_research.R --------------------------------------------------------------
# Token-level access to a local model through llama.cpp's native endpoints:
# the next-token distribution at any point, continuations of a reply whose
# beginning the researcher writes, the log-probability of a given text, and the
# tokenizer and chat template. These need a llama.cpp server, so they work with
# provider "llamacpp" only.

# The server a research function talks to: the managed one for an installed
# model, or a server the user runs at `api_url` (with the key in
# LLAMACPP_API_KEY, if it needs one).
.llmr_local_research_server <- function(config) {
  if (!inherits(config, "llm_config") || !identical(config$provider, "llamacpp")) {
    stop("This function needs a local model: llm_config(\"llamacpp\", model), after llm_local_install().",
         call. = FALSE)
  }
  if (.is_embedding_config(config)) {
    stop(sprintf("%s is an embedding model; token-level functions need a generative model.", config$model),
         call. = FALSE)
  }
  url <- config$model_params$api_url
  if (is.null(url)) return(.llmr_local_ensure(config$model))
  base <- sub("/v1/.*$", "", sub("/+$", "", url))
  srv <- list(model = config$model, url = base, key = Sys.getenv("LLAMACPP_API_KEY", unset = ""),
              device = "unknown", build = "user-run server", sha256 = NA_character_,
              ctx = NA_integer_)
  meta <- .llmr_local_request(srv, "/v1/models", method = "GET")
  m1 <- if (!is.null(meta$data) && length(meta$data)) meta$data[[1]]$meta else NULL
  srv$n_vocab <- as.integer(m1$n_vocab %||% NA_integer_)
  srv$ctx <- as.integer(m1$n_ctx %||% NA_integer_)
  srv
}

.llmr_local_provenance <- function(srv, ...) {
  c(list(model = srv$model, sha256 = srv$sha256, llama_cpp = srv$build,
         device = srv$device, ctx = srv$ctx,
         time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")), list(...))
}

.llmr_local_chat_messages <- function(messages) {
  msgs <- .normalize_messages(messages)
  lapply(msgs, function(m) {
    if (!is.character(m$content) || length(m$content) != 1L) {
      stop("Token-level functions take text messages only.", call. = FALSE)
    }
    list(role = m$role, content = m$content)
  })
}

.llmr_local_thinking <- function(thinking, config) {
  th <- thinking %||% config$model_params$thinking %||% FALSE
  if (!is.logical(th) || length(th) != 1L || is.na(th)) {
    stop("`thinking` must be TRUE or FALSE.", call. = FALSE)
  }
  th
}

# The exact text the model reads: the chat template applied to `messages`, or
# `prompt` as written, followed by `prefill`. With thinking off, the prefill
# opens the assistant's answer (after the template's empty reasoning block, for
# models that have one); with thinking on, it is placed where the model's turn
# begins, so it can also open the reasoning (for Qwen3, "<think>\n...").
.llmr_local_context <- function(srv, config, messages = NULL, prompt = NULL,
                                prefill = NULL, thinking = FALSE) {
  if (is.null(messages) == is.null(prompt)) {
    stop("Supply exactly one of `messages` (chat, rendered with the model's template) or `prompt` (raw text).",
         call. = FALSE)
  }
  prefill <- prefill %||% ""
  if (!is.character(prefill) || length(prefill) != 1L || is.na(prefill)) {
    stop("`prefill` must be one string.", call. = FALSE)
  }
  if (!is.null(prompt)) {
    if (!is.character(prompt) || length(prompt) != 1L || is.na(prompt)) {
      stop("`prompt` must be one string.", call. = FALSE)
    }
    return(paste0(prompt, prefill))
  }
  e <- .llmr_local_lookup(config$model)
  if (isTRUE(e$base)) {
    warning(sprintf("%s is a base model without instruction tuning; its chat template may not be meaningful. Pass `prompt =` to give it raw text.",
                    config$model), call. = FALSE)
  }
  msgs <- .llmr_local_chat_messages(messages)
  kw <- list(enable_thinking = isTRUE(thinking))
  if (nzchar(prefill) && !isTRUE(thinking)) {
    with_prefill <- c(msgs, list(list(role = "assistant", content = prefill)))
    r <- .llmr_local_request(srv, "/apply-template",
                             list(messages = with_prefill, chat_template_kwargs = kw))
    if (is.character(r$prompt) && endsWith(r$prompt, prefill)) return(r$prompt)
  }
  r <- .llmr_local_request(srv, "/apply-template", list(messages = msgs, chat_template_kwargs = kw))
  paste0(r$prompt, prefill)
}

.llmr_check_count <- function(x, what, min = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x) || x < min ||
      x > .Machine$integer.max) {
    stop(sprintf("`%s` must be one whole number of at least %d.", what, min), call. = FALSE)
  }
  as.integer(x)
}

# llama.cpp seeds are unsigned 32-bit; 4294967295 means "random".
.llmr_check_seed <- function(seed) {
  if (is.null(seed)) return(NULL)
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != round(seed) ||
      seed < 0 || seed >= 4294967295) {
    stop("`seed` must be one whole number from 0 to 4294967294.", call. = FALSE)
  }
  as.numeric(seed)
}

.llmr_piece <- function(p) {
  if (is.character(p)) return(p)
  rawToChar(as.raw(unlist(p)))
}

.llmr_piece_bytes <- function(p) {
  if (is.character(p)) charToRaw(enc2utf8(p)) else as.raw(unlist(p))
}

.llmr_top_tibble <- function(tops) {
  lp <- vapply(tops, function(t) as.numeric(t$logprob), 0)
  tibble::tibble(
    rank    = seq_along(tops),
    id      = vapply(tops, function(t) as.integer(t$id), 0L),
    token   = vapply(tops, function(t) as.character(t$token), ""),
    logprob = lp,
    prob    = exp(lp),
    bytes   = lapply(tops, function(t) as.raw(unlist(t$bytes)))
  )
}

# The id of a token that is always a complete character. Forcing it keeps
# llama.cpp from withholding the probability list, which it does when the
# sampled token is part of a multi-byte character.
.llmr_local_anchor <- function(srv) {
  ids <- .llmr_local_request(srv, "/tokenize", list(content = "\n", add_special = FALSE))$tokens
  if (length(ids) != 1L) stop("The model's tokenizer has no single token for a newline.", call. = FALSE)
  as.integer(ids[[1]])
}

#' Render a chat as the text a local model reads
#'
#' Applies the model's own chat template, as llama.cpp does for a chat
#' request, and returns the resulting text. With `prefill`, the text ends with
#' the beginning of the assistant's turn, which is the context
#' [llm_continue()], [llm_next_token()], and [llm_score()] use.
#'
#' @param config An [llm_config()] for provider `"llamacpp"`.
#' @param messages Chat messages, as in [call_llm()].
#' @param prefill Text the assistant's turn starts with.
#' @param thinking For reasoning models such as Qwen3. With `FALSE` the
#'   template closes the model's reasoning block before the reply, so
#'   `prefill` opens the answer; with `TRUE` the turn is left open and
#'   `prefill` is placed where the model would begin, so it can also open the
#'   reasoning (for Qwen3, start it with `"<think>\n"`). By default the
#'   configuration's `thinking` setting, or else `FALSE`.
#' @return One string.
#' @seealso [llm_next_token()], [llm_continue()]
#' @examples
#' if (llm_local_ready("smollm2-135m")) {
#'   cfg <- llm_config("llamacpp", "smollm2-135m")
#'   llm_local_start("smollm2-135m", threads = 2)
#'   cat(llm_render_template(cfg, "Name a color.", prefill = "My answer is"))
#'   llm_local_stop()
#' }
#' @export
llm_render_template <- function(config, messages, prefill = NULL, thinking = NULL) {
  srv <- .llmr_local_research_server(config)
  .llmr_local_context(srv, config, messages = messages, prefill = prefill,
                      thinking = .llmr_local_thinking(thinking, config))
}

#' Tokenize and detokenize text with a local model's tokenizer
#'
#' @param config An [llm_config()] for provider `"llamacpp"`.
#' @param text One string.
#' @param special If `TRUE`, special-token text such as `<|im_start|>` is
#'   read as the special token; otherwise as plain text.
#' @param ids Integer token ids.
#' @return `llm_tokenize()` returns a tibble with `id`, `piece` (the token's
#'   text), and `bytes` (its raw bytes; a token can be part of a multi-byte
#'   character, whose `piece` alone is then not valid text).
#'   `llm_detokenize()` returns one string.
#' @examples
#' if (llm_local_ready("smollm2-135m")) {
#'   cfg <- llm_config("llamacpp", "smollm2-135m")
#'   llm_local_start("smollm2-135m", threads = 2)
#'   tk <- llm_tokenize(cfg, "The capital of France is Paris.")
#'   llm_detokenize(cfg, tk$id)
#'   llm_local_stop()
#' }
#' @export
llm_tokenize <- function(config, text, special = FALSE) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    stop("`text` must be one string.", call. = FALSE)
  }
  srv <- .llmr_local_research_server(config)
  r <- .llmr_local_request(srv, "/tokenize", list(content = text, add_special = FALSE,
                                                  parse_special = isTRUE(special),
                                                  with_pieces = TRUE))
  tibble::tibble(id = vapply(r$tokens, function(t) as.integer(t$id), 0L),
                 piece = vapply(r$tokens, function(t) .llmr_piece(t$piece), ""),
                 bytes = lapply(r$tokens, function(t) .llmr_piece_bytes(t$piece)))
}

#' @rdname llm_tokenize
#' @export
llm_detokenize <- function(config, ids) {
  if (!is.numeric(ids) || anyNA(ids)) stop("`ids` must be integer token ids.", call. = FALSE)
  srv <- .llmr_local_research_server(config)
  .llmr_local_request(srv, "/detokenize", list(tokens = as.list(as.integer(ids))))$content
}

#' The next-token distribution of a local model
#'
#' Returns the model's probability distribution over the next token after a
#' context: the chat template applied to `messages`, or the raw `prompt`,
#' followed by `prefill`. The probabilities are the model's own, before any
#' sampling setting (temperature, top-k, top-p) acts, so they do not depend
#' on the configuration's sampling parameters.
#'
#' llama.cpp computes the probabilities in single precision: the whole
#' vocabulary sums to 1 within about 5e-4, and a value can differ by about
#' 3e-4 with the number of tokens requested. The values also depend on the
#' device (see [llm_local_start()]).
#'
#' @param config An [llm_config()] for provider `"llamacpp"`.
#' @param messages Chat messages, as in [call_llm()]; rendered with the
#'   model's chat template. Supply this or `prompt`.
#' @param prompt Raw text, used as written (for base models, or to control the
#'   context exactly).
#' @param prefill Text appended to the context; with `messages`, the start of
#'   the assistant's turn (see [llm_render_template()]).
#' @param top_n Number of most probable tokens to return.
#' @param full If `TRUE`, return the whole vocabulary.
#' @param thinking See [llm_render_template()].
#' @return A tibble with `rank`, `id`, `token`, `logprob`, `prob`, and `bytes`
#'   (the token's raw bytes, which matter for tokens that are parts of a
#'   multi-byte character). The attribute `llmr_local` records the model, its
#'   file's sha256, the llama.cpp build, the device, and the context text.
#' @seealso [llm_continue()], [llm_score()], [llm_choice_probs()]
#' @examples
#' if (llm_local_ready("smollm2-135m")) {
#'   cfg <- llm_config("llamacpp", "smollm2-135m")
#'   llm_local_start("smollm2-135m", threads = 2)
#'   llm_next_token(cfg, prompt = "The capital of France is", top_n = 5)
#'   llm_local_stop()
#' }
#' @export
llm_next_token <- function(config, messages = NULL, prompt = NULL, prefill = NULL,
                           top_n = 20, full = FALSE, thinking = NULL) {
  srv <- .llmr_local_research_server(config)
  thinking <- .llmr_local_thinking(thinking, config)
  text <- .llmr_local_context(srv, config, messages, prompt, prefill, thinking)
  n <- if (isTRUE(full)) {
    if (is.na(srv$n_vocab)) stop("The server does not report its vocabulary size, so `full = TRUE` is unavailable.", call. = FALSE)
    srv$n_vocab
  } else .llmr_check_count(top_n, "top_n")
  # The returned list is the model's own distribution; forcing a complete
  # token to be "sampled" only keeps llama.cpp from withholding the list.
  r <- .llmr_local_request(srv, "/completion",
                           list(prompt = text, n_predict = 1L, n_probs = n, temperature = 0,
                                cache_prompt = FALSE, post_sampling_probs = FALSE,
                                logit_bias = list(list(.llmr_local_anchor(srv), 1000))),
                           log_kind = "local")
  cp <- r$completion_probabilities
  if (!length(cp) || !length(cp[[1]]$top_logprobs)) {
    stop("The server returned no token probabilities for this context.", call. = FALSE)
  }
  out <- .llmr_top_tibble(cp[[1]]$top_logprobs)
  attr(out, "llmr_local") <- .llmr_local_provenance(
    srv, context = text, n_context_tokens = as.integer(r$tokens_evaluated %||% NA_integer_),
    thinking = thinking)
  out
}

#' Continue a reply, or any text, with a local model
#'
#' Generates continuations of a context whose end the researcher writes: the
#' chat template applied to `messages` with the assistant's turn starting
#' with `prefill`, or the raw `prompt` followed by `prefill`. This shows how
#' the model goes on from words put in its mouth, and with `n > 1` gives a
#' sample of continuations from the same point.
#'
#' Unless the arguments or the configuration set them, the model samples from
#' its full distribution: temperature 1, with no top-k, top-p, or min-p
#' truncation, no repetition penalties, and no other sampler that would alter
#' the distribution. llama.cpp's own defaults (top-k 40, top-p 0.95, min-p
#' 0.05) are not used. Of the configuration's settings, `temperature`,
#' `top_p`, `top_k`, and `min_p` are used when the arguments are not given;
#' other generation settings in the configuration are not.
#'
#' @inheritParams llm_next_token
#' @param n Number of continuations.
#' @param max_tokens Largest number of tokens to generate for each.
#' @param seed `NULL` for unseeded sampling, or a whole number from 0 to
#'   4294967294: continuation `i` then uses seed `seed + i - 1`, so the sample
#'   can be reproduced on the same llama.cpp build and device.
#' @param temperature,top_p,top_k,min_p Sampling settings; `top_k` limits
#'   sampling to the `top_k` most probable tokens, with 0 meaning no limit.
#' @param top_n If above 0, also return, for each generated token, its
#'   log-probability and the `top_n` most probable alternatives. llama.cpp
#'   omits these for a token that is part of a multi-byte character.
#' @param ... Further llama.cpp `/completion` fields, sent as given (for
#'   example `stop`, `grammar`, or `logit_bias`).
#' @return A tibble with one row per continuation: `sample`, `seed`,
#'   `prefill`, `continuation`, `text` (`prefill` followed by the
#'   continuation), `finish_reason` (`"stop"` or `"length"`), `n_tokens`, and,
#'   with `top_n > 0`, a list column `logprobs`. The attribute `llmr_local`
#'   records the model, build, device, context, and sampling settings.
#' @seealso [llm_next_token()], [llm_score()]
#' @examples
#' if (llm_local_ready("smollm2-135m")) {
#'   cfg <- llm_config("llamacpp", "smollm2-135m")
#'   llm_local_start("smollm2-135m", threads = 2)
#'   llm_continue(cfg, messages = "Is coffee good for you?",
#'                prefill = "Honestly, I am not sure, but", n = 2,
#'                max_tokens = 20, seed = 110)
#'   llm_local_stop()
#' }
#' @export
llm_continue <- function(config, messages = NULL, prompt = NULL, prefill = "",
                         n = 1, max_tokens = 256, seed = NULL,
                         temperature = NULL, top_p = NULL, top_k = NULL, min_p = NULL,
                         thinking = NULL, top_n = 0, ...) {
  srv <- .llmr_local_research_server(config)
  thinking <- .llmr_local_thinking(thinking, config)
  n <- .llmr_check_count(n, "n")
  max_tokens <- .llmr_check_count(max_tokens, "max_tokens")
  top_n <- .llmr_check_count(top_n, "top_n", min = 0L)
  seed <- .llmr_check_seed(seed)
  mp <- config$model_params %||% list()
  samp <- list(temperature = temperature %||% mp$temperature %||% 1,
               top_p = top_p %||% mp$top_p %||% 1,
               top_k = as.integer(top_k %||% mp$top_k %||% 0L),
               min_p = min_p %||% mp$min_p %||% 0,
               typical_p = 1, repeat_penalty = 1, presence_penalty = 0,
               frequency_penalty = 0, dry_multiplier = 0, xtc_probability = 0,
               top_n_sigma = -1, mirostat = 0L, dynatemp_range = 0,
               adaptive_target = -1)
  if (n > 1L && samp$temperature == 0) {
    warning("With temperature 0 every continuation is the single most probable one; set `temperature` above 0 to sample.",
            call. = FALSE)
  }
  text <- .llmr_local_context(srv, config, messages, prompt, prefill, thinking)
  extra <- list(...)
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    body <- c(list(prompt = text, n_predict = max_tokens, cache_prompt = FALSE,
                   n_probs = top_n, return_tokens = TRUE), samp)
    s <- if (is.null(seed)) NA_real_ else (seed + i - 1) %% 4294967295
    if (!is.na(s)) body$seed <- s
    body <- utils::modifyList(body, extra)
    r <- .llmr_local_request(srv, "/completion", body, log_kind = "local")
    lp <- if (top_n > 0L) {
      cp <- r$completion_probabilities %||% list()
      tibble::tibble(
        pos = seq_along(cp),
        id = vapply(cp, function(e) as.integer(e$id), 0L),
        token = vapply(cp, function(e) as.character(e$token), ""),
        logprob = vapply(cp, function(e) as.numeric(e$logprob), 0),
        top = lapply(cp, function(e) .llmr_top_tibble(e$top_logprobs)))
    } else NULL
    fr <- switch(r$stop_type %||% "", eos = "stop", word = "stop", limit = "length", NA_character_)
    rows[[i]] <- tibble::tibble(
      sample = i, seed = s, prefill = prefill %||% "", continuation = r$content %||% "",
      text = paste0(prefill %||% "", r$content %||% ""), finish_reason = fr,
      n_tokens = as.integer(r$tokens_predicted %||% NA_integer_),
      logprobs = if (top_n > 0L) list(lp) else list(NULL))
  }
  out <- dplyr::bind_rows(rows)
  if (top_n == 0L) out$logprobs <- NULL
  attr(out, "llmr_local") <- .llmr_local_provenance(
    srv, context = text, thinking = thinking, sampling = samp,
    max_tokens = max_tokens, extra = if (length(extra)) extra else NULL)
  out
}

#' Score a text a local model did not write
#'
#' Computes the log-probability of each token of `continuation` given the
#' context before it, whether or not the model would have produced that
#' text. The continuation is tokenized on its own and appended to the
#' tokenized context; each token is scored by one request whose prompt is the
#' context and the tokens before it, read from the model's distribution at
#' that point (forcing the token does not change its value).
#'
#' The values come from processing each prefix afresh. The log-probabilities
#' llama.cpp reports while generating the same tokens come from processing
#' them one at a time, and the two differ by floating-point effects: by about
#' 1e-3 on Apple GPUs and by up to about 0.1 on the CPU. `cache = TRUE`
#' reuses the previous request's work, which reproduces the generation values
#' and is faster.
#'
#' @inheritParams llm_next_token
#' @param continuation The text to score, one string.
#' @param top_n Number of most probable alternatives to record at each
#'   position.
#' @param cache If `TRUE`, let the server reuse the shared prefix between
#'   requests (see Details).
#' @return A tibble with one row per token of `continuation`: `pos`, `id`,
#'   `token`, `bytes`, `logprob`, `prob`, `rank` (the token's rank among the
#'   alternatives, `NA` when below `top_n`), and `top` (a list of tibbles of
#'   the alternatives). Attributes `total_logprob` and `mean_logprob` give the
#'   sum and mean, and `llmr_local` the provenance and context.
#' @seealso [llm_choice_probs()], [llm_next_token()]
#' @examples
#' if (llm_local_ready("smollm2-135m")) {
#'   cfg <- llm_config("llamacpp", "smollm2-135m")
#'   llm_local_start("smollm2-135m", threads = 2)
#'   s <- llm_score(cfg, " Lyon.", prompt = "The capital of France is")
#'   attr(s, "total_logprob")
#'   llm_local_stop()
#' }
#' @export
llm_score <- function(config, continuation, messages = NULL, prompt = NULL,
                      prefill = NULL, top_n = 5, thinking = NULL, cache = FALSE) {
  if (!is.character(continuation) || length(continuation) != 1L || is.na(continuation) ||
      !nzchar(continuation)) {
    stop("`continuation` must be one non-empty string.", call. = FALSE)
  }
  top_n <- .llmr_check_count(top_n, "top_n")
  srv <- .llmr_local_research_server(config)
  thinking <- .llmr_local_thinking(thinking, config)
  text <- .llmr_local_context(srv, config, messages, prompt, prefill, thinking)
  ctx_ids <- vapply(.llmr_local_request(srv, "/tokenize",
                      list(content = text, add_special = TRUE, parse_special = TRUE))$tokens,
                    as.integer, 0L)
  ct <- .llmr_local_request(srv, "/tokenize", list(content = continuation, add_special = FALSE,
                                                   parse_special = FALSE, with_pieces = TRUE))$tokens
  ids <- vapply(ct, function(t) as.integer(t$id), 0L)
  pieces <- vapply(ct, function(t) .llmr_piece(t$piece), "")
  piece_bytes <- lapply(ct, function(t) .llmr_piece_bytes(t$piece))
  anchor <- NULL
  rows <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    prefix <- as.list(c(ctx_ids, ids[seq_len(i - 1L)]))
    r <- .llmr_local_request(srv, "/completion", list(
      prompt = prefix, n_predict = 1L, n_probs = top_n,
      temperature = 0, cache_prompt = isTRUE(cache), post_sampling_probs = FALSE,
      logit_bias = list(list(ids[[i]], 1000))), log_kind = "local")
    cp <- if (length(r$completion_probabilities)) r$completion_probabilities[[1]] else NULL
    if (!is.null(cp) && length(cp$top_logprobs)) {
      if (!identical(as.integer(cp$id), ids[[i]])) {
        stop(sprintf("The server scored token %s where %d was requested at position %d.",
                     format(cp$id), ids[[i]], i), call. = FALSE)
      }
      lp <- as.numeric(cp$logprob)
      top <- .llmr_top_tibble(cp$top_logprobs)
    } else {
      # A token that is part of a multi-byte character: llama.cpp gives no
      # list when it is the sampled token, so read its value from the whole
      # distribution while a complete token is sampled instead.
      if (is.na(srv$n_vocab %||% NA)) {
        stop(sprintf("Token %d at position %d is part of a multi-byte character, and the server does not report its vocabulary size, so its probability cannot be read.",
                     ids[[i]], i), call. = FALSE)
      }
      anchor <- anchor %||% .llmr_local_anchor(srv)
      r <- .llmr_local_request(srv, "/completion", list(
        prompt = prefix, n_predict = 1L, n_probs = srv$n_vocab,
        temperature = 0, cache_prompt = isTRUE(cache), post_sampling_probs = FALSE,
        logit_bias = list(list(anchor, 1000))), log_kind = "local")
      all_top <- .llmr_top_tibble(r$completion_probabilities[[1]]$top_logprobs)
      hit <- match(ids[[i]], all_top$id)
      if (is.na(hit)) {
        stop(sprintf("Token %d at position %d is missing from the model's distribution.", ids[[i]], i),
             call. = FALSE)
      }
      lp <- all_top$logprob[[hit]]
      top <- all_top[seq_len(min(top_n, nrow(all_top))), ]
    }
    rows[[i]] <- tibble::tibble(pos = i, id = ids[[i]], token = pieces[[i]],
                                bytes = list(piece_bytes[[i]]), logprob = lp, prob = exp(lp),
                                rank = match(ids[[i]], top$id), top = list(top))
  }
  out <- dplyr::bind_rows(rows)
  attr(out, "total_logprob") <- sum(out$logprob)
  attr(out, "mean_logprob") <- mean(out$logprob)
  attr(out, "llmr_local") <- .llmr_local_provenance(
    srv, context = text, n_context_tokens = length(ctx_ids), thinking = thinking,
    cache = isTRUE(cache))
  out
}

#' Probabilities a local model gives to a set of options
#'
#' Scores each option as a continuation of the same context with
#' [llm_score()] and returns its total log-probability and its probability
#' renormalized over the options. Longer options have lower total
#' probabilities by construction, so options of similar length, such as
#' `"Yes"` and `"No"`, compare most directly; `n_tokens` is reported for that
#' reason.
#'
#' @inheritParams llm_next_token
#' @param options Character vector of options, each scored as a continuation.
#'   Write each as the model would produce it: after a prompt that ends
#'   mid-sentence, a word usually begins with a space (`" Paris"`); at the
#'   start of a chat reply it does not (`"Yes"`).
#' @return A tibble with `option`, `logprob`, `n_tokens`, and `prob`.
#' @seealso [llm_score()]
#' @examples
#' if (llm_local_ready("smollm2-135m")) {
#'   cfg <- llm_config("llamacpp", "smollm2-135m")
#'   llm_local_start("smollm2-135m", threads = 2)
#'   llm_choice_probs(cfg, c(" Paris", " Lyon"), prompt = "The capital of France is")
#'   llm_local_stop()
#' }
#' @export
llm_choice_probs <- function(config, options, messages = NULL, prompt = NULL,
                             prefill = NULL, thinking = NULL) {
  if (!is.character(options) || length(options) < 2L || anyNA(options) ||
      any(!nzchar(options)) || anyDuplicated(options)) {
    stop("`options` must hold at least two distinct, non-empty strings.", call. = FALSE)
  }
  sc <- lapply(options, function(o)
    llm_score(config, o, messages = messages, prompt = prompt, prefill = prefill,
              top_n = 1, thinking = thinking))
  lp <- vapply(sc, function(s) attr(s, "total_logprob"), 0)
  w <- exp(lp - max(lp))
  out <- tibble::tibble(option = options, logprob = lp,
                        n_tokens = vapply(sc, nrow, 0L), prob = w / sum(w))
  attr(out, "llmr_local") <- attr(sc[[1]], "llmr_local")
  out
}
