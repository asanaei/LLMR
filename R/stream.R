# stream.R --------------------------------------------------------------------
# Server-sent-events streaming for chat models. One exported function,
# call_llm_stream(), built on the same request builders as call_llm(), so a
# streamed call sends exactly what a regular call would.

# Per-provider quirks for the OpenAI-compatible streaming path.
.compat_stream_spec <- function(provider) {
  endpoints <- c(
    openai   = "https://api.openai.com/v1/chat/completions",
    groq     = "https://api.groq.com/openai/v1/chat/completions",
    together = "https://api.together.xyz/v1/chat/completions",
    deepseek = "https://api.deepseek.com/chat/completions",
    xiaomi   = "https://api.xiaomimimo.com/v1/chat/completions",
    alibaba  = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions",
    zhipu    = "https://open.bigmodel.cn/api/paas/v4/chat/completions",
    moonshot = "https://api.moonshot.ai/v1/chat/completions",
    xai      = "https://api.x.ai/v1/chat/completions",
    ollama   = "http://localhost:11434/v1/chat/completions"
  )
  list(
    endpoint = endpoints[[provider]] %||% endpoints[["openai"]],
    auth = if (identical(provider, "xiaomi")) "api-key"
           else if (identical(provider, "ollama")) "none" else "bearer",
    drop = switch(provider,
      together = character(0),
      ollama   = character(0),
      alibaba  = "repetition_penalty",
      zhipu    = c("frequency_penalty", "presence_penalty", "repetition_penalty"),
      c("top_k", "repetition_penalty")
    ),
    # providers known to accept stream_options.include_usage (final usage chunk)
    usage_opt = provider %in% c("openai", "groq", "deepseek", "together", "alibaba")
  )
}

#' Stream a chat completion token by token
#'
#' Like [call_llm()], but the reply arrives incrementally: `callback` is
#' invoked with each text chunk as it is generated, and the complete
#' [llmr_response] is returned at the end. Streaming keeps long generations
#' responsive and avoids HTTP timeouts on slow, lengthy completions.
#'
#' Supported providers: all OpenAI-compatible chat APIs (openai, groq,
#' together, deepseek, xai, alibaba, zhipu, moonshot, xiaomi, ollama),
#' Anthropic, and Gemini. The request body is built by the same internals as
#' [call_llm()], so parameters, structured output, and hooks behave
#' identically; only the transport differs.
#'
#' @param config An [llm_config] for a generative model.
#' @param messages Messages as in [call_llm()].
#' @param callback Function called with each text chunk (a character scalar)
#'   as it arrives. The default prints chunks to the console with `cat()`.
#'   Reasoning deltas (when a provider streams them separately) are not passed
#'   to `callback`; they are collected into the result's `thinking` field.
#' @param verbose Print the assembled response object at the end.
#' @return An [llmr_response] assembled from the stream (invisibly). Token
#'   usage is filled when the provider reports it in the stream; otherwise it
#'   is `NA`.
#' @examples
#' \dontrun{
#' cfg <- llm_config("groq", "openai/gpt-oss-20b")
#' r <- call_llm_stream(cfg, "Tell a 100-word story about a lighthouse.")
#' tokens(r)
#' }
#' @seealso [call_llm()], [chat_session()]
#' @export
call_llm_stream <- function(config, messages,
                            callback = function(chunk) cat(chunk),
                            verbose = FALSE) {
  stopifnot(inherits(config, "llm_config"), is.function(callback))
  if (.is_embedding_config(config)) {
    stop("call_llm_stream() is for generative models; embeddings do not stream.",
         call. = FALSE)
  }
  prov <- config$provider %||% "openai"
  start_time <- Sys.time()

  if (identical(prov, "anthropic")) {
    out <- .stream_anthropic(config, messages, callback)
  } else if (identical(prov, "gemini")) {
    out <- .stream_gemini(config, messages, callback)
  } else {
    out <- .stream_openai_compat(config, messages, callback)
  }

  resp <- new_llmr_response(
    text          = out$text,
    provider      = prov,
    model         = config$model,
    finish_reason = out$finish %||% "stop",
    usage         = list(
      sent      = out$sent %||% NA_integer_,
      rec       = out$rec %||% NA_integer_,
      total     = if (!is.null(out$sent) || !is.null(out$rec)) {
        as.integer(sum(c(out$sent, out$rec), na.rm = TRUE))
      } else NA_integer_,
      reasoning = out$reasoning_tokens %||% NA_integer_,
      cached    = out$cached %||% NA_integer_
    ),
    response_id   = out$id %||% NA_character_,
    duration_s    = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    raw           = out$raw,
    raw_json      = NA_character_,
    model_version = out$model_version %||% NA_character_,
    thinking      = if (nzchar(out$thinking %||% "")) out$thinking else NA_character_
  )
  .llmr_log_event(kind = "stream", provider = prov, model = config$model,
                  status = 200L, response = resp)
  if (verbose) print(resp)
  invisible(resp)
}

# Shared SSE pump: hands each parsed JSON event to `handle`.
.stream_events <- function(req, handle) {
  con <- httr2::req_perform_connection(req)
  on.exit(close(con), add = TRUE)
  repeat {
    ev <- httr2::resp_stream_sse(con)
    if (is.null(ev)) break
    dat <- ev$data
    if (is.null(dat) || !nzchar(dat)) next
    if (identical(trimws(dat), "[DONE]")) break
    j <- tryCatch(jsonlite::fromJSON(dat, simplifyVector = FALSE),
                  error = function(e) NULL)
    if (!is.null(j)) handle(j)
  }
  invisible(NULL)
}

.stream_openai_compat <- function(config, messages, callback) {
  spec <- .compat_stream_spec(config$provider %||% "openai")
  extra <- list(stream = TRUE)
  if (spec$usage_opt) extra$stream_options <- list(include_usage = TRUE)
  req <- .compat_chat_request(
    config, messages,
    endpoint = get_endpoint(config, default_endpoint = spec$endpoint),
    auth_style = spec$auth, drop_params = spec$drop, extra_body = extra
  )

  acc <- new.env(parent = emptyenv())
  acc$text <- character(0); acc$thinking <- character(0)
  acc$finish <- NULL; acc$sent <- NULL; acc$rec <- NULL
  acc$reasoning_tokens <- NULL; acc$cached <- NULL
  acc$id <- NULL; acc$model_version <- NULL; acc$last <- NULL

  .stream_events(req, function(j) {
    acc$last <- j
    if (is.null(acc$id) && !is.null(j$id)) acc$id <- j$id
    if (is.null(acc$model_version) && !is.null(j$model)) acc$model_version <- j$model
    ch <- tryCatch(j$choices[[1]], error = function(e) NULL)
    if (!is.null(ch)) {
      d <- ch$delta
      if (!is.null(d$content) && is.character(d$content) && nzchar(d$content)) {
        acc$text <- c(acc$text, d$content)
        callback(d$content)
      }
      rc <- d$reasoning_content %||% d$reasoning
      if (!is.null(rc) && is.character(rc) && nzchar(rc)) {
        acc$thinking <- c(acc$thinking, rc)
      }
      if (!is.null(ch$finish_reason)) acc$finish <- ch$finish_reason
    }
    if (!is.null(j$usage)) {
      acc$sent <- as.integer(j$usage$prompt_tokens %||% NA_integer_)
      acc$rec  <- as.integer(j$usage$completion_tokens %||% NA_integer_)
      acc$reasoning_tokens <- as.integer(
        tryCatch(j$usage$completion_tokens_details$reasoning_tokens,
                 error = function(e) NA_integer_) %||% NA_integer_)
      acc$cached <- as.integer(
        tryCatch(j$usage$prompt_tokens_details$cached_tokens,
                 error = function(e) NA_integer_) %||% NA_integer_)
    }
  })

  list(text = paste(acc$text, collapse = ""),
       thinking = paste(acc$thinking, collapse = ""),
       finish = .map_stream_finish(acc$finish),
       sent = acc$sent, rec = acc$rec,
       reasoning_tokens = acc$reasoning_tokens, cached = acc$cached,
       id = acc$id, model_version = acc$model_version, raw = acc$last)
}

.stream_anthropic <- function(config, messages, callback) {
  req <- .anthropic_chat_request(config, messages, extra_body = list(stream = TRUE))

  acc <- new.env(parent = emptyenv())
  acc$text <- character(0); acc$thinking <- character(0)
  acc$finish <- NULL; acc$sent <- NULL; acc$rec <- NULL; acc$cached <- NULL
  acc$id <- NULL; acc$model_version <- NULL; acc$last <- NULL

  .stream_events(req, function(j) {
    acc$last <- j
    type <- j$type %||% ""
    if (identical(type, "message_start")) {
      acc$id <- j$message$id %||% acc$id
      acc$model_version <- j$message$model %||% acc$model_version
      acc$sent <- as.integer(j$message$usage$input_tokens %||% NA_integer_)
      acc$cached <- as.integer(j$message$usage$cache_read_input_tokens %||% NA_integer_)
    } else if (identical(type, "content_block_delta")) {
      d <- j$delta
      if (identical(d$type, "text_delta") && is.character(d$text) && nzchar(d$text)) {
        acc$text <- c(acc$text, d$text)
        callback(d$text)
      } else if (identical(d$type, "thinking_delta") && is.character(d$thinking)) {
        acc$thinking <- c(acc$thinking, d$thinking)
      }
    } else if (identical(type, "message_delta")) {
      if (!is.null(j$delta$stop_reason)) acc$finish <- j$delta$stop_reason
      if (!is.null(j$usage$output_tokens)) acc$rec <- as.integer(j$usage$output_tokens)
    }
  })

  list(text = paste(acc$text, collapse = ""),
       thinking = paste(acc$thinking, collapse = ""),
       finish = .map_stream_finish(acc$finish),
       sent = acc$sent, rec = acc$rec, cached = acc$cached,
       id = acc$id, model_version = acc$model_version, raw = acc$last)
}

.stream_gemini <- function(config, messages, callback) {
  req <- .gemini_chat_request(config, messages,
                              suffix = "streamGenerateContent",
                              query = list(alt = "sse"))

  acc <- new.env(parent = emptyenv())
  acc$text <- character(0); acc$thinking <- character(0)
  acc$finish <- NULL; acc$sent <- NULL; acc$rec <- NULL; acc$cached <- NULL
  acc$reasoning_tokens <- NULL
  acc$id <- NULL; acc$model_version <- NULL; acc$last <- NULL

  .stream_events(req, function(j) {
    acc$last <- j
    if (is.null(acc$id) && !is.null(j$responseId)) acc$id <- j$responseId
    if (is.null(acc$model_version) && !is.null(j$modelVersion)) acc$model_version <- j$modelVersion
    cand <- tryCatch(j$candidates[[1]], error = function(e) NULL)
    if (!is.null(cand)) {
      parts <- tryCatch(cand$content$parts, error = function(e) NULL)
      if (is.list(parts)) {
        for (p in parts) {
          if (!is.null(p$text) && is.character(p$text) && nzchar(p$text)) {
            if (isTRUE(p$thought)) {
              acc$thinking <- c(acc$thinking, p$text)
            } else {
              acc$text <- c(acc$text, p$text)
              callback(p$text)
            }
          }
        }
      }
      if (!is.null(cand$finishReason)) acc$finish <- cand$finishReason
    }
    if (!is.null(j$usageMetadata)) {
      um <- j$usageMetadata
      acc$sent <- as.integer(um$promptTokenCount %||% NA_integer_)
      acc$rec  <- as.integer(um$candidatesTokenCount %||% NA_integer_)
      acc$reasoning_tokens <- as.integer(um$thoughtsTokenCount %||% NA_integer_)
      acc$cached <- as.integer(um$cachedContentTokenCount %||% NA_integer_)
    }
  })

  list(text = paste(acc$text, collapse = ""),
       thinking = paste(acc$thinking, collapse = ""),
       finish = .map_stream_finish(acc$finish),
       sent = acc$sent, rec = acc$rec, cached = acc$cached,
       reasoning_tokens = acc$reasoning_tokens,
       id = acc$id, model_version = acc$model_version, raw = acc$last)
}

# Reuse the standardized finish vocabulary for stream-reported reasons.
.map_stream_finish <- function(fr) {
  if (is.null(fr)) return("stop")
  .std_finish_reason(list(choices = list(list(finish_reason = fr))))
}
