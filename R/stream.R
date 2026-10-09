# stream.R --------------------------------------------------------------------
# Server-sent-events streaming for chat models. One exported function,
# call_llm_stream(), built on the same request builders as call_llm(), so a
# streamed call sends exactly what a regular call would.

# Per-provider quirks for the OpenAI-compatible streaming path.
.compat_stream_spec <- function(provider) {
  provider <- provider %||% "openai"   # NULL -> OpenAI-compatible default
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
    openrouter = "https://openrouter.ai/api/v1/chat/completions",
    ollama   = "http://localhost:11434/v1/chat/completions"
  )
  list(
    # `[[` with an unknown name errors ("subscript out of bounds") before %||%
    # can fall back, so guard the lookup; an unlisted (or NULL) provider streams
    # against the OpenAI-compatible default, matching call_llm()'s default route.
    endpoint = if (isTRUE(provider %in% names(endpoints))) endpoints[[provider]]
               else endpoints[["openai"]],
    auth = if (identical(provider, "xiaomi")) "api-key"
           else if (identical(provider, "ollama")) "none" else "bearer",
    drop = switch(provider,
      together = character(0),
      ollama   = character(0),
      openrouter = character(0),
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
#' [call_llm()], so parameters and structured output behave identically; the
#' `request_modifier` and `req_builder` hooks and the request timeout apply as
#' well. Only the transport differs. The `response_modifier` hook is the one
#' exception: it rewrites a full parsed response, which has no per-chunk meaning
#' for a stream, so it is not applied here.
#'
#' A stream counts as complete only when the provider signals completion. An
#' HTTP error, an error event sent inside the stream, or a connection that ends
#' before the provider's completion signal raises a typed `llmr_api_*_error`
#' that also has class `llmr_stream_error`; the text received before the
#' failure is in the condition's `partial_text` field. The call is not retried,
#' because `callback` may already have emitted part of the text. Events that
#' cannot be parsed are skipped with a warning that gives their number. An error
#' raised by `callback` itself propagates unchanged.
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
  call <- rlang::current_env()

  if (identical(prov, "anthropic")) {
    out <- .stream_anthropic(config, messages, callback, call = call)
  } else if (identical(prov, "gemini")) {
    out <- .stream_gemini(config, messages, callback, call = call)
  } else {
    out <- .stream_openai_compat(config, messages, callback, call = call)
  }

  resp <- new_llmr_response(
    text          = out$text,
    provider      = prov,
    model         = config$model,
    finish_reason = out$finish %||% "stop",
    usage         = list(
      sent      = out$sent %||% NA_integer_,
      rec       = out$rec %||% NA_integer_,
      total     = {
        s <- out$sent; r <- out$rec
        if (is.null(s) && is.null(r)) NA_integer_
        else if (is.na(s %||% NA_integer_) && is.na(r %||% NA_integer_)) NA_integer_
        else as.integer(sum(c(s, r), na.rm = TRUE))
      },
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
                  status = out$status, request = out$req, response = resp)
  if (verbose) print(resp)
  invisible(resp)
}

# Apply the req_builder hook and the request timeout to a streaming request,
# exactly as perform_request() does for the non-streaming path. Without this the
# streaming transport ignored both, contradicting call_llm_stream()'s promise
# that hooks behave identically. (request_modifier is already applied by the
# request builders; response_modifier operates on a full parsed response and has
# no per-chunk meaning, so it does not apply to the incremental stream.)
.llmr_apply_req_hooks <- function(req, config) {
  if (!is.null(config) && is.function(config$model_params$req_builder)) {
    req <- config$model_params$req_builder(req)
  }
  tmo <- suppressWarnings(as.numeric(
    (if (!is.null(config)) config$model_params$timeout else NULL) %||%
      getOption("llmr.timeout", 600)))
  if (length(tmo) == 1L && is.finite(tmo) && tmo > 0) {
    req <- httr2::req_timeout(req, tmo)
  }
  req
}

# Shared SSE pump: applies the request hooks, then hands each parsed JSON event
# to `handle`, which records text and metadata in `acc`. The stream counts as
# complete only when the provider signals it: `[DONE]`, Anthropic's
# `message_stop` (both set `acc$done`), or a finish reason in any event (set by
# the handlers in `acc$finish`). An HTTP failure, a provider error event, or a
# stream that ends without such a signal raises a typed LLMR error carrying the
# text received so far; nothing is retried, since `callback` may already have
# emitted that text. Returns the HTTP status and the request as sent.
.stream_events <- function(req, config, handle, acc, call = rlang::caller_env()) {
  start_time <- Sys.time()
  req <- .llmr_apply_req_hooks(req, config)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  con <- httr2::req_perform_connection(req)
  on.exit(close(con), add = TRUE)
  status <- httr2::resp_status(con)
  if (status >= 400) {
    raw_err <- .llmr_stream_body_text(con)
    err <- try(jsonlite::fromJSON(raw_err, simplifyVector = FALSE), silent = TRUE)
    .llmr_http_failure(status, err, raw_err, con, req,
                       config$provider %||% "openai", config$model, start_time,
                       call = call)
  }
  skipped <- 0L
  repeat {
    ev <- httr2::resp_stream_sse(con)
    if (is.null(ev)) break
    dat <- ev$data
    if (is.null(dat) || !nzchar(dat)) next
    if (identical(trimws(dat), "[DONE]")) {
      acc$done <- TRUE
      break
    }
    j <- tryCatch(jsonlite::fromJSON(dat, simplifyVector = FALSE),
                  error = function(e) NULL)
    if (is.null(j)) {
      skipped <- skipped + 1L
      next
    }
    if (!is.null(j$error) || identical(j$type, "error")) {
      fields <- .llmr_error_fields(j)
      .llmr_stream_fail(fields$reason, acc, req, config, status, start_time,
                        code = fields$code, response_body = j, call = call)
    }
    handle(j)
  }
  if (is.null(acc$finish) && !isTRUE(acc$done)) {
    .llmr_stream_fail("The stream ended before the provider signalled completion.",
                      acc, req, config, status, start_time, call = call)
  }
  if (skipped > 0L) {
    warning(sprintf(paste0(
      "%d streamed event(s) could not be parsed as JSON and were skipped; ",
      "the returned text may be incomplete."), skipped), call. = FALSE)
  }
  list(status = status, req = req)
}

# Read the remaining body of a streaming response, for error reporting.
# resp_stream_raw() in bounded chunks works on every supported httr2 version.
.llmr_stream_body_text <- function(con) {
  buf <- raw(0)
  repeat {
    chunk <- httr2::resp_stream_raw(con, kb = 64)
    if (!length(chunk)) break
    buf <- c(buf, chunk)
  }
  trimws(rawToChar(buf))
}

# Log and raise the typed error for a stream that failed after the connection
# opened. The text received before the failure travels as `partial_text`.
.llmr_stream_fail <- function(reason, acc, req, config, status, start_time,
                              code = NA_character_, response_body = NULL,
                              call = rlang::caller_env()) {
  provider <- config$provider %||% "openai"
  partial <- paste(acc$text, collapse = "")
  category <- .llmr_classify_error(provider, NA_integer_, code, reason)
  .llmr_log_event(
    kind = "error", provider = provider, model = config$model, status = status,
    request = req, error_message = reason,
    duration_s = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  )
  .llmr_error(
    message = paste(c(
      "LLM stream failed.",
      paste0("Reason: ", reason),
      if (nzchar(partial))
        sprintf("%d characters arrived before the failure; they are in the condition's `partial_text`.",
                nchar(partial))
    ), collapse = "\n"),
    category = category,
    provider = provider,
    model = config$model %||% NA_character_,
    code = code,
    response_body = response_body,
    class = "llmr_stream_error",
    call = call,
    partial_text = partial
  )
}

.stream_openai_compat <- function(config, messages, callback, call = rlang::caller_env()) {
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
  acc$id <- NULL; acc$model_version <- NULL; acc$last <- NULL; acc$done <- NULL

  ev <- .stream_events(req, config, acc = acc, call = call, handle = function(j) {
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
       id = acc$id, model_version = acc$model_version, raw = acc$last,
       status = ev$status, req = ev$req)
}

.stream_anthropic <- function(config, messages, callback, call = rlang::caller_env()) {
  req <- .anthropic_chat_request(config, messages, extra_body = list(stream = TRUE))

  acc <- new.env(parent = emptyenv())
  acc$text <- character(0); acc$thinking <- character(0)
  acc$finish <- NULL; acc$sent <- NULL; acc$rec <- NULL; acc$cached <- NULL
  acc$id <- NULL; acc$model_version <- NULL; acc$last <- NULL; acc$done <- NULL

  ev <- .stream_events(req, config, acc = acc, call = call, handle = function(j) {
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
    } else if (identical(type, "message_stop")) {
      acc$done <- TRUE
    }
  })

  list(text = paste(acc$text, collapse = ""),
       thinking = paste(acc$thinking, collapse = ""),
       finish = .map_stream_finish(acc$finish),
       sent = acc$sent, rec = acc$rec, cached = acc$cached,
       id = acc$id, model_version = acc$model_version, raw = acc$last,
       status = ev$status, req = ev$req)
}

.stream_gemini <- function(config, messages, callback, call = rlang::caller_env()) {
  req <- .gemini_chat_request(config, messages,
                              suffix = "streamGenerateContent",
                              query = list(alt = "sse"))

  acc <- new.env(parent = emptyenv())
  acc$text <- character(0); acc$thinking <- character(0)
  acc$finish <- NULL; acc$sent <- NULL; acc$rec <- NULL; acc$cached <- NULL
  acc$reasoning_tokens <- NULL
  acc$id <- NULL; acc$model_version <- NULL; acc$last <- NULL; acc$done <- NULL

  ev <- .stream_events(req, config, acc = acc, call = call, handle = function(j) {
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
       id = acc$id, model_version = acc$model_version, raw = acc$last,
       status = ev$status, req = ev$req)
}

# Reuse the standardized finish vocabulary for stream-reported reasons.
.map_stream_finish <- function(fr) {
  if (is.null(fr)) return("stop")
  .std_finish_reason(list(choices = list(list(finish_reason = fr))))
}
