# LLMR.R
# -------------------------------------------------------------------
# This file provides the core functionality for the LLMR package,
# including configuration, API dispatching, and response parsing.
# It defines the main S3 generic `call_llm()` and provides specific
# implementations for various providers like OpenAI, Anthropic, Gemini, etc.
#
# Key Features:
#   1. llm_config() - Standardized configuration object.
#   2. call_llm() - S3 generic for dispatching to the correct provider API.
#   3. Provider-specific implementations (e.g., call_llm.openai).
#   4. Support for both generative and embedding models.
#   5. (New) Support for multimodal inputs (text and files) for capable providers.
# -------------------------------------------------------------------

#' LLMR: unified LLM workflows for R
#'
#' LLMR provides provider-agnostic text generation, embeddings, structured JSON,
#' XML-like tag extraction, chat sessions, robust retries, and parallel
#' experiment utilities.
#'
#' @section Common workflows:
#' - One prompt: [call_llm()] or [call_llm_robust()]
#' - Vectors or data frames: [llm_fn()]
#' - dplyr pipelines: [llm_mutate()], including shorthand
#'   `llm_mutate(new_col = "{existing_col}", .config = cfg)`
#' - Strict structured fields: [llm_mutate_structured()] or [llm_mutate()]
#'   with `.structured = TRUE`
#' - Soft structured fields: [llm_mutate_tags()] or [llm_mutate()] with `.tags`
#' - Factorial experiments: [build_factorial_experiments()] and [call_llm_par()]
#'
#' @section Reliability and parallel execution:
#' [call_llm_robust()] retries transient failures. [setup_llm_parallel()]
#' controls worker count for parallel helpers; [llm_fn()] and [llm_mutate()]
#' use [call_llm_broadcast()] internally.
#'
#' @keywords internal
"_PACKAGE"

# ----- Internal Helper Functions -----

#' Normalise message inputs  (LLMR.R)
#'
#' Called once in `call_llm()` (not in embedding mode).
#' Rules, in order:
#'   1. Already-well-formed list -> returned untouched.
#'   2. Plain character vector   -> each element becomes a `"user"` turn.
#'   3. Named char vector **without** "file" -> names are roles (legacy path).
#'   4. Named char vector **with**  "file"  -> multimodal shortcut:
#'        - any `system` entries become separate system turns
#'        - consecutive {user | file} entries combine into one user turn
#'        - every `file` path is tilde-expanded
#'
#' @keywords internal
#' @noRd
.normalize_messages <- function(messages) {

  ## -- 1 - leave proper message objects unchanged -----------------------
  if (is.list(messages) &&
      length(messages)        > 0L &&
      is.list(messages[[1]])  &&
      !is.null(messages[[1]]$role) &&
      !is.null(messages[[1]]$content)) {
    return(messages)
  }

  ## -- 2 - character vectors --------------------------------------------
  if (is.character(messages)) {
    msg_names <- names(messages)

    ### 2a - *unnamed* -> each element a user turn
    if (is.null(msg_names)) {
      return(lapply(messages,
                    function(txt) list(role = "user", content = txt)))
    }

    ### 2b - *named* but no "file" -> legacy path
    if (!"file" %in% msg_names) {
      return(unname(purrr::imap(messages,
                                \(txt, role) list(role = role, content = txt))))
    }

    ### 2c - multimodal shortcut ----------------------------------------
    # ensure names are never NA (happens after `c()` with empty strings)
    msg_names[is.na(msg_names) | msg_names == ""] <- "user"

    final_messages <- list()
    i <- 1L
    while (i <= length(messages)) {
      role <- msg_names[i]

      if (role %in% c("user", "file")) {              # start a user block
        user_parts <- list()
        j <- i
        has_text  <- FALSE
        while (j <= length(messages) &&
               msg_names[j] %in% c("user", "file")) {

          if (msg_names[j] == "user") {
            user_parts <- append(user_parts,
                                 list(list(type = "text",
                                           text = unname(messages[j]))))
            has_text <- TRUE
          } else {  # msg_names[j] == "file"
            user_parts <- append(user_parts,
                                 list(list(type = "file",
                                           path = path.expand(unname(messages[j])))))
          }
          j <- j + 1L
        }
        if (!has_text)
          stop("A user block containing a 'file' part must also contain at least one 'user' text part.")

        final_messages <- append(final_messages,
                                 list(list(role = "user",
                                           content = purrr::compact(user_parts))))
        i <- j                                           # advance
      } else {                                           # system / assistant
        final_messages <- append(final_messages,
                                 list(list(role = role,
                                           content = unname(messages[i]))))
        i <- i + 1L
      }
    }
    return(unname(final_messages))
  }

  stop("`messages` must be a character vector or a list of message objects.")
}






#' Process a file for multimodal API calls
#'
#' Reads a file, determines its MIME type, and base64 encodes it.
#' This is an internal helper function.
#' @param file_path The path to the file.
#' @return A list containing the mime_type and base64_data.
#' @keywords internal
#' @noRd
#' @importFrom mime guess_type
#' @importFrom base64enc base64encode
.process_file_content <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found at path: ", file_path)
  }
  # Guess MIME type from file extension
  mime_type <- mime::guess_type(file_path, empty = "application/octet-stream")

  # Read file and encode using the reliable base64enc package
  base64_data <- base64enc::base64encode(what = file_path)

  return(list(
    mime_type = mime_type,
    base64_data = base64_data
  ))
}

## keep only NULL-free elements -----
## this makes sure innocent api calls (what the user doesn't explicitly mention
## is not mentioned in the api call)
.drop_null <- function(x) purrr::compact(x)


#' Perform API Request
#'
#' Internal helper function to perform the API request and process the response.
#'
#' @keywords internal
#' @noRd
#' @importFrom httr2 req_perform resp_body_raw resp_body_json req_error resp_status resp_header
perform_request <- function(req, verbose, provider = NULL, model = NULL, config = NULL) {
  start_time <- Sys.time()

  # MODIFIABILITY HOOK 1: Intercept/Modify the raw httr2 HTTP request (headers, etc.)
  if (!is.null(config) && is.function(config$model_params$req_builder)) {
    req <- config$model_params$req_builder(req)
  }

  # A stuck connection should fail, not hang the session. Every request carries
  # a total timeout in seconds: per-config `timeout = ...` wins, then
  # options(llmr.timeout = ...), then 600.
  tmo <- suppressWarnings(as.numeric(
    (if (!is.null(config)) config$model_params$timeout else NULL) %||%
      getOption("llmr.timeout", 600)
  ))
  if (length(tmo) == 1L && is.finite(tmo) && tmo > 0) {
    req <- httr2::req_timeout(req, tmo)
  }

  req <- httr2::req_error(req, is_error = \(resp) FALSE)
  resp <- httr2::req_perform(req)
  code <- httr2::resp_status(resp)

  if (code >= 400) {
    # Parse the error body tolerantly. Some providers (e.g. DeepSeek) return a
    # JSON error body under Content-Type: application/octet-stream, which
    # resp_body_json() rejects by default; check_type = FALSE lets us read the
    # real message instead of masking it. The happy-path 200 parse below stays
    # strict. raw_err is the final fallback when the body has no error$message.
    err <- try(httr2::resp_body_json(resp, check_type = FALSE), silent = TRUE)
    raw_err <- try(httr2::resp_body_string(resp), silent = TRUE)
    raw_err <- if (inherits(raw_err, "try-error")) NA_character_ else trimws(raw_err)
    category <- if (code >= 500) "server" else if (code == 429) "rate_limit"
    else if (code %in% c(401L, 403L)) "auth"
    else if (code == 408) "server"            # request timeout: transient, retried like a 5xx
    else if (code == 400) "param" else "unknown"
    bad_param <- if (!inherits(err, "try-error")) err$error$param %||% err$param %||% NA_character_ else NA_character_
    raw_tail <- if (!is.na(raw_err) && nzchar(raw_err)) substr(raw_err, 1L, 2000L) else NULL
    err_reason <- if (!inherits(err, "try-error")) {
      err$error$message %||% err$message %||% err$error$type %||% err$error$code %||%
        raw_tail %||% "No message supplied"
    } else {
      raw_tail %||% "No message supplied"
    }
    msg_lines <- c(
      "LLM API request failed.",
      paste0("HTTP status: ", code),
      paste0("Reason: ", err_reason),
      "Tip: check model params for provider/API version."
    )
    .llmr_log_event(
      kind = "error", provider = provider, model = model, status = code,
      request = req, error_message = err_reason,
      duration_s = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )
    .llmr_error(
      message     = paste(msg_lines, collapse = "\n"),
      category    = category,
      status_code = code,
      provider    = provider %||% NA_character_,
      model       = model %||% NA_character_,
      param       = bad_param,
      code        = if (!inherits(err, "try-error")) err$error$code %||% err$code %||% NA_character_ else NA_character_,
      request_id  = httr2::resp_header(resp, "x-request-id") %||%
        httr2::resp_header(resp, "request-id"),
      retry_after = suppressWarnings(as.numeric(httr2::resp_header(resp, "retry-after") %||% NA_real_)),
      body        = if (!inherits(err, "try-error")) err
                    else if (!is.null(raw_tail)) list(raw = raw_tail) else NULL
    )
  }

  raw_response <- httr2::resp_body_raw(resp)
  raw_json     <- rawToChar(raw_response)
  content      <- httr2::resp_body_json(resp)

  # MODIFIABILITY HOOK 2: Intercept/Modify the incoming JSON payload before the package parses it
  if (!is.null(config) && is.function(config$model_params$response_modifier)) {
    content <- config$model_params$response_modifier(content)
  }

  if (verbose) {
    cat("Full API Response:\n")
    print(content)
  }

  is_embedding_like <- is.list(content) &&
    (!is.null(content$data) || !is.null(content$embedding) || !is.null(content$embeddings))
  if (is_embedding_like) {
    .llmr_log_event(
      kind = "embedding", provider = provider, model = model, status = code,
      request = req,
      duration_s = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )
    return(content)
  }

  txt <- extract_text(content)
  fr  <- .std_finish_reason(content)
  tc  <- .token_counts(content)
  rt  <- .reasoning_tokens_from(content)

  # Support Responses API fallback for total tokens
  direct_total <- if (!is.null(content$usage) && !is.null(content$usage$total_tokens)) content$usage$total_tokens else NULL

  sent_i <- as.integer(tc$sent %||% NA_integer_)
  rec_i  <- as.integer(tc$rec  %||% NA_integer_)
  # total: prefer a provider-reported total; otherwise sum sent+rec, but only if
  # at least one part is known (NA + NA stays NA = "unknown", not a false 0).
  total_i <- if (!is.null(direct_total)) as.integer(direct_total)
             else if (is.na(sent_i) && is.na(rec_i)) NA_integer_
             else as.integer(sum(c(sent_i, rec_i), na.rm = TRUE))
  usage <- list(sent = sent_i, rec = rec_i, total = total_i,
                reasoning = as.integer(rt %||% NA_integer_),
                cached = as.integer(.cached_tokens_from(content) %||% NA_integer_))

  out <- new_llmr_response(
    text         = txt %||% NA_character_,
    provider     = provider %||% NA_character_,
    model        = model %||% NA_character_,
    finish_reason= fr,
    usage        = usage,
    response_id  = .response_id_from(content),
    duration_s   = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    raw          = content,
    raw_json     = raw_json,
    model_version = .model_version_from(content),
    thinking     = .thinking_from(content)
  )

  attr(out, "full_response") <- content
  attr(out, "raw_json")      <- raw_json

  .llmr_log_event(
    kind = "call", provider = provider, model = model, status = code,
    request = req, response = out
  )

  out
}



#' Extract Text from API Response
#' @keywords internal
#' @noRd
extract_text <- function(content) {
  if (is.list(content) && (!is.null(content$data) || !is.null(content$embedding))) return(content)

  # Support for OpenAI's new Responses API structure
  if (identical(content$object, "response") && !is.null(content$output) && is.list(content$output)) {
    out_texts <- c()
    for (item in content$output) {
      if (identical(item$type, "message") && is.list(item$content)) {
        for (cblock in item$content) {
          if (identical(cblock$type, "output_text") || identical(cblock$type, "text")) {
            if (!is.null(cblock$text)) out_texts <- c(out_texts, cblock$text)
          }
        }
      } else if (identical(item$type, "output_text") || identical(item$type, "text")) {
          if (!is.null(item$text)) out_texts <- c(out_texts, item$text)
      } else if (is.character(item$output_text)) {
          out_texts <- c(out_texts, item$output_text)
      } else if (is.character(item$text)) {
          out_texts <- c(out_texts, item$text)
      }
    }
    if (length(out_texts) > 0) return(paste(out_texts, collapse = "\n"))
    return(NA_character_)
  }

  if (!is.null(content$choices)) {
    if (length(content$choices) == 0) return(NA_character_)
    ch <- content$choices[[1]]
    if (!is.null(ch$message) && !is.null(ch$message$content)) return(ch$message$content)
    # An explicit refusal replaces content; surface its text rather than NA so
    # the user can see why nothing came back (finish_reason maps to "filter").
    if (!is.null(ch$message) && is.character(ch$message$refusal) &&
        nzchar(ch$message$refusal[1])) return(ch$message$refusal[1])
    if (!is.null(ch$text)) return(ch$text)
    return(NA_character_)
  }

  if (!is.null(content$content)) {
    cc <- content$content
    if (length(cc) == 0) return(NA_character_)
    is_tool_use <- function(b) identical(b$type, "tool_use")
    has_tool <- any(vapply(cc, is_tool_use, logical(1)))
    if (has_tool) {
      idx <- which(vapply(cc, is_tool_use, logical(1)))[1]
      tu  <- cc[[idx]]
      if (!is.null(tu$input)) {
        if (is.character(tu$input) && length(tu$input) == 1L) return(tu$input)
        return(jsonlite::toJSON(tu$input, auto_unbox = TRUE, null = "null"))
      }
    }
    text_blocks <- Filter(function(b) !is.null(b$text) && is.character(b$text) && nzchar(b$text[1]), cc)
    if (length(text_blocks) > 0) {
      return(paste(vapply(text_blocks, function(b) b$text[1], character(1)),
                   collapse = "\n"))
    }
    return(NA_character_)
  }

  if (!is.null(content$candidates)) {
    if (length(content$candidates) == 0) return(NA_character_)
    cand <- content$candidates[[1]]
    if (!is.null(cand$content) && is.character(cand$content)) {
      out <- paste(cand$content[nzchar(cand$content)], collapse = "\n")
      return(if (nzchar(out)) out else NA_character_)
    }
    get_parts <- function(obj) {
      if (is.null(obj)) return(NULL)
      if (is.list(obj) && !is.null(obj$parts)) return(obj$parts)
      if (is.list(obj) && length(obj) >= 1L && is.list(obj[[1]]) && !is.null(obj[[1]]$parts)) return(obj[[1]]$parts)
      NULL
    }
    parts <- get_parts(cand$content)
    if (!is.null(parts) && length(parts) > 0) {
      # Parts flagged `thought = TRUE` are Gemini reasoning traces (returned
      # when include_thoughts is requested); they belong in $thinking, not in
      # the answer text.
      texts <- vapply(parts, function(p) {
        if (isTRUE(p$thought)) return("")
        if (!is.null(p$text) && is.character(p$text)) p$text else ""
      }, character(1))
      out <- paste(texts[nzchar(texts)], collapse = "\n")
      if (nzchar(out)) return(out)
    }
    if (!is.null(cand$text) && is.character(cand$text) && nzchar(cand$text)) return(cand$text)
    allowed_keys <- c("text", "outputText", "output_text", "candidateText")
    find_text <- function(x) {
      if (is.list(x)) {
        nm <- names(x)
        if (!is.null(nm) && length(nm)) {
          for (k in allowed_keys) if (k %in% nm && is.character(x[[k]]) && nzchar(x[[k]][1])) return(x[[k]][1])
          for (nm_i in nm) {
            if (identical(nm_i, "role")) next
            val <- find_text(x[[nm_i]])
            if (!is.null(val) && nzchar(val)) return(val)
          }
        } else {
          for (el in x) {
            val <- find_text(el)
            if (!is.null(val) && nzchar(val)) return(val)
          }
        }
      }
      NULL
    }
    val <- find_text(cand)
    return(if (!is.null(val) && nzchar(val)) val else NA_character_)
  }
  NA_character_
}



#' Format Anthropic Messages
#'
#' Internal helper function to format messages for Anthropic API.
#' This helper is now simplified as logic is moved into call_llm.anthropic
#'
#' @keywords internal
#' @noRd
format_anthropic_messages <- function(messages) {
  system_messages <- purrr::keep(messages, ~ .x$role == "system")
  user_messages <- purrr::keep(messages, ~ .x$role != "system")

  system_text <- if (length(system_messages) > 0) {
    paste(sapply(system_messages, function(x) x$content), collapse = " ")
  } else {
    NULL
  }

  # The complex formatting is now handled directly in call_llm.anthropic
  # to support multimodal content. This function just separates system/user messages.
  list(system_text = system_text, user_messages = user_messages)
}

# Helper to determine the endpoint
get_endpoint <- function(config, default_endpoint) {
  if (!is.null(config$model_params$api_url)) {
    return(config$model_params$api_url)
  }
  default_endpoint
}

# ----- Exported Functions -----

#' Create an LLM configuration (provider-agnostic)
#'
#' `llm_config()` builds a provider-agnostic configuration object that
#' `call_llm()` (and friends) understand. You can pass provider-specific
#' parameters via `...`; LLMR forwards them as-is, with a few safe conveniences.
#'
#' @param provider Character scalar naming the backend. Known providers:
#'   `"openai"`, `"anthropic"`, `"gemini"`, `"groq"`, `"together"`,
#'   `"deepseek"`, `"xai"`, `"xiaomi"`, `"alibaba"` (Alibaba Cloud DashScope,
#'   OpenAI-compatible mode; serves the Qwen models), `"zhipu"`, `"moonshot"`,
#'   `"voyage"` (embeddings only), and `"ollama"` (local server, usually
#'   keyless). Other names are accepted and routed through the
#'   OpenAI-compatible path.
#'
#'   When `api_key` is omitted, LLMR reads the key from the environment using a
#'   formulaic default: it tries `<PROVIDER>_API_KEY` and then `<PROVIDER>_KEY`,
#'   upper-cased (e.g. `OPENAI_API_KEY`, or `ALIBABA_API_KEY`/`ALIBABA_KEY`). The
#'   one exception is Gemini with `vertex = TRUE`, which reads `VERTEX_ACCESS_TOKEN`.
#' @param model Character scalar. Model name understood by the chosen provider.
#'   (e.g., `"gpt-4.1-nano"`, `"gpt-5-nano"`, `"gemini-2.5-flash-lite"`,
#'   `"openai/gpt-oss-20b"`, etc.)
#' @param api_key Provider API key. Preferred form is `llm_api_key_env("VAR")`,
#'   referencing an environment variable by name (see `provider` for the
#'   formulaic defaults). A bare environment-variable name or `"env:VAR"`
#'   string also works, as does a character vector of variable names tried in
#'   order. Supplying a literal key string is accepted but discouraged and
#'   triggers a warning. When omitted (or given as an empty string, which is
#'   what `Sys.getenv()` returns for an unset variable), the provider default
#'   is used. Printing a config never reveals the key.
#' @param troubleshooting Logical. If `TRUE`, prints the messages and the config
#'   for debugging. The API key is masked in this output, not shown.
#' @param base_url Optional character. Back-compat alias; if supplied it is
#'   stored as `api_url` in `model_params` and overrides the default endpoint.
#' @param embedding `NULL` (default), `TRUE`, or `FALSE`. If `TRUE`, the call
#'   is routed to the provider's embeddings API; if `FALSE`, to the chat API.
#'   If `NULL`, LLMR infers embeddings when `model` contains `"embedding"`.
#' @param no_change Logical. If `TRUE`, LLMR **never** auto-renames/adjusts
#'   provider parameters. If `FALSE` (default), well-known compatibility shims
#'   may apply (e.g., renaming OpenAI's `max_tokens` -> `max_completion_tokens`
#'   after a server hint; see `call_llm()` notes).
#' @param ... Additional model parameters. LLMR understands a small canonical
#'   set spelled the OpenAI way and translates it per provider, so you can keep
#'   one vocabulary across backends:
#'   \itemize{
#'     \item `temperature`, `top_p`, `top_k`, `max_tokens`,
#'           `frequency_penalty`, `presence_penalty`, `repetition_penalty` --
#'           sampling controls. Parameters a provider does not accept are
#'           dropped with a console note (e.g., `repetition_penalty` for
#'           Gemini); spellings a provider renames are renamed
#'           (e.g., `max_tokens` becomes `maxOutputTokens` for Gemini).
#'     \item `seed` -- request reproducible sampling where supported
#'           (OpenAI-compatible providers and Gemini; Anthropic has no seed).
#'           Determinism is not guaranteed; record `model_version` from the
#'           response for the full picture.
#'     \item `logprobs`, `top_logprobs` -- token log-probabilities where
#'           supported (OpenAI-compatible chat APIs and Gemini). Retrieve them
#'           tidily with [llm_logprobs()].
#'     \item `thinking_budget`, `include_thoughts` -- reasoning controls for
#'           Gemini and Anthropic. `thinking_budget` caps reasoning tokens
#'           (`thinkingConfig.thinkingBudget` on Gemini,
#'           `thinking.budget_tokens` on Anthropic, where it must be smaller
#'           than `max_tokens`). `include_thoughts = TRUE` asks Gemini to
#'           return its reasoning; Anthropic returns thinking blocks whenever
#'           thinking is on. Returned reasoning lands in the response's
#'           `thinking` field.
#'     \item `timeout` -- total request timeout in seconds (default 600; also
#'           settable globally via `options(llmr.timeout = ...)`).
#'     \item `cache` -- set `cache = TRUE` to mark the system prompt and tools
#'           as cacheable for Anthropic (prompt caching). OpenAI, Gemini,
#'           DeepSeek, and several compatible providers cache long prompt
#'           prefixes automatically; cached token counts are reported in
#'           `tokens(x)$cached` either way.
#'     \item Anything else (e.g., `reasoning_effort`, `api_url`,
#'           provider-specific flags) is forwarded verbatim on the
#'           OpenAI-compatible providers, so new provider features work
#'           without waiting for an LLMR release. Anthropic and Gemini have
#'           stricter request shapes: their builders send recognized fields
#'           only, and quietly note (once per session) anything they drop.
#'           The `req_builder` / `request_modifier` hooks remain the escape
#'           hatch for arbitrary fields on those providers.
#'   }
#'
#' @section Advanced hooks:
#' Three optional functions in `...` customize the HTTP exchange when a
#' provider needs something unusual (a gateway header, an exotic body field, a
#' nonstandard response envelope). All are applied on every request for every
#' provider:
#' \itemize{
#'   \item `request_modifier`: `function(body) -> body`, edits the JSON body
#'         before serialization (OpenAI-compatible chat paths).
#'   \item `req_builder`: `function(req) -> req`, edits the `httr2` request
#'         (headers, URL, auth) just before it is performed.
#'   \item `response_modifier`: `function(content) -> content`, edits the
#'         parsed JSON before LLMR interprets it.
#' }
#'
#' @return An object of class `c("llm_config", provider)`. Fields:
#'   `provider`, `model`, `api_key`, `troubleshooting`, `embedding`,
#'   `no_change`, and `model_params` (a named list of extras). `print()` masks
#'   the API key.
#'
#' @section Temperature range clamping:
#' Anthropic temperatures must be in `[0, 1]`; others in `[0, 2]`. Out-of-range
#' values are clamped with a warning. Reasoning or thinking-oriented models may
#' reject custom temperature values; omit `temperature` unless the selected
#' model accepts it.
#'
#' @section Endpoint overrides:
#' You can pass `api_url` (or `base_url=` alias) in `...` to point to gateways
#' or compatible proxies.
#'
#' @section Vertex Gemini:
#' Use `provider = "gemini", vertex = TRUE` for Gemini on Vertex AI. Supply
#' `project` and optionally `location`; when `api_key` is omitted, LLMR looks for
#' `VERTEX_ACCESS_TOKEN` and sends it as a Bearer token.
#'
#' @seealso
#'   \code{\link{call_llm}},
#'   \code{\link{call_llm_robust}},
#'   \code{\link{llm_chat_session}},
#'   \code{\link{call_llm_par}},
#'   \code{\link{get_batched_embeddings}}
#'
#'@export
#'
#' @examples
#' \dontrun{
#' # Basic OpenAI config
#' cfg <- llm_config("openai", "gpt-4.1-nano",
#' temperature = 0.7, max_tokens = 300)
#'
#' # Generative call returns an llmr_response object
#' r <- call_llm(cfg, "Say hello in Greek.")
#' print(r)
#' as.character(r)
#'
#' # Embeddings (inferred from the model name)
#' e_cfg <- llm_config("gemini", "gemini-embedding-001")
#'
#' # Force embeddings even if model name does not contain "embedding"
#' e_cfg2 <- llm_config("voyage", "voyage-3.5-lite", embedding = TRUE)
#'
#' # Gemini through Vertex AI. VERTEX_ACCESS_TOKEN should contain a Bearer token.
#' v_cfg <- llm_config(
#'   "gemini", "gemini-2.5-flash-lite",
#'   vertex = TRUE,
#'   project = "my-gcp-project",
#'   location = "us-central1",
#'   api_key = "VERTEX_ACCESS_TOKEN"
#' )
#' }
llm_config <- function(provider, model, api_key = NULL,
                       troubleshooting = FALSE,
                       base_url = NULL,
                       embedding = NULL,
                       no_change = FALSE,
                       ...) {
  model_params <- list(...)
  ## clamp temperature to valid range
  if (!is.null(model_params$temperature)) {
    temp <- model_params$temperature
    if (identical(provider, "anthropic")) {
      if (temp < 0 || temp > 1) {
        temp <- min(max(temp, 0), 1)
        warning(paste0("Anthropic temperature must be between 0 and 1; setting it at: ", temp))
      }
    } else {
      if (temp < 0 || temp > 2) {
        temp <- min(max(temp, 0), 2)
        warning(paste0("Temperature must be between 0 and 2; setting it at: ", temp))
      }
    }
    model_params$temperature <- temp
  }
  ## end clamp

  # Handle base_url passed via ... for backward compatibility, renaming to api_url internally
  if (!is.null(base_url)) {
    model_params$api_url <- base_url
  }
  # Normalize API key: keep only an environment reference in the config
  api_key_handle <- NULL
  empty_string_key <- is.character(api_key) && length(api_key) == 1L &&
    !is.na(api_key) && !nzchar(api_key)
  if (empty_string_key) {
    # "" is what Sys.getenv() returns for an unset variable; the user meant
    # "no key supplied", so fall back to the provider defaults rather than
    # storing an empty literal that would send "Authorization: Bearer ".
    warning("api_key is an empty string; falling back to the provider's default ",
            "environment variables. Prefer omitting api_key altogether.")
  }
  if (missing(api_key) || is.null(api_key) || empty_string_key) {
    default_key_env <- if (identical(tolower(provider), "gemini") &&
                           isTRUE(model_params$vertex)) {
      "VERTEX_ACCESS_TOKEN"
    } else {
      .default_api_key_env(provider)
    }
    api_key_handle <- llm_api_key_env(default_key_env)
  } else if (inherits(api_key, "llmr_secret")) {
    api_key_handle <- api_key
  } else if (is.character(api_key) && anyNA(api_key)) {
    stop("'api_key' must not be NA. Omit it to use the provider's default environment variables.")
  } else if (is.character(api_key) && length(api_key) > 1L) {
    # A vector means a list of environment-variable names tried in order.
    nm <- sub("^env:", "", api_key)
    if (!all(grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm))) {
      stop("A vector 'api_key' must contain only environment-variable names, ",
           "e.g. c(\"MY_PRIMARY_KEY\", \"MY_FALLBACK_KEY\").")
    }
    api_key_handle <- llm_api_key_env(nm)
  } else if (is.character(api_key) && length(api_key) == 1L) {
    if (grepl("^env:", api_key)) {
      api_key_handle <- llm_api_key_env(sub("^env:", "", api_key))
    } else if (grepl("^[A-Z][A-Z0-9_]*$", api_key)) {
      # A bare ENV-NAME-shaped string is treated as an environment-variable
      # reference whether or not it is currently set, matching the documented
      # contract. If it is unset, resolution fails cleanly at call time with a
      # "missing env var" error rather than silently sending the literal name as
      # the key. Real API keys contain lowercase/digits/hyphens and so do not
      # match this pattern; use the "env:" prefix to force env semantics anyway.
      api_key_handle <- llm_api_key_env(api_key)
    } else {
      api_key_handle <- structure(
        list(kind = "literal", value = api_key),
        class = c("llmr_secret", "llmr_secret_literal")
      )
      warning(sprintf(
        "A literal API key was supplied. Prefer '%s' in ~/.Renviron.",
        paste(.default_api_key_env(provider), collapse = " or ")
      ))
    }
  } else {
    stop("Unsupported 'api_key' argument. Use llm_api_key_env(\"", .default_api_key_env(provider), "\") or a valid env var name.")
  }

  config <- list(
    provider = provider,
    model = model,
    api_key = api_key_handle,
    troubleshooting = troubleshooting,
    embedding = embedding,
    no_change = isTRUE(no_change),
    model_params = model_params
  )
  class(config) <- c("llm_config", provider)
  return(config)
}

#' Print an LLM configuration with the API key masked
#'
#' Configurations never print their key: a literal key shows as
#' `<llmr_secret: literal>` and an environment reference as
#' `<llmr_secret: env:VARNAME>`, so configs are safe to print in scripts,
#' logs, and rendered documents.
#'
#' @param x An `llm_config` object.
#' @param ... Ignored.
#' @return `x` invisibly (for `print`); a character vector (for `format`).
#' @export
print.llm_config <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @rdname print.llm_config
#' @export
format.llm_config <- function(x, ...) {
  mp <- x$model_params %||% list()
  fmt_val <- function(v) {
    if (is.function(v)) return("<function>")
    if (is.list(v)) return(sprintf("<list of %d>", length(v)))
    paste(utils::head(as.character(v), 5L), collapse = ",")
  }
  param_str <- if (length(mp)) {
    paste(vapply(names(mp), function(nm) paste0(nm, "=", fmt_val(mp[[nm]])),
                 character(1)), collapse = ", ")
  } else "none"
  c(
    sprintf("<llm_config  provider=%s  model=%s%s>",
            x$provider %||% "?", x$model %||% "?",
            if (isTRUE(x$embedding)) "  [embedding]" else ""),
    sprintf("  api_key: %s", .mask_api_key(x$api_key)),
    sprintf("  params:  %s", param_str)
  )
}

# Internal: should this config be routed to the provider's embeddings API?
# Explicit embedding = TRUE/FALSE always wins; with embedding = NULL the model
# name decides (the documented inference: it contains "embedding").
.is_embedding_config <- function(config) {
  if (!is.null(config$embedding)) return(isTRUE(config$embedding))
  grepl("embedding", config$model %||% "", ignore.case = TRUE)
}

#' Call an LLM (chat/completions or embeddings) with optional multimodal input
#'
#' `call_llm()` dispatches to the correct provider implementation based on
#' `config$provider`. It supports both generative chat/completions and
#' embeddings, plus a simple multimodal shortcut for local files.
#'
#' @param config An \code{\link{llm_config}} object.
#' @param messages One of:
#'   \itemize{
#'     \item Plain character vector - each element becomes a `"user"` message.
#'     \item Named character vector - names are roles (`"system"`, `"user"`,
#'           `"assistant"`). **Multimodal shortcut:** include one or more
#'           elements named `"file"` whose values are local paths; consecutive
#'           `{user | file}` entries are combined into one user turn and files
#'           are inlined (base64) for capable providers.
#'     \item List of message objects: `list(role=..., content=...)`. For
#'           multimodal content, set `content` to a list of parts like
#'           `list(list(type="text", text="..."), list(type="file", path="..."))`.
#'   }
#' @param verbose Logical. If `TRUE`, prints the full parsed API response.
#'
#' @return
#' - Generative mode: an `llmr_response` object. Use `as.character(x)` to get just the text; `print(x)` shows text plus a status line; use helpers `finish_reason(x)` and `tokens(x)`.
#' - Embedding mode: provider-native list with an element `data`; convert with [parse_embeddings()].
#'
#' @section Provider notes:
#' \itemize{
#'   \item \strong{OpenAI-compatible:} On a server 400 that identifies the bad
#'         parameter as `max_tokens`, LLMR will, unless `no_change=TRUE`,
#'         retry once replacing `max_tokens` with `max_completion_tokens`
#'         (and inform via a `cli_alert_info`). The former experimental
#'         "uncapped retry on empty content" is \emph{disabled} by default to
#'         avoid unexpected costs.
#'   \item \strong{Anthropic:} `max_tokens` is required; if omitted LLMR uses
#'         `2048` and warns. Multimodal images are inlined as base64 and PDFs
#'         as document blocks. Extended thinking is supported: provide
#'         `thinking_budget` (which must stay below `max_tokens`) and the
#'         response will carry `content` blocks of type `"thinking"`, also
#'         exposed as the `thinking` field of the result. Beta features can be
#'         requested by passing `anthropic_beta = "..."`, sent as the
#'         `anthropic-beta` header.
#'   \item \strong{Gemini (REST):} `systemInstruction` is supported; user
#'         parts use `text`/`inlineData(mimeType,data)`; responses are set to
#'         `responseMimeType = "text/plain"`. For Vertex AI, use
#'         `provider = "gemini", vertex = TRUE, project = ...`.
#'   \item \strong{Ollama (local):} OpenAI-compatible endpoints on `http://localhost:11434/v1/*`;
#'         no Authorization header is required. Override with `api_url` as needed.
#'   \item \strong{Alibaba / Moonshot regions:} Defaults target the
#'         \emph{international} endpoints (`dashscope-intl.aliyuncs.com` and
#'         `api.moonshot.ai`). China-region accounts must pass `api_url` for the
#'         mainland hosts (`dashscope.aliyuncs.com` and `api.moonshot.cn`);
#'         using the wrong region returns HTTP 401.
#'   \item \strong{Error handling:} HTTP errors raise structured conditions with
#'         classes like `llmr_api_param_error`, `llmr_api_rate_limit_error`,
#'         `llmr_api_server_error`; see the condition fields for status, code,
#'         request id, and (where supplied) the offending parameter.
#' }
#'
#' @section Message normalization:
#' See the \emph{"multimodal shortcut"} described under `messages`. Internally,
#' LLMR expands these into the provider's native request shape and tilde-expands
#' local file paths.
#'
#' @seealso
#'   \code{\link{llm_config}},
#'   \code{\link{call_llm_robust}},
#'   \code{\link{llm_chat_session}},
#'   \code{\link{parse_embeddings}},
#'   \code{\link{finish_reason}},
#'   \code{\link{tokens}}
#'
#' @examples
#' \dontrun{
#' ## 1) Basic generative call
#' cfg <- llm_config("openai", "gpt-5-nano")
#' call_llm(cfg, "Say hello in Greek.")
#'
#' ## 2) Generative with rich return
#' r <- call_llm(cfg, "Say hello in Greek.")
#' r
#' as.character(r)
#' finish_reason(r); tokens(r)
#'
#' ## 3) Anthropic extended thinking (single example)
#' ## max_tokens must cover the thinking budget plus the visible reply.
#' a_cfg <- llm_config("anthropic", "claude-sonnet-4-6",
#'                     max_tokens = 20000,
#'                     thinking_budget = 16000)
#' r2 <- call_llm(a_cfg, "Compute 87*93 in your head. Give only the final number.")
#' # reasoning text: r2$thinking
#' # final text:     as.character(r2)
#'
#' ## 4) Multimodal (named-vector shortcut)
#' msg <- c(
#'   system = "Answer briefly.",
#'   user   = "Describe this image in one sentence.",
#'   file   = "~/Pictures/example.png"
#' )
#' call_llm(cfg, msg)
#'
#' ## 5) Embeddings
#' e_cfg <- llm_config("voyage", "voyage-3.5-lite",
#'                     embedding = TRUE)
#' emb_raw <- call_llm(e_cfg, c("first", "second"))
#' emb_mat <- parse_embeddings(emb_raw)
#'
#' ## 6) With a chat session
#' ch <- chat_session(cfg)
#' ch$send("Say hello in Greek.")   # prints the same status line as `print.llmr_response`
#' ch$history()
#' }
#'
#' @export
call_llm <- function(config, messages, verbose = FALSE) {
  if (isTRUE(config$troubleshooting)) {
    cat("\n\nInside call_llm for troubleshooting\n")
    print(messages)
    print(.mask_config_for_print(config))
    cat("\n\n")
  }

  UseMethod("call_llm", config)
}

#' @export
call_llm.default <- function(config, messages, verbose = FALSE) {
  # This default is mapped to the OpenAI-compatible endpoint structure
  message("Provider-specific function not present, defaulting to OpenAI format.")
  call_llm.openai(config, messages, verbose)
}
#' @export
call_llm.openai <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    return(call_llm.openai_embedding(config, messages, verbose))
  }
  messages <- .normalize_messages(messages)
  mp <- config$model_params %||% list()
  if (!isTRUE(config$no_change)) {
    bad <- intersect(names(mp), c("top_k", "repetition_penalty"))
    if (length(bad)) {
      .llmr_param_note(sprintf("Dropped unsupported parameters for %s: %s",
                               config$provider, paste(bad, collapse = ", ")))
      mp <- mp[setdiff(names(mp), bad)]
    }
  }
  api_url_param <- mp$api_url %||% ""

  # Auto-detect the Responses API for models that are only served there
  responses_only_models <- "^(o1-pro|o3-pro|gpt-5-pro|o3-deep-research|o4-mini-deep-research)"
  is_responses_api <- grepl("v1/responses", api_url_param) ||
                      isTRUE(mp$use_responses_api) ||
                      grepl(responses_only_models, config$model)

  if (is_responses_api) {
      if (grepl("v1/chat/completions/?$", api_url_param)) {
          endpoint <- sub("v1/chat/completions/?$", "v1/responses", api_url_param)
      } else if (nzchar(api_url_param)) {
          endpoint <- api_url_param
      } else {
          endpoint <- "https://api.openai.com/v1/responses"
      }
  } else {
      endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/chat/completions")
  }

  formatted_messages <- lapply(messages, function(msg) {
    if (msg$role != "user" || is.character(msg$content)) return(msg)
    if (is.list(msg$content)) {
      parts <- lapply(msg$content, function(part) {
        if (part$type == "text") {
          list(type = if (is_responses_api) "input_text" else "text", text = part$text)
        } else if (part$type == "file") {
          fd <- .process_file_content(part$path)
          list(type = if (is_responses_api) "input_image" else "image_url",
               image_url = if (is_responses_api) paste0("data:", fd$mime_type, ";base64,", fd$base64_data) else list(url = paste0("data:", fd$mime_type, ";base64,", fd$base64_data)))
        } else NULL
      })
      msg$content <- purrr::compact(parts)
    }
    msg
  })

  body0 <- list(model = config$model)

  if (is_responses_api) {
      sys_msgs <- purrr::keep(formatted_messages, function(x) identical(x$role, "system"))
      other_msgs <- purrr::keep(formatted_messages, function(x) !identical(x$role, "system"))

      instructions <- if (length(sys_msgs) > 0) {
        paste(vapply(sys_msgs, function(x) {
          if (is.list(x$content)) {
            paste(vapply(x$content, function(p) if (p$type %in% c("text", "input_text")) p$text else "", character(1)), collapse = " ")
          } else as.character(x$content)
        }, character(1)), collapse = "\n\n")
      } else NULL

      body0$input <- if (length(other_msgs) > 0) other_msgs else formatted_messages
      # With nothing but system messages, those messages ARE the input; adding
      # them as instructions too would send the same text twice.
      if (!is.null(instructions) && length(other_msgs) > 0) body0$instructions <- instructions
      body0$temperature <- mp$temperature
      body0$top_p <- mp$top_p
      # The Responses API rejects the chat-completions penalty and logprob
      # parameters; dropping beats a guaranteed 400.
      dropped <- intersect(names(mp), c("frequency_penalty", "presence_penalty",
                                        "logprobs", "top_logprobs", "seed"))
      if (length(dropped)) {
        .llmr_param_note(sprintf(
          "Dropped parameters not supported by the OpenAI Responses API: %s",
          paste(dropped, collapse = ", ")))
      }

      if (!is.null(mp$max_tokens)) body0$max_output_tokens <- mp$max_tokens
      if (!is.null(mp$max_completion_tokens)) body0$max_output_tokens <- mp$max_completion_tokens
      if (!is.null(mp$max_output_tokens)) body0$max_output_tokens <- mp$max_output_tokens
      # reasoning effort uses a nested shape here, unlike chat completions
      if (!is.null(mp$reasoning_effort)) body0$reasoning <- list(effort = mp$reasoning_effort)

  } else {
      # json_object mode (OpenAI sets this when a schema is absent) needs "json"
      # in the prompt; inject when absent (no-op for json_schema mode).
      formatted_messages      <- .ensure_json_object_instruction(formatted_messages, mp$response_format)
      body0$messages          <- formatted_messages
      body0$temperature       <- mp$temperature
      body0$top_p             <- mp$top_p
      body0$frequency_penalty <- mp$frequency_penalty
      body0$presence_penalty  <- mp$presence_penalty
      if (!is.null(mp$max_tokens)) body0$max_tokens <- mp$max_tokens
      # The o-series / gpt-5 family use max_completion_tokens; honor it when
      # supplied directly so those models work without a failed round trip.
      if (!is.null(mp$max_completion_tokens)) body0$max_completion_tokens <- mp$max_completion_tokens
  }

  body0 <- .drop_null(body0)

  if (!is.null(mp$response_format)) body0$response_format <- mp$response_format
  if (is.null(body0$response_format) && !is.null(mp$json_schema)) {
    body0$response_format <- list(type = "json_schema", json_schema = list(name = "llmr_schema", schema = mp$json_schema, strict = TRUE))
  }
  if (!is.null(mp$tools))       body0$tools       <- mp$tools
  if (!is.null(mp$tool_choice)) body0$tool_choice <- mp$tool_choice

  # MODIFIABILITY HOOK 3: Pass-through any extra configuration params verbatim
  skip_keys <- c("temperature", "top_p", "frequency_penalty", "presence_penalty",
                 "max_tokens", "max_output_tokens", "max_completion_tokens",
                 "response_format", "json_schema", "tools", "tool_choice",
                 "api_url", "use_responses_api", "request_modifier", "req_builder", "response_modifier",
                 "timeout", "cache")
  if (is_responses_api) {
    skip_keys <- c(skip_keys, "reasoning_effort", "logprobs", "top_logprobs", "seed")
  }
  extras <- mp[setdiff(names(mp), skip_keys)]
  if (length(extras) > 0) body0 <- c(body0, .drop_null(extras))

  # MODIFIABILITY HOOK 4: Allows arbitrary mutation of the entire JSON request before serialization
  if (is.function(mp$request_modifier)) {
      body0 <- mp$request_modifier(body0)
  }

  build_req <- function(bdy) {
    httr2::request(endpoint) |>
      httr2::req_headers(
        "Content-Type"  = "application/json",
        "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))
      ) |>
      httr2::req_body_json(bdy)
  }

  run <- function(bdy) {
    # Config is passed down so perform_request can trigger the remaining hooks
    perform_request(build_req(bdy), verbose, provider = config$provider, model = config$model, config = config)
  }

  res <- tryCatch(
    run(body0),
    llmr_api_param_error = function(e) {
      if (isTRUE(config$no_change)) stop(e)
      if (!is.null(e$param) && identical(e$param, "max_tokens") && !is.null(mp$max_tokens)) {
        b2 <- body0
        if (is_responses_api) {
            b2$max_output_tokens <- b2$max_tokens
        } else {
            b2$max_completion_tokens <- b2$max_tokens
        }
        b2$max_tokens <- NULL
        if (requireNamespace("cli", quietly = TRUE)) {
          cli::cli_alert_info(sprintf("Replaced `max_tokens` with `%s` for %s after server hint.",
                    if(is_responses_api) "max_output_tokens" else "max_completion_tokens", config$model))
        }
        res2 <- run(b2)
        return(res2)
      }
      stop(e)
    }
  )

  res
}


#' @export
call_llm.anthropic <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for Anthropic!")
  }
  req <- .anthropic_chat_request(config, messages)
  perform_request(req, verbose, provider = config$provider, model = config$model, config = config)
}

# Build (without performing) the Anthropic messages request. Used by
# call_llm.anthropic, call_llm_stream(), and the batch-API submitter.
.anthropic_chat_request <- function(config, messages, extra_body = list()) {
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "https://api.anthropic.com/v1/messages")

  # Separate system text and user/assistant messages
  formatted <- format_anthropic_messages(messages)

  # Build Anthropic message blocks:
  # - content MUST be an array of blocks; never send a bare string
  # - images are base64-inlined
  processed_user_messages <- lapply(formatted$user_messages, function(msg) {
    blocks <- if (is.character(msg$content)) {
      list(list(type = "text", text = msg$content))
    } else if (is.list(msg$content)) {
      purrr::compact(lapply(msg$content, function(part) {
        if (identical(part$type, "text")) {
          list(type = "text", text = part$text)
        } else if (identical(part$type, "file")) {
          fd <- .process_file_content(part$path)
          # PDFs are "document" blocks in the Anthropic API; only true images
          # may be sent as "image" blocks.
          list(
            type = if (identical(fd$mime_type, "application/pdf")) "document" else "image",
            source = list(
              type = "base64",
              media_type = fd$mime_type,
              data = fd$base64_data
            )
          )
        } else {
          # Other typed blocks (tool_use, tool_result, thinking, ...) pass
          # through verbatim; the tool-calling loop relies on this.
          part
        }
      }))
    } else {
      list(list(type = "text", text = as.character(msg$content)))
    }
    list(
      role = if (msg$role %in% c("user","assistant")) msg$role else "user",
      content = blocks
    )
  })

  # Translate canonical params but keep unknowns
  params <- .translate_params("anthropic", config$model_params,
                              auto_fix = !isTRUE(config$no_change))

  if (is.null(params$max_tokens))
    warning("Anthropic requires max_tokens; setting it at 2048.")

  # thinking is enabled when either:
  # - user sets thinking = TRUE (or "enabled"), or
  # - a budget_tokens is provided directly (or via thinking_budget alias)
  budget_tokens <- params$budget_tokens %||% params$thinking_budget
  thinking_enabled <- isTRUE(config$model_params$thinking) ||
    identical(tolower(as.character(config$model_params$thinking %||% "")), "enabled") ||
    !is.null(budget_tokens)

  max_tok <- params$max_tokens %||% 2048
  if (thinking_enabled && !is.null(budget_tokens) && max_tok <= budget_tokens) {
    warning(sprintf(
      "Anthropic requires max_tokens > thinking_budget; you supplied max_tokens=%s and thinking_budget=%s. The request will likely be rejected; raise max_tokens (it must cover thinking plus the visible reply).",
      max_tok, budget_tokens))
  }

  body <- .drop_null(list(
    model       = config$model,
    max_tokens  = max_tok,
    temperature = params$temperature,
    top_p       = params$top_p,
    top_k       = params$top_k,
    system      = formatted$system_text,                    # top-level system
    messages    = processed_user_messages,
    thinking    = if (thinking_enabled) .drop_null(list(
      type          = "enabled",
      budget_tokens = budget_tokens
    )) else NULL
  ))

  # Respect tools/tool_choice if already present (e.g., from enable_structured_output)
  mp <- config$model_params %||% list()
  if (!is.null(mp$tools))       body$tools       <- c(body$tools %||% list(), mp$tools)
  if (!is.null(mp$tool_choice)) body$tool_choice <- mp$tool_choice

  # Ensure tool names unique and tool_choice valid, unchanged from your version
  if (!is.null(body$tools) && length(body$tools) > 0) {
    nm <- vapply(body$tools, function(t) t$name %||% "", character(1))
    keep <- !duplicated(nm) | nm == ""
    body$tools <- body$tools[keep]
    if (!is.null(body$tool_choice) && identical(body$tool_choice$type, "tool")) {
      want <- body$tool_choice$name %||% ""
      have <- vapply(body$tools, function(t) identical(t$name, want), logical(1))
      if (!nzchar(want) || !any(have)) {
        first_named <- which(nzchar(vapply(body$tools, function(t) t$name %||% "", character(1))))[1]
        if (!is.na(first_named)) {
          body$tool_choice <- list(type = "tool", name = body$tools[[first_named]]$name)
        } else {
          body$tool_choice <- NULL
        }
      }
    }
  }

  # Prompt caching: mark the stable prefix (system prompt, tool definitions)
  # as cacheable so repeated calls with the same prefix are billed at the
  # provider's reduced cache-read rate.
  if (isTRUE(mp$cache)) {
    if (!is.null(body$system)) {
      body$system <- list(list(type = "text", text = body$system,
                               cache_control = list(type = "ephemeral")))
    }
    if (!is.null(body$tools) && length(body$tools) > 0) {
      body$tools[[length(body$tools)]]$cache_control <- list(type = "ephemeral")
    }
  }

  if (length(extra_body)) body <- utils::modifyList(body, extra_body)

  httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"      = "application/json",
      "x-api-key"         = .resolve_api_key(config$api_key, provider = config$provider, model = config$model),
      "anthropic-version" = "2023-06-01",
      !!! ( if (!is.null(mp$anthropic_beta)) list("anthropic-beta" = mp$anthropic_beta) else NULL )
    ) |>
    httr2::req_body_json(body)
}

# --- Gemini ------------------------------------------------------------------

.gemini_is_vertex <- function(config) {
  isTRUE(config$model_params$vertex)
}

.gemini_vertex_endpoint <- function(config, suffix = "generateContent") {
  mp <- config$model_params %||% list()
  project <- mp$project %||% mp$vertex_project
  if (is.null(project) || !nzchar(as.character(project)[1])) {
    stop("Vertex Gemini mode requires `project` in llm_config(...).")
  }

  location <- mp$location %||% mp$region %||% "us-central1"
  location <- as.character(location)[1]
  project <- as.character(project)[1]
  suffix <- sub("^:", "", as.character(suffix)[1])

  default_endpoint <- sprintf(
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:%s",
    location,
    project,
    location,
    config$model,
    suffix
  )
  get_endpoint(config, default_endpoint)
}

.gemini_developer_endpoint <- function(config, suffix = "generateContent") {
  suffix <- sub("^:", "", as.character(suffix)[1])
  get_endpoint(
    config,
    default_endpoint = paste0(
      "https://generativelanguage.googleapis.com/v1beta/models/",
      config$model,
      ":",
      suffix
    )
  )
}

.gemini_auth_headers <- function(config) {
  key <- .resolve_api_key(config$api_key, provider = config$provider, model = config$model)
  if (.gemini_is_vertex(config)) {
    return(list("Authorization" = paste("Bearer", key)))
  }
  list("x-goog-api-key" = key)
}

.gemini_embedding_params <- function(mp, body_names = character()) {
  extras <- .embedding_extras(mp, body_names)
  if ("task_type" %in% names(extras)) {
    extras$taskType <- extras$task_type
    extras$task_type <- NULL
  }
  if ("output_dimensionality" %in% names(extras)) {
    extras$outputDimensionality <- extras$output_dimensionality
    extras$output_dimensionality <- NULL
  }
  if ("auto_truncate" %in% names(extras)) {
    extras$autoTruncate <- extras$auto_truncate
    extras$auto_truncate <- NULL
  }
  extras
}

#' @export
call_llm.gemini <- function(config, messages, verbose = FALSE) {

  if (.is_embedding_config(config)) {
    return(call_llm.gemini_embedding(config, messages, verbose))
  }

  req <- .gemini_chat_request(config, messages)
  perform_request(req, verbose, provider = config$provider, model = config$model, config = config)
}

# Build (without performing) the Gemini generateContent request. Used by
# call_llm.gemini, call_llm_stream() (with the streaming endpoint suffix), and
# the batch-API submitter (which reads req$body$data).
.gemini_chat_request <- function(config, messages, suffix = "generateContent",
                                 query = NULL) {
  messages  <- .normalize_messages(messages)
  endpoint  <- if (.gemini_is_vertex(config)) {
    .gemini_vertex_endpoint(config, suffix)
  } else {
    .gemini_developer_endpoint(config, suffix)
  }

  # Canonical -> Gemini names (camelCase inside generationConfig). With
  # no_change = TRUE nothing is renamed, so the builder below also reads the
  # canonical spellings as fallbacks rather than discarding them.
  params <- .translate_params("gemini", config$model_params, auto_fix = !isTRUE(config$no_change))

  # System instruction (omit role; REST samples do this)
  sys_msgs   <- purrr::keep(messages, ~ .x$role == "system")
  other_msgs <- purrr::keep(messages, ~ .x$role != "system")
  systemInstruction <- if (length(sys_msgs) > 0L) {
    list(parts = list(list(text = paste(vapply(sys_msgs, `[[`, "", "content"), collapse = " "))))
  } else NULL

  # Contents: send only parts (no role); inlineData camelCase per REST
  formatted_messages <- lapply(other_msgs, function(msg) {
    parts <- if (is.character(msg$content)) {
      list(list(text = msg$content))
    } else if (is.list(msg$content)) {
      purrr::compact(lapply(msg$content, function(part) {
        if (part$type == "text") {
          list(text = part$text)
        } else if (part$type == "file") {
          fd <- .process_file_content(part$path)
          list(inlineData = list(
            mimeType = fd$mime_type,
            data     = fd$base64_data
          ))
        } else NULL
      }))
    } else {
      list(list(text = as.character(msg$content)))
    }
    list(role = if (identical(msg$role, "assistant")) "model" else "user",
         parts = parts)
  })

  resp_mime <- params$responseMimeType %||% config$model_params$response_mime_type

  # Logprobs: Gemini wants responseLogprobs = TRUE plus an integer `logprobs`
  # count. After translation, canonical `logprobs` is responseLogprobs and
  # canonical `top_logprobs` is the count; asking for a count implies the flag.
  resp_logprobs <- params$responseLogprobs
  n_logprobs    <- if (!is.null(params$responseLogprobs) || !is.null(params$logprobs)) params$logprobs else NULL
  if (!is.null(n_logprobs) && is.null(resp_logprobs)) resp_logprobs <- TRUE

  thinking_cfg <- .drop_null(list(
    thinkingBudget  = params$thinkingBudget  %||% params$thinking_budget,
    includeThoughts = params$includeThoughts %||% params$include_thoughts
  ))

  gen_cfg <- .drop_null(list(
    temperature      = params$temperature,
    maxOutputTokens  = params$maxOutputTokens %||% params$max_tokens,
    topP             = params$topP %||% params$top_p,
    topK             = params$topK %||% params$top_k,
    seed             = params$seed,
    presencePenalty  = params$presencePenalty  %||% params$presence_penalty,
    frequencyPenalty = params$frequencyPenalty %||% params$frequency_penalty,
    responseLogprobs = resp_logprobs,
    logprobs         = n_logprobs,
    thinkingConfig   = if (length(thinking_cfg)) thinking_cfg else NULL,
    responseMimeType = resp_mime %||% "text/plain"
  ))

  if (!is.null(config$model_params$response_schema)) {
    gen_cfg$responseSchema <- config$model_params$response_schema
  }
  if (!is.null(config$model_params$response_json_schema)) {
    # Standard JSON Schema variant (Gemini 2.5+); what
    # enable_structured_output() sets.
    gen_cfg$responseJsonSchema <- config$model_params$response_json_schema
  }

  body <- .drop_null(list(
    contents          = formatted_messages,
    generationConfig  = gen_cfg,
    systemInstruction = systemInstruction
  ))

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"   = "application/json",
      !!!.gemini_auth_headers(config)
    ) |>
    httr2::req_body_json(body)
  if (!is.null(query)) req <- httr2::req_url_query(req, !!!query)
  req
}

# ---- Shared OpenAI-compatible chat handler ---------------------------------
# groq/together/deepseek/xiaomi/alibaba/zhipu/moonshot/xai/ollama all speak the
# OpenAI chat-completions dialect. One builder keeps their feature set
# identical: multimodal file parts, the full canonical parameter set,
# structured output, tools, verbatim extras passthrough, and the
# modifiability hooks. Per-provider differences reduce to an endpoint, an
# auth header style, and a short list of parameters the provider rejects.
.openai_compat_chat <- function(config, messages, verbose, endpoint,
                                auth_style = c("bearer", "api-key", "none"),
                                drop_params = character()) {
  req <- .compat_chat_request(config, messages, endpoint,
                              auth_style = auth_style, drop_params = drop_params)
  perform_request(req, verbose, provider = config$provider, model = config$model, config = config)
}

# Build (without performing) the OpenAI-compatible chat request. Used by the
# provider methods above, by call_llm_stream() (with extra_body = stream
# flags), and by the batch-API submitters (which read req$body$data).
.compat_chat_request <- function(config, messages, endpoint,
                                 auth_style = c("bearer", "api-key", "none"),
                                 drop_params = character(),
                                 extra_body = list()) {
  auth_style <- match.arg(auth_style)
  messages <- .normalize_messages(messages)
  mp <- config$model_params %||% list()

  # Multimodal: inline local files as data URLs, the shape every
  # OpenAI-compatible vision endpoint understands. Without this conversion a
  # raw {type:"file", path:...} part would be serialized to the provider.
  formatted_messages <- lapply(messages, function(msg) {
    if (msg$role != "user" || is.character(msg$content)) return(msg)
    if (is.list(msg$content)) {
      parts <- lapply(msg$content, function(part) {
        if (part$type == "text") {
          list(type = "text", text = part$text)
        } else if (part$type == "file") {
          fd <- .process_file_content(part$path)
          list(type = "image_url",
               image_url = list(url = paste0("data:", fd$mime_type, ";base64,", fd$base64_data)))
        } else NULL
      })
      msg$content <- purrr::compact(parts)
    }
    msg
  })

  if (length(drop_params) && !isTRUE(config$no_change)) {
    bad <- intersect(names(mp), drop_params)
    if (length(bad)) {
      .llmr_param_note(sprintf("Dropped unsupported parameters for %s: %s",
                               config$provider, paste(bad, collapse = ", ")))
      mp <- mp[setdiff(names(mp), bad)]
    }
  }

  # json_object mode on DeepSeek/Alibaba/Zhipu/Moonshot/Xiaomi needs "json" in
  # the prompt; inject a minimal instruction when absent (no-op otherwise).
  formatted_messages <- .ensure_json_object_instruction(formatted_messages, mp$response_format)

  body0 <- list(model = config$model, messages = formatted_messages)
  body0$temperature        <- mp$temperature
  body0$top_p              <- mp$top_p
  body0$top_k              <- mp$top_k
  body0$frequency_penalty  <- mp$frequency_penalty
  body0$presence_penalty   <- mp$presence_penalty
  body0$repetition_penalty <- mp$repetition_penalty
  body0$seed               <- mp$seed
  body0$logprobs           <- mp$logprobs
  body0$top_logprobs       <- mp$top_logprobs
  if (!is.null(mp$max_tokens)) body0$max_tokens <- mp$max_tokens
  if (!is.null(mp$max_completion_tokens)) body0$max_completion_tokens <- mp$max_completion_tokens
  body0 <- .drop_null(body0)

  # Structured outputs (OpenAI-compatible)
  if (!is.null(mp$response_format)) body0$response_format <- mp$response_format
  if (is.null(body0$response_format) && !is.null(mp$json_schema)) {
    body0$response_format <- list(
      type = "json_schema",
      json_schema = list(name = "llmr_schema", schema = mp$json_schema, strict = TRUE)
    )
  }
  if (!is.null(mp$tools))       body0$tools       <- mp$tools
  if (!is.null(mp$tool_choice)) body0$tool_choice <- mp$tool_choice

  # Pass-through any extra configuration params verbatim, so provider-specific
  # features (e.g., Qwen's enable_thinking, Moonshot's partial mode) work
  # without an LLMR release.
  skip_keys <- c("temperature", "top_p", "top_k", "frequency_penalty", "presence_penalty",
                 "repetition_penalty", "seed", "logprobs", "top_logprobs",
                 "max_tokens", "max_output_tokens", "max_completion_tokens",
                 "response_format", "json_schema", "tools", "tool_choice",
                 "api_url", "use_responses_api", "request_modifier", "req_builder",
                 "response_modifier", "timeout", "cache")
  extras <- mp[setdiff(names(mp), skip_keys)]
  if (length(extras) > 0) body0 <- c(body0, .drop_null(extras))

  # MODIFIABILITY HOOK: arbitrary mutation of the JSON body before serialization
  if (is.function(mp$request_modifier)) body0 <- mp$request_modifier(body0)

  if (length(extra_body)) body0 <- utils::modifyList(body0, extra_body)

  headers <- list("Content-Type" = "application/json")
  if (auth_style == "bearer") {
    headers[["Authorization"]] <- paste("Bearer",
      .resolve_api_key(config$api_key, provider = config$provider, model = config$model))
  } else if (auth_style == "api-key") {
    headers[["api-key"]] <- .resolve_api_key(config$api_key, provider = config$provider, model = config$model)
  }

  httr2::request(endpoint) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(body0)
}

#' @export
call_llm.groq <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for Groq!")
  }
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://api.groq.com/openai/v1/chat/completions"),
    drop_params = c("top_k", "repetition_penalty")
  )
}

#' @export
call_llm.together <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    return(call_llm.together_embedding(config, messages, verbose))
  }
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/chat/completions")
  )
}

#' @export
call_llm.deepseek <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for DeepSeek!")
  }
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://api.deepseek.com/chat/completions"),
    drop_params = c("top_k", "repetition_penalty")
  )
}

#' @export
call_llm.xiaomi <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for Xiaomi MiMo!")
  }
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://api.xiaomimimo.com/v1/chat/completions"),
    auth_style = "api-key",
    drop_params = c("top_k", "repetition_penalty")
  )
}

#' @export
call_llm.alibaba <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for Alibaba (DashScope)!")
  }
  # Default to the international DashScope endpoint. China-region accounts should
  # pass api_url = "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions".
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions"),
    drop_params = c("repetition_penalty")
  )
}

#' @export
call_llm.zhipu <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for Zhipu!")
  }
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://open.bigmodel.cn/api/paas/v4/chat/completions"),
    drop_params = c("frequency_penalty", "presence_penalty", "repetition_penalty")
  )
}

#' @export
call_llm.moonshot <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for Moonshot!")
  }
  # Default to the international Moonshot endpoint. China-platform accounts should
  # pass api_url = "https://api.moonshot.cn/v1/chat/completions".
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://api.moonshot.ai/v1/chat/completions"),
    drop_params = c("top_k", "repetition_penalty")
  )
}


#' @export
call_llm.xai <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    stop("Embedding models are not currently supported for xai!")
  }
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "https://api.x.ai/v1/chat/completions"),
    drop_params = c("top_k", "repetition_penalty")
  )
}



#' @export
#' @rdname call_llm
#' @section Using a local Ollama server:
#' Ollama provides an OpenAI-compatible HTTP API on localhost by default. Start the
#' daemon and pull a model first (terminal): `ollama serve` (in background) and
#' `ollama pull llama3`. Then configure LLMR with
#' `llm_config("ollama", "llama3", embedding = FALSE)` for chat or
#' `llm_config("ollama", "nomic-embed-text", embedding = TRUE)` for embeddings.
#' Override the endpoint with `api_url` if not using the default
#' `http://localhost:11434/v1/*`.
call_llm.ollama <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    return(call_llm.ollama_embedding(config, messages, verbose))
  }
  .openai_compat_chat(
    config, messages, verbose,
    endpoint = get_endpoint(config, default_endpoint = "http://localhost:11434/v1/chat/completions"),
    auth_style = "none"
  )
}

#' @export
#' @keywords internal
call_llm.ollama_embedding <- function(config, messages, verbose = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "http://localhost:11434/v1/embeddings")
  texts <- if (is.character(messages)) messages else vapply(messages, `[[`, "", "content")
  body <- list(model = config$model, input = texts)
  body <- c(body, .embedding_extras(config$model_params, names(body)))

  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(body)

  perform_request(req, verbose, provider = config$provider, model = config$model, config = config)
}

# ----- Embedding-specific Handlers -----

# Internal: extras safe to forward to an embeddings endpoint. Generation-only
# parameters (sampling, penalties, reasoning) and LLMR-internal keys are
# removed, so reusing a chat config for embeddings cannot poison the request
# body with fields the endpoint rejects.
.embedding_extras <- function(mp, body_names = character()) {
  mp <- mp %||% list()
  skip <- c(body_names,
            "temperature", "max_tokens", "max_completion_tokens", "max_output_tokens",
            "top_p", "top_k", "frequency_penalty", "presence_penalty",
            "repetition_penalty", "seed", "logprobs", "top_logprobs",
            "thinking_budget", "include_thoughts", "thinking", "reasoning_effort",
            "response_format", "json_schema", "response_schema", "tools", "tool_choice",
            "api_url", "use_responses_api", "request_modifier", "req_builder",
            "response_modifier", "timeout", "cache", "anthropic_beta",
            "vertex", "project", "vertex_project", "location", "region")
  .drop_null(mp[setdiff(names(mp), skip)])
}

#' @export
#' @keywords internal
call_llm.openai_embedding <- function(config, messages, verbose = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(model = config$model, input = texts)
  body <- c(body, .embedding_extras(config$model_params, names(body)))

  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, provider = config$provider, model = config$model, config = config)
}

#' @export
call_llm.voyage <- function(config, messages, verbose = FALSE) {
  # Voyage is embeddings-only in this implementation
  return(call_llm.voyage_embedding(config, messages, verbose))
}

#' @export
#' @keywords internal
call_llm.voyage_embedding <- function(config, messages, verbose = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.voyageai.com/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(input = texts, model = config$model)
  body <- c(body, .embedding_extras(config$model_params, names(body)))
  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, provider = config$provider, model = config$model, config = config)
}

#' @export
#' @keywords internal
call_llm.together_embedding <- function(config, messages, verbose = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(model = config$model, input = texts)
  body <- c(body, .embedding_extras(config$model_params, names(body)))

  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, provider = config$provider, model = config$model, config = config)
}

#' @export
#' @keywords internal
call_llm.gemini_embedding <- function(config, messages,
                                      verbose = FALSE) {

  # 1. pull the raw strings ---------------------------------------------------
  texts <- if (is.character(messages)) messages else
    vapply(messages, \(m)
           if (is.list(m) && !is.null(m$content))
             m$content else as.character(m),
           character(1))

  # 2. Vertex AI: one embedContent request per text (no batch endpoint on this
  # path). Requests go through perform_request() so errors carry the usual
  # typed classes and retry metadata.
  if (.gemini_is_vertex(config)) {
    endpoint <- .gemini_vertex_endpoint(config, "embedContent")
    out <- lapply(texts, function(txt) {
      body <- list(content = list(parts = list(list(text = txt))))
      extras <- .gemini_embedding_params(config$model_params, names(body))
      body   <- .drop_null(c(body, extras))
      dat <- httr2::request(endpoint) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          !!!.gemini_auth_headers(config)
        ) |>
        httr2::req_body_json(body) |>
        perform_request(verbose, provider = config$provider, model = config$model, config = config)
      list(embedding = dat$embedding$values)
    })
    return(list(data = out))
  }

  # 3. Developer API: batchEmbedContents embeds up to 100 texts per HTTP call,
  # instead of one round trip per text.
  endpoint <- .gemini_developer_endpoint(config, "batchEmbedContents")
  extras <- .gemini_embedding_params(config$model_params, c("model", "content"))
  chunks <- split(texts, ceiling(seq_along(texts) / 100))
  out <- list()
  for (ch in chunks) {
    requests <- lapply(ch, function(txt) {
      .drop_null(c(
        list(model   = paste0("models/", config$model),
             content = list(parts = list(list(text = txt)))),
        extras
      ))
    })
    dat <- httr2::request(endpoint) |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        !!!.gemini_auth_headers(config)
      ) |>
      httr2::req_body_json(list(requests = requests)) |>
      perform_request(verbose, provider = config$provider, model = config$model, config = config)
    out <- c(out, lapply(dat$embeddings, function(e) list(embedding = e$values)))
  }

  # LLMR-style return ---------------------------------------------------------
  list(data = out)
}



# ----- Embedding Utility Functions -----

#' Parse Embedding Response into a Numeric Matrix
#'
#' Converts the embedding response data to a numeric matrix.
#'
#' @param embedding_response The response returned from an embedding API call.
#'
#' @return A numeric matrix of embeddings with column names as sequence numbers.
#' @export
#'
#' @examples
#' \dontrun{
#'   text_input <- c("Political science is a useful subject",
#'                   "We love sociology",
#'                   "German elections are different",
#'                   "A student was always curious.")
#'
#'   # Configure the embedding API provider (example with Voyage API).
#'   # The key is read from the VOYAGE_API_KEY environment variable.
#'   voyage_config <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-3.5-lite"
#'   )
#'
#'   embedding_response <- call_llm(voyage_config, text_input)
#'   embeddings <- parse_embeddings(embedding_response)
#'   # Additional processing:
#'   embeddings |> cor() |> print()
#' }
parse_embeddings <- function(embedding_response) {
  if (is.null(embedding_response$data) || length(embedding_response$data) == 0)
    return(matrix(nrow = 0, ncol = 0))

  list_of_vectors <- purrr::map(embedding_response$data, ~ {
    if (is.list(.x) && !is.null(.x$embedding) && !all(is.na(.x$embedding))) {
      as.numeric(.x$embedding)
    } else {
      NA_real_ # This will be treated as a vector of length 1 by list_transpose if not handled
    }
  })

  first_valid_vector <- purrr::detect(list_of_vectors, ~!all(is.na(.x)))
  true_embedding_dim <- if (!is.null(first_valid_vector)) length(first_valid_vector) else 0

  processed_list_of_vectors <- purrr::map(list_of_vectors, ~ {
    if (length(.x) == 1 && all(is.na(.x))) { # Was a placeholder for a failed embedding
      if (true_embedding_dim > 0) rep(NA_real_, true_embedding_dim) else NA_real_ # vector of NAs
    } else if (length(.x) == true_embedding_dim) {
      .x # Already correct
    } else {
      # This case should ideally not happen if API is consistent or errors are NA_real_
      if (true_embedding_dim > 0) rep(NA_real_, true_embedding_dim) else NA_real_
    }
  })

  if (true_embedding_dim == 0 && length(processed_list_of_vectors) > 0) {
    # All embeddings failed, and we couldn't determine dimension.
    # Return a matrix of NAs with rows = num_texts_in_batch, cols = 1 (placeholder)
    # get_batched_embeddings will later reconcile this with first_emb_dim if known from other batches.
    return(matrix(NA_real_, nrow = length(processed_list_of_vectors), ncol = 1))
  }
  if (length(processed_list_of_vectors) == 0) { # No data to process
    return(matrix(nrow = 0, ncol = 0))
  }

  embeddings_matrix <- processed_list_of_vectors |>
    purrr::list_transpose() |>
    as.data.frame() |>
    as.matrix()

  return(embeddings_matrix)
}

#' Bind tools to a config (provider-agnostic)
#'
#' @param config llm_config
#' @param tools list of tools (each with name, description, and parameters/input_schema)
#' @param tool_choice optional tool_choice spec (provider-specific shape)
#' @return modified llm_config
#' @export
bind_tools <- function(config, tools, tool_choice = NULL) {
  stopifnot(inherits(config, "llm_config"))
  mp <- config$model_params %||% list()
  mp$tools <- append(mp$tools %||% list(), tools)
  if (!is.null(tool_choice)) mp$tool_choice <- tool_choice
  config$model_params <- mp
  config
}








#' Generate Embeddings in Batches
#'
#' A wrapper function that processes a list of texts in batches to generate embeddings,
#' avoiding rate limits. This function calls \code{\link{call_llm_robust}} for each
#' batch and stitches the results together and parses them (using `parse_embeddings`) to
#' return a numeric matrix.
#'
#' @param texts Character vector of texts to embed. If named, the names will be
#'   used as row names in the output matrix.
#' @param embed_config An \code{llm_config} object configured for embeddings.
#' @param batch_size Integer. Number of texts to process in each batch. Default is 50.
#'   (Gemini's developer API embeds at most 100 texts per request; larger
#'   batches are split automatically.)
#' @param verbose Logical. If TRUE, prints progress messages. Default is FALSE.
#' @param tries,wait_seconds,backoff_factor Retry controls forwarded to
#'   \code{\link{call_llm_robust}} for each batch.
#'
#' @return A numeric matrix where each row is an embedding vector for the corresponding text.
#'   Columns are named \code{v1}, \code{v2}, ..., \code{vK} where K is the embedding dimension.
#'   If embedding fails for certain texts, those rows will be filled with NA values.
#'   The matrix will always have the same number of rows as the input texts.
#'   Returns NULL if no embeddings were successfully generated.
#'
#' @inheritSection llm_fn Batching, chunking, and row packing
#' @seealso
#' \code{\link{llm_config}} to create the embedding configuration.
#' \code{\link{parse_embeddings}} to convert the raw response to a numeric matrix.
#' Here `batch_size` is embedding chunking, the synchronous sense; it is not the
#' asynchronous provider Batch API (\code{\link{llm_batch_submit}}) nor the
#' generative row packing of \code{\link{llm_mutate}} (`.rows_per_prompt`).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   texts <- c("Hello world", "How are you?", "Machine learning is great")
#'   names(texts) <- c("greeting", "question", "statement")
#'
#'   # The key is read from the VOYAGE_API_KEY environment variable.
#'   embed_cfg <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-3.5-lite",
#'     embedding = TRUE
#'   )
#'
#'   embeddings <- get_batched_embeddings(
#'     texts = texts,
#'     embed_config = embed_cfg,
#'     batch_size = 2
#'   )
#' }
get_batched_embeddings <- function(texts,
                                   embed_config,
                                   batch_size = 50,
                                   verbose = FALSE,
                                   tries = 5,
                                   wait_seconds = 2,
                                   backoff_factor = 3) {

  # Input validation
  if (!is.character(texts) || length(texts) == 0) {
    if (verbose) message("No texts provided. Returning NULL.")
    return(NULL)
  }
  if (!inherits(embed_config, "llm_config")) {
    stop("embed_config must be a valid llm_config object.")
  }

  # Setup
  n_docs <- length(texts)
  batches <- split(seq_len(n_docs), ceiling(seq_len(n_docs) / batch_size))
  emb_list <- vector("list", n_docs)
  first_emb_dim <- NULL

  if (verbose) {
    message("Processing ", n_docs, " texts in ", length(batches), " batches of up to ", batch_size, " texts each")
  }

  # Process batches
  for (b in seq_along(batches)) {
    idx <- batches[[b]]
    batch_texts <- texts[idx]

    if (verbose) {
      message("Processing batch ", b, "/", length(batches), " (texts ", min(idx), "-", max(idx), ")")
    }

    tryCatch({
      # Call LLM for this batch using the robust caller
      resp <- call_llm_robust(embed_config, batch_texts, verbose = FALSE,
                              tries = tries, wait_seconds = wait_seconds,
                              backoff_factor = backoff_factor)
      emb_chunk <- parse_embeddings(resp)

      # A degenerate chunk (provider returned empty data, or fewer rows than
      # requested) must not lock in a wrong dimension; route it to the NA path.
      if (is.null(emb_chunk) || ncol(emb_chunk) == 0L || nrow(emb_chunk) < length(idx)) {
        stop("degenerate embedding chunk for this batch")
      }

      # Capture the embedding dimension only from a genuine, non-empty chunk, so
      # a later real batch can still establish it if early batches degenerate.
      if (is.null(first_emb_dim) && ncol(emb_chunk) > 0L && !all(is.na(emb_chunk))) {
        first_emb_dim <- ncol(emb_chunk)
      }

      # Store per-document embeddings
      for (i in seq_along(idx)) {
        emb_list[[idx[i]]] <- emb_chunk[i, ]
      }

    }, error = function(e) {
      if (verbose) {
        message("Error in batch ", b, ": ", conditionMessage(e))
        message("Skipping batch and continuing...")
      }
      # Store NA for failed batch
      for (i in idx) {
        emb_list[[i]] <- NA
      }
    })
  }

  # Determine the dimension of the embeddings from the first successful result
  if (is.null(first_emb_dim)) {
    # Find the first non-NA element to determine dimensionality
    successful_emb <- purrr::detect(emb_list, ~ !all(is.na(.x)))
    if (!is.null(successful_emb)) {
      first_emb_dim <- length(successful_emb)
    } else {
      if (verbose) message("No embeddings were successfully generated.")
      return(NULL)
    }
  }

  # Replace NA placeholders with vectors of NAs of the correct dimension
  emb_list <- lapply(emb_list, function(emb) {
    if (length(emb) == 1 && is.na(emb)) {
      return(rep(NA_real_, first_emb_dim))
    }
    return(emb)
  })

  # Combine all embeddings into final matrix
  final_embeddings <- do.call(rbind, emb_list)

  if (!is.null(names(texts))) {
    rownames(final_embeddings) <- names(texts)
  }

  # Always assign stable column names v1..vK for downstream consistency
  colnames(final_embeddings) <- paste0("v", seq_len(ncol(final_embeddings)))

  if (verbose) {
    n_successful <- sum(stats::complete.cases(final_embeddings))
    message("Successfully generated embeddings for ", n_successful,
            "/", n_docs, " texts (", ncol(final_embeddings), " dimensions)")
  }

  return(final_embeddings)
}
