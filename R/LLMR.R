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

# ----- Internal Helper Functions -----

#' Normalise message inputs  (LLMR.R)
#'
#' Called once in `call_llm()` (not in embedding mode).
#' Rules, in order:
#'   1. Already‑well‑formed list → returned untouched.
#'   2. Plain character vector   → each element becomes a `"user"` turn.
#'   3. Named char vector **without** "file" → names are roles (legacy path).
#'   4. Named char vector **with**  "file"  → multimodal shortcut:
#'        - any `system` entries become separate system turns
#'        - consecutive {user | file} entries combine into one user turn
#'        - every `file` path is tilde‑expanded
#'
#' @keywords internal
#' @noRd
.normalize_messages <- function(messages) {

  ## ── 1 · leave proper message objects unchanged ───────────────────────
  if (is.list(messages) &&
      length(messages)        > 0L &&
      is.list(messages[[1]])  &&
      !is.null(messages[[1]]$role) &&
      !is.null(messages[[1]]$content)) {
    return(messages)
  }

  ## ── 2 · character vectors --------------------------------------------
  if (is.character(messages)) {
    msg_names <- names(messages)

    ### 2a · *unnamed* → each element a user turn
    if (is.null(msg_names)) {
      return(lapply(messages,
                    function(txt) list(role = "user", content = txt)))
    }

    ### 2b · *named* but no "file" → legacy path
    if (!"file" %in% msg_names) {
      return(unname(purrr::imap(messages,
                                \(txt, role) list(role = role, content = txt))))
    }

    ### 2c · multimodal shortcut ----------------------------------------
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
                                           text = messages[j])))
            has_text <- TRUE
          } else {  # msg_names[j] == "file"
            user_parts <- append(user_parts,
                                 list(list(type = "file",
                                           path = path.expand(messages[j]))))
          }
          j <- j + 1L
        }
        if (!has_text)
          stop("A 'file' part must be preceded by at least one 'user' text.")

        final_messages <- append(final_messages,
                                 list(list(role = "user",
                                           content = purrr::compact(user_parts))))
        i <- j                                           # advance
      } else {                                           # system / assistant
        final_messages <- append(final_messages,
                                 list(list(role = role,
                                           content = messages[i])))
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
perform_request <- function(req, verbose, provider = NULL, model = NULL) {
  start_time <- Sys.time()

  # Let us inspect 4xx/5xx bodies instead of httr2 throwing first
  req <- httr2::req_error(req, is_error = \(resp) FALSE)
  resp <- httr2::req_perform(req)
  code <- httr2::resp_status(resp)

  if (code >= 400) {
    err <- try(httr2::resp_body_json(resp), silent = TRUE)
    category <- if (code >= 500) "server" else if (code == 429) "rate_limit"
    else if (code == 401) "auth" else if (code == 400) "param" else "unknown"
    bad_param <- if (!inherits(err, "try-error")) err$error$param %||% NA_character_ else NA_character_
    msg_lines <- c(
      "LLM API request failed.",
      paste0("HTTP status: ", code),
      paste0("Reason: ",
             if (inherits(err, "try-error")) "No message supplied"
             else err$error$message %||% "No message supplied"),
      "Tip: check model params for provider/API version."
    )
    .llmr_error(
      message     = paste(msg_lines, collapse = "\n"),
      category    = category,
      status_code = code,
      param       = bad_param,
      code        = if (!inherits(err, "try-error")) err$error$code %||% NA_character_ else NA_character_,
      request_id  = httr2::resp_header(resp, "x-request-id") %||%
        httr2::resp_header(resp, "request-id")
    )
  }

  raw_response <- httr2::resp_body_raw(resp)
  raw_json     <- rawToChar(raw_response)
  content      <- httr2::resp_body_json(resp)

  if (verbose) {
    cat("Full API Response:\n")
    print(content)
  }

  # Detect embeddings and return list for embeddings
  is_embedding_like <- is.list(content) && (!is.null(content$data) || !is.null(content$embedding))
  if (is_embedding_like) {
    # Return embedding response (list) directly
    return(content)
  }

  # Generative: build an llmr_response
  txt <- extract_text(content)
  fr  <- .std_finish_reason(content)

  # reuse your existing provider-agnostic token counter
  tc  <- .token_counts(content)
  rt  <- .reasoning_tokens_from(content)

  usage <- list(sent = as.integer(tc$sent %||% NA_integer_),
                rec  = as.integer(tc$rec  %||% NA_integer_),
                total = as.integer((tc$sent %||% 0L) + (tc$rec %||% 0L)),
                reasoning = as.integer(rt %||% NA_integer_))

  out <- new_llmr_response(
    text         = txt %||% NA_character_,
    provider     = provider %||% NA_character_,
    model        = model %||% NA_character_,
    finish_reason= fr,
    usage        = usage,
    response_id  = .response_id_from(content),
    duration_s   = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    raw          = content,
    raw_json     = raw_json
  )

  # Back-compat attributes expected elsewhere
  attr(out, "full_response") <- content
  attr(out, "raw_json")      <- raw_json

  out
}



#' Extract Text from API Response
#'
#' Internal helper function to extract text from the API response content.
#'
#' @keywords internal
#' @noRd
extract_text <- function(content) {

  # Embeddings
  if (is.list(content) && (!is.null(content$data) || !is.null(content$embedding))) {
    return(content)
  }

  # OpenAI / Groq / Together
  if (!is.null(content$choices)) {
    if (length(content$choices) == 0) return(NA_character_)
    ch <- content$choices[[1]]
    if (!is.null(ch$message) && !is.null(ch$message$content)) return(ch$message$content)
    if (!is.null(ch$text)) return(ch$text)
    return(NA_character_)
  }

  # Anthropic
  if (!is.null(content$content)) {
    cc <- content$content
    if (length(cc) == 0) return(NA_character_)

    # Prefer tool_use when structured mode is on (unchanged)
    is_tool_use <- function(b) identical(b$type, "tool_use")
    has_tool <- any(vapply(cc, is_tool_use, logical(1)))
    if (has_tool) {
      idx <- which(vapply(cc, is_tool_use, logical(1)))[1]
      tu  <- cc[[idx]]
      if (!is.null(tu$input)) {
        if (is.character(tu$input) && length(tu$input) == 1L) {
          return(tu$input)
        } else {
          return(jsonlite::toJSON(tu$input, auto_unbox = TRUE, null = "null"))
        }
      }
    }

    # pick the last text block, skipping the thinking block
    text_blocks <- Filter(function(b) !is.null(b$text) && is.character(b$text) && nzchar(b$text[1]), cc)
    if (length(text_blocks) > 0) {
      return(text_blocks[[length(text_blocks)]]$text)
    }

    return(NA_character_)
  }




  # Gemini (REST)
  if (!is.null(content$candidates)) {
    if (length(content$candidates) == 0) return(NA_character_)
    cand <- content$candidates[[1]]

    # 1) Direct character payload
    if (!is.null(cand$content) && is.character(cand$content)) {
      out <- paste(cand$content[nzchar(cand$content)], collapse = "\n")
      return(if (nzchar(out)) out else NA_character_)
    }

    # Helper: parts resolver for both shapes
    get_parts <- function(obj) {
      if (is.null(obj)) return(NULL)
      if (is.list(obj) && !is.null(obj$parts)) return(obj$parts)
      if (is.list(obj) && length(obj) >= 1L && is.list(obj[[1]]) && !is.null(obj[[1]]$parts)) {
        return(obj[[1]]$parts)
      }
      NULL
    }

    # 2) Standard: content.parts[*].text
    parts <- get_parts(cand$content)
    if (!is.null(parts) && length(parts) > 0) {
      texts <- vapply(parts, function(p) if (!is.null(p$text) && is.character(p$text)) p$text else "", character(1))
      out <- paste(texts[nzchar(texts)], collapse = "\n")
      if (nzchar(out)) return(out)
    }

    # 3) Fallbacks
    if (!is.null(cand$text) && is.character(cand$text) && nzchar(cand$text)) return(cand$text)

    # 4) Strict deep fallback: search only text-like keys; skip 'role'
    allowed_keys <- c("text", "outputText", "output_text", "candidateText")
    find_text <- function(x) {
      if (is.list(x)) {
        nm <- names(x)
        if (!is.null(nm) && length(nm)) {
          # prefer named text-like keys
          for (k in allowed_keys) {
            if (k %in% nm && is.character(x[[k]]) && nzchar(x[[k]][1])) return(x[[k]][1])
          }
          # recurse other fields, explicitly skip 'role'
          for (nm_i in nm) {
            if (identical(nm_i, "role")) next
            val <- find_text(x[[nm_i]])
            if (!is.null(val) && nzchar(val)) return(val)
          }
        } else {
          # unnamed list
          for (el in x) {
            val <- find_text(el)
            if (!is.null(val) && nzchar(val)) return(val)
          }
        }
      }
      # do NOT return bare character scalars here (avoids grabbing 'model')
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
#' @param provider Character scalar. One of:
#'   `"openai"`, `"anthropic"`, `"gemini"`, `"groq"`, `"together"`,
#'   `"voyage"` (embeddings only), `"deepseek"`, `"xai"`, `"ollama"`.
#' @param model Character scalar. Model name understood by the chosen provider.
#'   (e.g., `"gpt-4o-mini"`, `"o4-mini"`, `"claude-3.7"`, `"gemini-2.0-flash"`, etc.)
#' @param api_key Character scalar. Provider API key.
#' @param troubleshooting Logical. If `TRUE`, prints the full request payloads
#'   (including your API key!) for debugging. **Use with extreme caution.**
#' @param base_url Optional character. Back-compat alias; if supplied it is
#'   stored as `api_url` in `model_params` and overrides the default endpoint.
#' @param embedding `NULL` (default), `TRUE`, or `FALSE`. If `TRUE`, the call
#'   is routed to the provider's embeddings API; if `FALSE`, to the chat API.
#'   If `NULL`, LLMR infers embeddings when `model` contains `"embedding"`.
#' @param no_change Logical. If `TRUE`, LLMR **never** auto-renames/adjusts
#'   provider parameters. If `FALSE` (default), well-known compatibility shims
#'   may apply (e.g., renaming OpenAI's `max_tokens` → `max_completion_tokens`
#'   after a server hint; see `call_llm()` notes).
#' @param ... Additional provider-specific parameters (e.g., `temperature`,
#'   `top_p`, `max_tokens`, `top_k`, `repetition_penalty`, `reasoning_effort`,
#'   `api_url`, etc.). Values are forwarded verbatim unless documented shims apply.
#'   For Anthropic extended thinking, supply `thinking_budget` (canonical;
#'   mapped to `thinking.budget_tokens`) together with `include_thoughts = TRUE`
#'   to request the thinking block in the response.
#'
#' @return An object of class `c("llm_config", provider)`. Fields:
#'   `provider`, `model`, `api_key`, `troubleshooting`, `embedding`,
#'   `no_change`, and `model_params` (a named list of extras).
#'
#' @section Temperature range clamping:
#' Anthropic temperatures must be in `[0, 1]`; others in `[0, 2]`. Out-of-range
#' values are clamped with a warning.
#'
#' @section Endpoint overrides:
#' You can pass `api_url` (or `base_url=` alias) in `...` to point to gateways
#' or compatible proxies.
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
#' cfg <- llm_config("openai", "gpt-4o-mini",
#' temperature = 0.7, max_tokens = 300)
#'
#' # Generative call returns an llmr_response object
#' r <- call_llm(cfg, "Say hello in Greek.")
#' print(r)
#' as.character(r)
#'
#' # Embeddings (inferred from the model name)
#' e_cfg <- llm_config("gemini", "text-embedding-004")
#'
#' # Force embeddings even if model name does not contain "embedding"
#' e_cfg2 <- llm_config("voyage", "voyage-large-2", embedding = TRUE)
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
  if (missing(api_key) || is.null(api_key)) {
    api_key_handle <- llm_api_key_env(.default_api_key_env(provider))
  } else if (inherits(api_key, "llmr_secret")) {
    api_key_handle <- api_key
  } else if (is.character(api_key) && length(api_key) == 1L) {
    if (grepl("^env:", api_key)) {
      api_key_handle <- llm_api_key_env(sub("^env:", "", api_key))
    } else if (grepl("^[A-Z][A-Z0-9_]*$", api_key) && nzchar(Sys.getenv(api_key, unset = ""))) {
      api_key_handle <- llm_api_key_env(api_key)
    } else {
      # A literal token was supplied. Move it into a temporary env var and keep only its name.
      rand <- paste(sample(c(LETTERS, 0:9), 8, TRUE), collapse = "")
      env_name <- paste0("LLMR_", toupper(provider), "_KEY_", rand)
      # Sys.setenv(structure(api_key, names = env_name)) ### not right
      do.call(Sys.setenv, setNames(list(api_key), env_name))
      api_key_handle <- llm_api_key_env(env_name)
      if (requireNamespace("cli", quietly = TRUE)) {
        # cli::cli_alert_warning(paste0(
        #   "A literal API key was supplied to llm_config(). ",
        #   "For security, it was moved to a temporary environment variable '{", env_name, "}'. ",
        #   "Prefer defining '", .default_api_key_env(provider), "' in your .Renviron."
        # ))
        warning(sprintf(
          "A literal API key was supplied. Moved to temporary env var '%s'. Prefer '%s' in ~/.Renviron.",
          env_name, .default_api_key_env(provider)
        ))

      } else {
        warning(paste0(
          "A literal API key was supplied. It was moved to temporary env var '", env_name,
          "'. Prefer using ", .default_api_key_env(provider), " in your .Renviron."
        ))
      }
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



 

#' Call an LLM (chat/completions or embeddings) with optional multimodal input
#'
#' `call_llm()` dispatches to the correct provider implementation based on
#' `config$provider`. It supports both generative chat/completions and
#' embeddings, plus a simple multimodal shortcut for local files.
#'
#' @param config An \code{\link{llm_config}} object.
#' @param messages One of:
#'   \itemize{
#'     \item Plain character vector — each element becomes a `"user"` message.
#'     \item Named character vector — names are roles (`"system"`, `"user"`,
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
#'         `2048` and warns. Multimodal images are inlined as base64. Extended
#'         thinking is supported: provide `thinking_budget` and
#'         `include_thoughts = TRUE` to include a `content` block of type
#'         `"thinking"` in the response; LLMR sets the beta header automatically.
#'   \item \strong{Gemini (REST):} `systemInstruction` is supported; user
#'         parts use `text`/`inlineData(mimeType,data)`; responses are set to
#'         `responseMimeType = "text/plain"`.
#'   \item \strong{Ollama (local):} OpenAI-compatible endpoints on `http://localhost:11434/v1/*`;
#'         no Authorization header is required. Override with `api_url` as needed.
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
#' cfg <- llm_config("openai", "gpt-4o-mini")
#' call_llm(cfg, "Say hello in Greek.")
#'
#' ## 2) Generative with rich return
#' r <- call_llm(cfg, "Say hello in Greek.")
#' r
#' as.character(r)
#' finish_reason(r); tokens(r)
#'
#' ## 3) Anthropic extended thinking (single example)
#' a_cfg <- llm_config("anthropic", "claude-sonnet-4-20250514",
#'                     max_tokens = 5000,
#'                     thinking_budget = 16000,
#'                     include_thoughts = TRUE)
#' r2 <- call_llm(a_cfg, "Compute 87*93 in your head. Give only the final number.")
#' # thinking (if present): r2$raw$content[[1]]$thinking
#' # final text:            r2$raw$content[[2]]$text
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
#' e_cfg <- llm_config("voyage", "voyage-large-2",
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
  if (config$troubleshooting == TRUE) {
    print("\n\n Inside call_llm for troubleshooting\n")
    print(messages)
    print(.mask_config_for_print(config))
    print("\n\n")
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
  if (isTRUE(config$embedding)) {
    return(call_llm.openai_embedding(config, messages, verbose))
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/chat/completions")

  # Format messages with multimodal support (inline base64 images)
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

  body0 <- .drop_null(list(
    model             = config$model,
    messages          = formatted_messages,
    temperature       = config$model_params$temperature,
    top_p             = config$model_params$top_p,
    frequency_penalty = config$model_params$frequency_penalty,
    presence_penalty  = config$model_params$presence_penalty
  ))
  if (!is.null(config$model_params$max_tokens)) {
    body0$max_tokens <- config$model_params$max_tokens
  }

  # Structured outputs (OpenAI-compatible). Pass response_format/tools/tool_choice when present.
  mp <- config$model_params %||% list()
  if (!is.null(mp$response_format)) body0$response_format <- mp$response_format
  if (is.null(body0$response_format) && !is.null(mp$json_schema)) {
    body0$response_format <- list(
      type = "json_schema",
      json_schema = list(
        name   = "llmr_schema",
        schema = mp$json_schema,
        strict = TRUE
      )
    )
  }
  if (!is.null(mp$tools))       body0$tools       <- mp$tools
  if (!is.null(mp$tool_choice)) body0$tool_choice <- mp$tool_choice

  build_req <- function(bdy) {
    httr2::request(endpoint) |>
      httr2::req_headers(
        "Content-Type"  = "application/json",
        "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))
      ) |>
      httr2::req_body_json(bdy)
  }

  last_body <- NULL  # used for potential retries and debugging

  run <- function(bdy) {
    last_body <<- bdy
    perform_request(build_req(bdy), verbose,
                    provider = config$provider, model = config$model)
  }


  # 1) First attempt; on 400/param(max_tokens) and no_change=FALSE, retry with max_completion_tokens
  res <- tryCatch(
    run(body0),
    llmr_api_param_error = function(e) {
      if (isTRUE(config$no_change)) stop(e)
      if (!is.null(e$param) && identical(e$param, "max_tokens") &&
          !is.null(config$model_params$max_tokens)) {
        b2 <- body0
        b2$max_completion_tokens <- b2$max_tokens
        b2$max_tokens <- NULL
        if (requireNamespace("cli", quietly = TRUE)) {
          cli::cli_alert_info(
            sprintf("Replaced `max_tokens` with `max_completion_tokens` for %s after server hint.", config$model)
          )
        }
        res2 <- run(b2)
        last_body <<- b2
        return(res2)
      }
      stop(e)
    }
  )
  ### THE following was experimental but it may cause unwanted costs, so it is disabled
  # 2) If the server returned empty string after the fix, do a final retry without any cap
  # if (!isTRUE(config$no_change) &&
  #     is.character(res) && !nzchar(trimws(res)) &&
  #     ( !is.null(last_body$max_completion_tokens) || !is.null(last_body$max_tokens) )) {
  #   b3 <- last_body
  #   b3$max_completion_tokens <- NULL
  #   b3$max_tokens <- NULL
  #   if (requireNamespace("cli", quietly = TRUE)) {
  #     cli::cli_alert_info("Empty content returned; retrying once without a completion cap.")
  #   }
  #   res <- run(b3)
  # }

  res
}


#' @export
call_llm.anthropic <- function(config, messages, verbose = FALSE) {
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for Anthropic!")
  }
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
          list(
            type = "image",
            source = list(
              type = "base64",
              media_type = fd$mime_type,
              data = fd$base64_data
            )
          )
        } else {
          NULL
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
  params <- .translate_params("anthropic", config$model_params)

  if (is.null(params$max_tokens))
    warning("Anthropic requires max_tokens; setting it at 2048.")

  # thinking is enabled when either:
  # - user sets thinking = TRUE (or "enabled"), or
  # - a budget_tokens is provided directly (or via thinking_budget alias)
  thinking_enabled <- isTRUE(config$model_params$thinking) ||
    identical(tolower(as.character(config$model_params$thinking %||% "")), "enabled") ||
    !is.null(params$budget_tokens)

  body <- .drop_null(list(
    model       = config$model,
    max_tokens  = params$max_tokens %||% 2048,
    temperature = params$temperature,
    top_p       = params$top_p,
    system      = formatted$system_text,                    # top-level system
    messages    = processed_user_messages,
    thinking    = if (thinking_enabled) .drop_null(list(
      type          = "enabled",
      budget_tokens = params$budget_tokens
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

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"      = "application/json",
      "x-api-key"         = .resolve_api_key(config$api_key, provider = config$provider, model = config$model),
      "anthropic-version" = "2023-06-01",
      !!! ( if (!is.null(mp$anthropic_beta)) list("anthropic-beta" = mp$anthropic_beta) else NULL )
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, provider = config$provider, model = config$model)
}

# --- Gemini ------------------------------------------------------------------

#' @export
call_llm.gemini <- function(config, messages, verbose = FALSE) {

  if (isTRUE(config$embedding) ||
      grepl("embedding", config$model, ignore.case = TRUE)) {
    return(call_llm.gemini_embedding(config, messages, verbose))
  }

  messages  <- .normalize_messages(messages)
  endpoint  <- get_endpoint(
    config,
    default_endpoint = paste0(
      "https://generativelanguage.googleapis.com/v1beta/models/",
      config$model,
      ":generateContent")
  )

  # Canonical -> Gemini names (camelCase inside generationConfig)
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
    list(role = "user", parts = parts)
  })

  resp_mime <- params$responseMimeType %||% config$model_params$response_mime_type
  gen_cfg <- .drop_null(list(
    temperature     = params$temperature,
    maxOutputTokens = params$maxOutputTokens,
    topP            = params$topP,
    topK            = params$topK,
    responseMimeType= resp_mime %||% "text/plain"
  ))

  body <- .drop_null(list(
    contents          = formatted_messages,
    generationConfig  = gen_cfg,
    systemInstruction = systemInstruction
  ))

  # Optional schema (Gemini supports JSON inline schemas)
  if (!is.null(config$model_params$response_schema)) {
    body$responseSchema <- config$model_params$response_schema
  }

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"   = "application/json",
      "x-goog-api-key" = .resolve_api_key(config$api_key, provider = config$provider, model = config$model)
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, provider = config$provider, model = config$model)
}




# call_llm.gemini <- function(config, messages, verbose = FALSE, json = FALSE) {
#   if (isTRUE(config$embedding) || grepl("embedding", config$model, ignore.case = TRUE)) {
#     return(call_llm.gemini_embedding(config, messages, verbose, json))
#   }
#   messages <- .normalize_messages(messages)
#   endpoint <- get_endpoint(config, default_endpoint = paste0("https://generativelanguage.googleapis.com/v1beta/models/", config$model, ":generateContent"))
#
#   ## convert canonical names ---> Gemini native
#   params <- .translate_params("gemini", config$model_params)
#
#   system_messages <- purrr::keep(messages, ~ .x$role == "system")
#   other_messages <- purrr::keep(messages, ~ .x$role != "system")
#   system_instruction <- if (length(system_messages) > 0) {
#     list(parts = list(list(text = paste(sapply(system_messages, function(x) x$content), collapse = " "))))
#   } else {
#     NULL
#   }
#
#   formatted_messages <- lapply(other_messages, function(msg) {
#     role <- if (msg$role == "assistant") "model" else "user"
#     content_parts <- list()
#     if (is.character(msg$content)) {
#       content_parts <- list(list(text = msg$content))
#     } else if (is.list(msg$content)) {
#       content_parts <- lapply(msg$content, function(part) {
#         if (part$type == "text") {
#           return(list(text = part$text))
#         } else if (part$type == "file") {
#           file_data <- .process_file_content(part$path)
#           return(list(inlineData = list(mimeType = file_data$mime_type, data = file_data$base64_data)))
#         } else {
#           return(NULL)
#         }
#       })
#       content_parts <- purrr::compact(content_parts)
#     }
#     list(role = role, parts = content_parts)
#   })
#
#   body <- .drop_null(list(
#     contents = formatted_messages,
#     generationConfig = .drop_null(list(
#       temperature     = params$temperature,
#       maxOutputTokens = params$maxOutputTokens,
#       topP            = params$topP,
#       topK            = params$topK
#     )),
#     generationConfig = .drop_null(list(
#       temperature       = params$temperature,
#       maxOutputTokens   = params$maxOutputTokens,
#       topP              = params$topP,
#       topK              = params$topK,
#       responseMimeType  = "text/plain",
#       thinkingConfig    = if (!is.null(params$thinkingBudget) ||
#                               !is.null(params$includeThoughts))
#         .drop_null(list(
#           thinkingBudget = params$thinkingBudget,
#           includeThoughts= isTRUE(params$includeThoughts)))
#     )),
#     # thinkingConfig = if (!is.null(params$thinkingBudget) ||
#     #                      !is.null(params$includeThoughts))
#     #   .drop_null(list(
#     #     budgetTokens   = params$thinkingBudget,
#     #     includeThoughts= isTRUE(params$includeThoughts)))
#   ))
#
#
#   if (!is.null(system_instruction))
#     body$systemInstruction <- system_instruction
#
#   req <- httr2::request(endpoint) |>
#     httr2::req_headers(
#       "Content-Type" = "application/json",
#       "x-goog-api-key" = config$api_key
#     ) |>
#     httr2::req_body_json(body)
#
#   perform_request(req, verbose, json)
# }

#' @export
call_llm.groq <- function(config, messages, verbose = FALSE) {
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for Groq!")
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "https://api.groq.com/openai/v1/chat/completions")

  body <- .drop_null(list(
    model      = config$model,
    messages   = messages,
    temperature= config$model_params$temperature,
    max_tokens = config$model_params$max_tokens
  ))

  # Structured outputs (OpenAI-compatible; Groq is OpenAI API compatible)
  if (!is.null(config$model_params$response_format)) {
    body$response_format <- config$model_params$response_format
  } else if (!is.null(config$model_params$json_schema)) {
    body$response_format <- list(
      type = "json_schema",
      json_schema = list(name="llmr_schema", schema = config$model_params$json_schema, strict = TRUE)
    )
  }


  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))
    ) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, provider = config$provider, model = config$model)
}

#' @export
call_llm.together <- function(config, messages, verbose = FALSE) {
  if (isTRUE(config$embedding)) {
    return(call_llm.together_embedding(config, messages, verbose))
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/chat/completions")

  body <- .drop_null(list(
    model              = config$model,
    messages           = messages,
    max_tokens         = config$model_params$max_tokens,
    temperature        = config$model_params$temperature,
    top_p              = config$model_params$top_p,
    top_k              = config$model_params$top_k,
    repetition_penalty = config$model_params$repetition_penalty
  ))

  # Structured outputs (OpenAI-compatible)
  if (!is.null(config$model_params$response_format)) {
    body$response_format <- config$model_params$response_format
  } else if (!is.null(config$model_params$json_schema)) {
    body$response_format <- list(
      type = "json_schema",
      json_schema = list(name="llmr_schema", schema = config$model_params$json_schema, strict = TRUE)
    )
  }

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))
    ) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, provider = config$provider, model = config$model)
}

#' @export
call_llm.deepseek <- function(config, messages, verbose = FALSE) {
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for DeepSeek!")
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "https://api.deepseek.com/chat/completions")

  body <- .drop_null(list(
    model      = config$model %||% "deepseek-chat",
    messages   = messages,
    temperature= config$model_params$temperature,
    max_tokens = config$model_params$max_tokens,
    top_p      = config$model_params$top_p
  ))

  # Structured outputs (OpenAI-compatible)
  if (!is.null(config$model_params$response_format)) {
    body$response_format <- config$model_params$response_format
  } else if (!is.null(config$model_params$json_schema)) {
    body$response_format <- list(
      type = "json_schema",
      json_schema = list(name="llmr_schema", schema = config$model_params$json_schema, strict = TRUE)
    )
  }

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))
    ) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, provider = config$provider, model = config$model)
}

#' @export
call_llm.xai <- function(config, messages, verbose = FALSE) {
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for xai!")
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(
    config,
    default_endpoint = "https://api.x.ai/v1/chat/completions"
  )

  body <- .drop_null(list(
    model       = config$model,
    messages    = messages,
    temperature = config$model_params$temperature,
    max_tokens  = config$model_params$max_tokens,
    top_p       = config$model_params$top_p,
    stream      = FALSE
  ))

  # Structured outputs (OpenAI-compatible)
  if (!is.null(config$model_params$response_format)) {
    body$response_format <- config$model_params$response_format
  } else if (!is.null(config$model_params$json_schema)) {
    body$response_format <- list(
      type = "json_schema",
      json_schema = list(name="llmr_schema", schema = config$model_params$json_schema, strict = TRUE)
    )
  }

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"  = "application/json",
      "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, provider = config$provider, model = config$model)
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
  if (isTRUE(config$embedding) ||
      grepl("embedding", config$model, ignore.case = TRUE)) {
    return(call_llm.ollama_embedding(config, messages, verbose))
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "http://localhost:11434/v1/chat/completions")

  body <- .drop_null(list(
    model       = config$model,
    messages    = messages,
    temperature = config$model_params$temperature,
    top_p       = config$model_params$top_p,
    max_tokens  = config$model_params$max_tokens
  ))

  # Structured outputs (OpenAI-compatible)
  mp <- config$model_params %||% list()
  if (!is.null(mp$response_format)) body$response_format <- mp$response_format
  if (is.null(body$response_format) && !is.null(mp$json_schema)) {
    body$response_format <- list(
      type = "json_schema",
      json_schema = list(name = "llmr_schema", schema = mp$json_schema, strict = TRUE)
    )
  }
  if (!is.null(mp$tools))       body$tools       <- mp$tools
  if (!is.null(mp$tool_choice)) body$tool_choice <- mp$tool_choice

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, provider = config$provider, model = config$model)
}

#' @export
#' @keywords internal
call_llm.ollama_embedding <- function(config, messages, verbose = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "http://localhost:11434/v1/embeddings")
  texts <- if (is.character(messages)) messages else vapply(messages, `[[`, "", "content")
  body <- list(model = config$model, input = texts)

  extras <- config$model_params[setdiff(names(config$model_params), names(body))]
  body   <- .drop_null(c(body, extras))

  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(body)

  perform_request(req, verbose, provider = config$provider, model = config$model)
}

# ----- Embedding-specific Handlers -----

#' @export
#' @keywords internal
call_llm.openai_embedding <- function(config, messages, verbose = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(model = config$model, input = texts)

  ## allowing extra parameters to be sent to the api
  extras <- config$model_params[setdiff(names(config$model_params), names(body))]
  body   <- .drop_null(c(body, extras))

  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))) |>
    httr2::req_body_json(body)
  perform_request(req, verbose)
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

  ## allowing extra parameters to be sent to the api
  extras <- config$model_params[setdiff(names(config$model_params), names(body))]
  body   <- .drop_null(c(body, extras))
  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))) |>
    httr2::req_body_json(body)
  perform_request(req, verbose)
}

#' @export
#' @keywords internal
call_llm.together_embedding <- function(config, messages, verbose = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(model = config$model, input = texts)

  ## allowing extra parameters to be sent to the api
  extras <- config$model_params[setdiff(names(config$model_params), names(body))]
  body   <- .drop_null(c(body, extras))

  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", .resolve_api_key(config$api_key, provider = config$provider, model = config$model))) |>
    httr2::req_body_json(body)
  perform_request(req, verbose)
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

  # 2. endpoint ---------------------------------------------------------------
  endpoint <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:embedContent",
    config$model)

  # 3. loop -------------------------------------------------------------------
  out <- lapply(texts, function(txt) {

    body <- list(
      model   = paste0("models/", config$model),      # mandatory
      content = list(parts = list(list(text = txt)))  # exactly one text
    )

    ## allowing extra parameters to be sent to the api
    extras <- config$model_params[setdiff(names(config$model_params), names(body))]
    body   <- .drop_null(c(body, extras))

    resp <- httr2::request(endpoint) |>
      httr2::req_headers(
        "Content-Type"   = "application/json",
        "x-goog-api-key" = .resolve_api_key(config$api_key, provider = config$provider, model = config$model)
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_perform()

    dat <- httr2::resp_body_json(resp)
    list(embedding = dat$embedding$values)
  })

  # LLMR‑style return ---------------------------------------------------------
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
#'   # Configure the embedding API provider (example with Voyage API)
#'   voyage_config <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-large-2",
#'     api_key = Sys.getenv("VOYAGE_API_KEY")
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
  valid_embeddings_data <- purrr::keep(embedding_response$data, ~is.list(.x) && !is.null(.x$embedding) && !all(is.na(.x$embedding)))

  if (length(valid_embeddings_data) == 0)
    NULL # nothing to do, keep going


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
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#'
#' @return A numeric matrix where each row is an embedding vector for the corresponding text.
#'   Columns are named \code{v1}, \code{v2}, ..., \code{vK} where K is the embedding dimension.
#'   If embedding fails for certain texts, those rows will be filled with NA values.
#'   The matrix will always have the same number of rows as the input texts.
#'   Returns NULL if no embeddings were successfully generated.
#'
#' @seealso
#' \code{\link{llm_config}} to create the embedding configuration.
#' \code{\link{parse_embeddings}} to convert the raw response to a numeric matrix.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   texts <- c("Hello world", "How are you?", "Machine learning is great")
#'   names(texts) <- c("greeting", "question", "statement")
#'
#'   embed_cfg <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-large-2-instruct",
#'     embedding = TRUE,
#'     api_key = Sys.getenv("VOYAGE_API_KEY")
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
                                   verbose = FALSE) {

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
      resp <- call_llm_robust(embed_config, batch_texts, verbose = FALSE)
      emb_chunk <- parse_embeddings(resp)

      if (is.null(first_emb_dim)) {
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
