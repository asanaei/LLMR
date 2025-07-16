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
#'        • any `system` entries become separate system turns
#'        • consecutive {user | file} entries combine into one user turn
#'        • every `file` path is tilde‑expanded
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
#' @importFrom httr2 req_perform resp_body_raw resp_body_json
perform_request <- function(req, verbose, json) {
  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) >= 400) {
    stop(rawToChar(httr2::resp_body_raw(resp)), call. = FALSE)
  }

  # Get the raw response as text
  raw_response <- httr2::resp_body_raw(resp)
  raw_json <- rawToChar(raw_response)
  # Parse the response as JSON
  content <- httr2::resp_body_json(resp)

  if (verbose) {
    cat("Full API Response:\n")
    print(content)
  }

  text <- extract_text(content)
  attr(text, "full_response") <- content
  attr(text, "raw_json") <- raw_json

  if (json) {
    return(text)
  }

  # By default, for non-json output, just return the text
  return(as.character(text))
}

#' Extract Text from API Response
#'
#' Internal helper function to extract text from the API response content.
#'
#' @keywords internal
#' @noRd


extract_text <- function(content) {
  # Handle embeddings FIRST with more flexible logic
  if (is.list(content) && (!is.null(content$data) || !is.null(content$embedding))) {
    return(content)
  }

  if (!is.null(content$choices)) {
    # For APIs like OpenAI, Groq, Together AI
    if (length(content$choices) == 0 || is.null(content$choices[[1]]$message$content)) {
      return(NA_character_)
    }
    return(content$choices[[1]]$message$content)
  }

  if (!is.null(content$content)) {
    # For Anthropic
    if (length(content$content) == 0 || is.null(content$content[[1]]$text)) {
      return(NA_character_)
    }
    return(content$content[[1]]$text)
  }

  if (!is.null(content$candidates)) {        # Gemini
    if (length(content$candidates) == 0 ||
        is.null(content$candidates[[1]]$content) ||
        is.null(content$candidates[[1]]$content$parts) ||
        length(content$candidates[[1]]$content$parts) == 0) {
      return(NA_character_)
    }
    cand <- content$candidates[[1]]
    parts <- cand$content$parts
    is_thought <- vapply(parts, function(p) isTRUE(p$thought), logical(1))
    answer_idx <- which(!is_thought)
    if (length(answer_idx) == 0) answer_idx <- 1
    answer_text  <- parts[[answer_idx[1]]]$text
    if (is.null(answer_text)) return(NA_character_)
    thoughts_txt <- paste(vapply(parts[is_thought], `[[`, "", "text"), collapse = "\n\n")
    attr(answer_text, "thoughts") <- thoughts_txt
    return(answer_text)
  }

  return(NA_character_)
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

#' Create LLM Configuration
#'
#' @param provider Provider name (openai, anthropic, groq, together, voyage, gemini, deepseek)
#' @param model Model name to use
#' @param api_key API key for authentication
#' @param troubleshooting Prints out all api calls. USE WITH EXTREME CAUTION as it prints your API key.
#' @param base_url Optional base URL override
#' @param embedding Logical indicating embedding mode: NULL (default, uses prior defaults), TRUE (force embeddings), FALSE (force generative)
#' @param ... Additional provider-specific parameters
#' @return Configuration object for use with call_llm()
#' @seealso
#' The main ways to use a config object:
#' \itemize{
#'   \item \code{\link{call_llm}} for a basic, single API call.
#'   \item \code{\link{call_llm_robust}} for a more reliable single call with retries.
#'   \item \code{\link{chat_session}} for creating an interactive, stateful conversation.
#'   \item \code{\link{llm_fn}} for applying a prompt to a vector or data frame.
#'   \item \code{\link{call_llm_par}} for running large-scale, parallel experiments.
#'   \item \code{\link{get_batched_embeddings}} for generating text embeddings.
#' }
#' @export
#' @examples
#' \dontrun{
#'   cfg <- llm_config(
#'     provider   = "openai",
#'     model      = "gpt-4o-mini",
#'     api_key    = Sys.getenv("OPENAI_API_KEY"),
#'     temperature = 0.7,
#'     max_tokens  = 500)
#'
#'   call_llm(cfg, "Hello!")  # one-shot, bare string
#' }
llm_config <- function(provider, model, api_key, troubleshooting = FALSE, base_url = NULL, embedding = NULL, ...) {
  model_params <- list(...)
  ##clamp temperature to valid range
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
        warning(paste0("Temperature must be between 0 and 2; setting it at: ",temp))
      }
    }
    model_params$temperature <- temp
  }
  ## end clamp

  # Handle base_url passed via ... for backward compatibility, renaming to api_url internally
  if (!is.null(base_url)) {
    model_params$api_url <- base_url
  }
  config <- list(
    provider = provider,
    model = model,
    api_key = api_key,
    troubleshooting = troubleshooting,
    embedding = embedding,
    model_params = model_params
  )
  class(config) <- c("llm_config", provider)
  return(config)
}



#' Call LLM API
#'
#' Send text or multimodal messages to a supported Large-Language-Model (LLM)
#' service and retrieve either a **chat/completion** response or a set of
#' **embeddings**.
#'
#' ## Generative vs. embedding mode
#' * **Generative calls** (the default) go to the provider's chat/completions
#'   endpoint. Any extra model-specific parameters supplied through `...` in
#'   [`llm_config()`] (for example `temperature`, `top_p`, `max_tokens`) are
#'   forwarded verbatim to the request body. For reasoning, some provider have
#'   shared model names and ask for a config argument that indicates
#'   reasoning should be enabled, others have dedicated model names for
#'   reasoning-enabled models, and may or may not allow for another argument that
#'   indicates the effort level.
#' * **Embedding calls** are triggered when `config$embedding` is `TRUE` or
#'   the model name contains the string "embedding". These calls are routed
#'   to the provider's embedding endpoint and return raw embedding data. At
#'   present, extra parameters are *not* passed through to embedding
#'   endpoints.
#'
#' ## Messages argument
#' `messages` can be
#' * a plain character vector (each element becomes a **user** message),
#' * a **named** character vector whose names are interpreted as roles, or
#' * a list of explicit message objects (`list(role = ..., content = ...)`).
#'
#' For multimodal requests you may either use the classic list-of-parts **or**
#' (simpler) pass a named character vector where any element whose name is
#' `"file"` is treated as a local file path and uploaded with the request (see
#' Example 3 below).
#'
#' @param config   An [`llm_config`] object created by `llm_config()`.
#' @param messages Character vector, named vector, or list of message objects as
#'   described above.
#' @param verbose  Logical; if `TRUE`, prints the full API response.
#' @param json     Logical; if `TRUE`, returns the raw JSON with attributes.
#'
#' @return
#' * **Generative mode:** a character string (assistant reply). When
#'   `json = TRUE`, the string has attributes `raw_json` (JSON text) and
#'   `full_response` (parsed list). call_llm supports reasning models as well, but whether the output of these models
#'    include the text of the reasning or not depends on the provider.
#' * **Embedding mode:** a list with element `data`, compatible with
#'   [`parse_embeddings()`].
#'
#' @seealso
#' \code{\link{llm_config}} to create the configuration object.
#' \code{\link{call_llm_robust}} for a version with automatic retries for rate limits.
#' \code{\link{call_llm_par}} helper that loops over texts and stitches
#'   embedding results into a matrix.
#'
#' @examples
#' \dontrun{
#' ## 1. Generative call ------------------------------------------------------
#' cfg <- llm_config("openai", "gpt-4o-mini", Sys.getenv("OPENAI_API_KEY"))
#' call_llm(cfg, "Hello!")
#'
#' ## 2. Embedding call -------------------------------------------------------
#' embed_cfg <- llm_config(
#'   provider  = "gemini",
#'   model     = "gemini-embedding-001",
#'   api_key   = Sys.getenv("GEMINI_KEY"),
#'   embedding = TRUE
#' )
#' emb <- call_llm(embed_cfg, "This is a test sentence.")
#' parse_embeddings(emb)
#'
#' ## 3. Multimodal call (named-vector shortcut) -----------------------------
#' cfg <- llm_config(
#'   provider = "openai",
#'   model    = "gpt-4.1-mini",
#'   api_key  = Sys.getenv("OPENAI_API_KEY")
#' )
#'
#' msg <- c(
#'   system = "you answer in rhymes",
#'   user   = "interpret this. Is there a joke here?",
#'   file   = "path/to/local_image.png")
#'
#'
#' call_llm(cfg, msg)
#'
#' ## 4. Reasoning example
#' cfg_reason <- llm_config("openai",
#'                          "o4-mini",
#'                          Sys.getenv("OPENAI_API_KEY"),
#'                          reasoning_effort = 'low')
#' call_llm(cfg_reason,
#'             c(system='Only give an integer number. Nothing else',
#'             user='Count "s" letters in Mississippi'))
#' }
#' @export
call_llm <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (config$troubleshooting == TRUE){
    print("\n\n Inside call_llm for troubleshooting\n")
    print("\n****\nBE CAREFUL THIS BIT CONTAINS YOUR API KEY! DO NOT REPORT IT AS IS!\n****\n")
    print(messages)
    print(config)
    print("\n\n")
  }

  UseMethod("call_llm", config)
}

#' @export
call_llm.default <- function(config, messages, verbose = FALSE, json = FALSE) {
  # This default is mapped to the OpenAI-compatible endpoint structure
  message("Provider-specific function not present, defaulting to OpenAI format.")
  call_llm.openai(config, messages, verbose, json)
}

#' @export
call_llm.openai <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (isTRUE(config$embedding)) {
    return(call_llm.openai_embedding(config, messages, verbose, json))
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/chat/completions")

  # Format messages with multimodal support
  formatted_messages <- lapply(messages, function(msg) {
    if (msg$role != "user" || is.character(msg$content)) {
      return(msg)
    }

    if (is.list(msg$content)) {
      content_parts <- lapply(msg$content, function(part) {
        if (part$type == "text") {
          return(list(type = "text", text = part$text))
        } else if (part$type == "file") {
          file_data <- .process_file_content(part$path)
          data_uri <- paste0("data:", file_data$mime_type, ";base64,", file_data$base64_data)
          return(list(type = "image_url", image_url = list(url = data_uri)))
        } else {
          return(NULL)
        }
      })
      msg$content <- purrr::compact(content_parts)
    }
    return(msg)
  })

  body <- .drop_null(list(
    model             = config$model,
    messages          = formatted_messages,
    temperature       = config$model_params$temperature,
    max_tokens        = config$model_params$max_tokens,
    top_p             = config$model_params$top_p,
    frequency_penalty = config$model_params$frequency_penalty,
    presence_penalty  = config$model_params$presence_penalty
  ))

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}

#' @export
call_llm.anthropic <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for Anthropic!")
  }
  messages <- .normalize_messages(messages)
  endpoint <- get_endpoint(config, default_endpoint = "https://api.anthropic.com/v1/messages")

  use_thinking_beta <- !is.null(config$model_params$thinking_budget) ||
    isTRUE(config$model_params$include_thoughts)

  # Use the helper to separate system messages
  formatted <- format_anthropic_messages(messages)

  # Process user messages for multimodal content
  processed_user_messages <- lapply(formatted$user_messages, function(msg) {
    # ── KEEP STRING CONTENT AS-IS ─────────────────────────────
    if (is.character(msg$content)) {
      return(list(role = msg$role, content = msg$content))
    }

    # ── OTHERWISE (images / tools) BUILD BLOCKS ───────────────
    content_blocks <- lapply(msg$content, function(part) {
      if (part$type == "text")
        list(type = "text", text = part$text)
      else if (part$type == "file") {
        fd <- .process_file_content(part$path)
        list(type = "image",
             source = list(type = "base64",
                           media_type = fd$mime_type,
                           data = fd$base64_data))
      } else NULL
    })
    list(role = msg$role, content = content_blocks |> purrr::compact())
  })

  ### translate & pull out Anthropic-specific aliases
  params <- .translate_params("anthropic", config$model_params)

  if (is.null(params$max_tokens))
    warning('Anthropic requires max_tokens; setting it at 2048.')



  body <- .drop_null(list(
    model      = config$model,
    max_tokens = params$max_tokens %||% 2048,
    temperature= params$temperature,
    top_p      = params$top_p,
    messages   = processed_user_messages,
    thinking   = if (!is.null(params$thinking_budget) &&
                     !is.null(params$include_thoughts))
      list(
        type          = "enabled",
        budget_tokens = params$thinking_budget
      )
  ))

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"      = "application/json",
      "x-api-key"         = config$api_key,
      "anthropic-version" = "2023-06-01",
      !!! (
        if (!is.null(body$thinking))
          list("anthropic-beta" = "extended-thinking-2025-05-14")
      )
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}


# --- Gemini ------------------------------------------------------------------

#' @export
call_llm.gemini <- function(config, messages, verbose = FALSE, json = FALSE) {

  ## ---- 1. Routing to embedding --------------------------------------------
  if (isTRUE(config$embedding) ||
      grepl("embedding", config$model, ignore.case = TRUE)) {
    return(call_llm.gemini_embedding(config, messages, verbose, json))
  }

  ## ---- 2. Prep -------------------------------------------------------------
  messages  <- .normalize_messages(messages)
  endpoint  <- get_endpoint(
    config,
    default_endpoint = paste0(
      "https://generativelanguage.googleapis.com/v1beta/models/",
      config$model,
      ":generateContent")
  )
  params    <- .translate_params("gemini", config$model_params)

  ## ---- 3. Separate system vs chat lines -----------------------------------
  sys_msgs   <- purrr::keep(messages, ~ .x$role == "system")
  other_msgs <- purrr::keep(messages, ~ .x$role != "system")

  system_instruction <- if (length(sys_msgs) > 0L) {
    list(parts = list(
      list(text = paste(vapply(sys_msgs, `[[`, "", "content"),
                        collapse = " "))
    ))
  } else NULL

  ## ---- 4. Convert each message to Gemini schema ---------------------------
  formatted_messages <- lapply(other_msgs, function(msg) {

    role_out <- if (msg$role == "assistant") "model" else "user"

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

    list(role = role_out, parts = parts)
  })

  ## ---- 5. Assemble request body -------------------------------------------
  body <- .drop_null(list(
    contents          = formatted_messages,
    generationConfig  = .drop_null(list(
      temperature       = params$temperature,
      maxOutputTokens   = params$maxOutputTokens,
      topP              = params$topP,
      topK              = params$topK,
      responseMimeType  = "text/plain",
      thinkingConfig    = if (!is.null(params$thinkingBudget) ||
                              !is.null(params$includeThoughts))
        .drop_null(list(
          thinkingBudget = params$thinkingBudget,
          includeThoughts= isTRUE(params$includeThoughts)
        ))
    )),
    system_instruction = system_instruction       # NOTE: snake_case per REST spec
  ))

  ## ---- 6. Fire -------------------------------------------------------------
  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"   = "application/json",
      "x-goog-api-key" = config$api_key
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
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
call_llm.groq <- function(config, messages, verbose = FALSE, json = FALSE) {
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


  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, json)
}

#' @export
call_llm.together <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (isTRUE(config$embedding)) {
    return(call_llm.together_embedding(config, messages, verbose, json))
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

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, json)
}

#' @export
call_llm.deepseek <- function(config, messages, verbose = FALSE, json = FALSE) {
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

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, json)
}

#' @export
call_llm.xai <- function(config, messages, verbose = FALSE, json = FALSE) {
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

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"  = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}



# ----- Embedding-specific Handlers -----

#' @export
#' @keywords internal
call_llm.openai_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(model = config$model, input = texts)
  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", config$api_key)) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, json=TRUE)
}

#' @export
call_llm.voyage <- function(config, messages, verbose = FALSE, json = FALSE) {
  # Voyage is embeddings-only in this implementation
  return(call_llm.voyage_embedding(config, messages, verbose, json))
}

#' @export
#' @keywords internal
call_llm.voyage_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.voyageai.com/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(input = texts, model = config$model)
  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", config$api_key)) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, json = TRUE)
}

#' @export
#' @keywords internal
call_llm.together_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/embeddings")
  texts <- if (is.character(messages)) messages else sapply(messages, `[[`, "content")
  body <- list(model = config$model, input = texts)
  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", config$api_key)) |>
    httr2::req_body_json(body)
  perform_request(req, verbose, json=TRUE)
}

#' @export
#' @keywords internal
call_llm.gemini_embedding <- function(config, messages,
                                      verbose = FALSE, json = FALSE) {

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

    resp <- httr2::request(endpoint) |>
      httr2::req_headers(
        "Content-Type"   = "application/json",
        "x-goog-api-key" = config$api_key
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
    num_expected_rows <- length(embedding_response$data)


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
      resp <- call_llm_robust(embed_config, batch_texts, verbose = FALSE, json = TRUE)
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

  if(!is.null(names(texts))){
    rownames(final_embeddings) <- names(texts)
  }

  if (verbose) {
    n_successful <- sum(stats::complete.cases(final_embeddings))
    message("Successfully generated embeddings for ", n_successful,
            "/", n_docs, " texts (", ncol(final_embeddings), " dimensions)")
  }

  return(final_embeddings)
}
