# -----------------------------------------------------------------------------
# ZAGENT.R
# ----- Helper Functions -----

#' Perform API Request
#'
#' Internal helper function to perform the API request and process the response.
#'
#' @keywords internal
#' @noRd
#' @importFrom httr2 req_perform resp_body_raw resp_body_json
perform_request <- function(req, verbose, json) {
  resp <- httr2::req_perform(req)
  # Get the raw response as text
  raw_response <- httr2::resp_body_raw(resp)
  raw_json <- rawToChar(raw_response)
  # Parse the response as JSON
  content <- httr2::resp_body_json(resp)

  ## uncomment for diagnostics
  # print(content)

  if (verbose) {
    cat("Full API Response:\n")
    print(content)
  }

  if (json) {
    # Return the text with attributes for full_response and raw_json
    text <- extract_text(content)
    attr(text, "full_response") <- content
    attr(text, "raw_json") <- raw_json
    return(text)
  }

  # Extract the text response (or embeddings for embedding calls)
  text <- extract_text(content)
  attr(text, "full_response") <- content
  return(text)
}



#' Extract Text from API Response
#'
#' Internal helper function to extract text from the API response content.
#'
#' @keywords internal
#' @noRd
extract_text <- function(content) {
  # If the content contains embedding results, return it as is.
  if (!is.null(content$data) && is.list(content$data)) {
    return(content)
  }

  if (!is.null(content$choices)) {
    # For APIs like OpenAI, Groq, Together AI
    if (length(content$choices) == 0 || is.null(content$choices[[1]]$message$content)) {
      stop("No choices returned from API.")
    }
    return(content$choices[[1]]$message$content)
  }

  if (!is.null(content$content)) {
    # For Anthropic
    if (length(content$content) == 0 || is.null(content$content[[1]]$text)) {
      stop("No content returned from Anthropic API.")
    }
    return(content$content[[1]]$text)
  }

  # --- ADD THIS GEMINI SPECIFIC BLOCK ---
  if (!is.null(content$candidates)) {
    # For Gemini API
    if (length(content$candidates) == 0 ||
        is.null(content$candidates[[1]]$content$parts) || # Check for parts
        length(content$candidates[[1]]$content$parts) == 0 || # Check if parts is empty
        is.null(content$candidates[[1]]$content$parts[[1]]$text)) { # Finally check for text
      stop("No content returned from Gemini API.")
    }
    return(content$candidates[[1]]$content$parts[[1]]$text)
  }
  # --- END GEMINI BLOCK ---

  stop("Unable to extract text from the API response.")
}






#' Format Anthropic Messages
#'
#' Internal helper function to format messages for Anthropic API.
#'
#' @keywords internal
#' @noRd
format_anthropic_messages <- function(messages) {
  # Separate system messages from user/assistant messages
  system_messages <- purrr::keep(messages, ~ .x$role == "system")
  user_messages <- purrr::keep(messages, ~ .x$role != "system")

  # Concatenate system messages if any
  if (length(system_messages) > 0) {
    system_text <- paste(sapply(system_messages, function(x) x$content), collapse = " ")
  } else {
    system_text <- NULL
  }

  # Convert user messages to Anthropic's expected format
  formatted_messages <- lapply(user_messages, function(msg) {
    list(
      role = msg$role,
      content = list(
        list(
          type = "text",
          text = msg$content
        )
      )
    )
  })

  list(system_text = system_text, formatted_messages = formatted_messages)
}

# ----- Exported Functions -----

#' Create LLM Configuration
#'
#' Creates a configuration object for interacting with a specified LLM API provider.
#'
#' @param provider A string specifying the API provider. Supported providers include:
#'   "openai" for OpenAI,
#'   "anthropic" for Anthropic,
#'   "groq" for Groq,
#'   "together" for Together AI,
#'   "deepseek" for DeepSeek,
#'   "voyage" for Voyage AI,
#'   "gemini" for Google Gemini.
#' @param model The model name to use. This depends on the provider.
#' @param api_key Your API key for the provider.
#' @param trouble_shooting Prints out all api calls. USE WITH EXTREME CAUTION as it prints your API key.
#' @param ... Additional model-specific parameters (e.g., `temperature`, `max_tokens`, etc.).
#'
#' @return An object of class `llm_config` containing API and model parameters.
#' @export
#'
#' @examples
#' \dontrun{
#'   # OpenAI Example (chat)
#'   openai_config <- llm_config(
#'     provider = "openai",
#'     model = "gpt-4o-mini",
#'     api_key = Sys.getenv("OPENAI_KEY"),
#'     temperature = 0.7,
#'     max_tokens = 500
#'   )
#'
#'   # OpenAI Embedding Example (overwriting api_url):
#'   openai_embed_config <- llm_config(
#'     provider = "openai",
#'     model = "text-embedding-3-small",
#'     api_key = Sys.getenv("OPENAI_KEY"),
#'     temperature = 0.3,
#'     api_url = "https://api.openai.com/v1/embeddings"
#'   )
#'
#'   text_input <- c("Political science is a useful subject",
#'                   "We love sociology",
#'                   "German elections are different",
#'                   "A student was always curious.")
#'
#'   embed_response <- call_llm(openai_embed_config, text_input)
#'   # parse_embeddings() can then be used to convert the embedding results.
#'
#'   # Voyage AI Example:
#'   voyage_config <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-large-2",
#'     api_key = Sys.getenv("VOYAGE_API_KEY")
#'   )
#'
#'   embedding_response <- call_llm(voyage_config, text_input)
#'   embeddings <- parse_embeddings(embedding_response)
#'   embeddings |> cor() |> print()
#' }
llm_config <- function(provider, model, api_key, trouble_shooting = FALSE, ...) {
  model_params <- list(...)
  config <- list(
    provider = provider,
    model = model,
    api_key = api_key,
    model_params = model_params,
    trouble_shooting = trouble_shooting
  )
  class(config) <- c("llm_config", provider)
  return(config)
}

#' Call LLM API
#'
#' Sends a message or data to the specified LLM API and retrieves the response.
#'
#' @param config An `llm_config` object created by `llm_config()`.
#' @param messages A list of message objects (for chat calls) or a character vector (for embeddings).
#' @param verbose Logical. If `TRUE`, prints the full API response.
#' @param json Logical. If `TRUE`, returns the raw JSON response as an attribute.
#'
#' @return The generated text response or embedding results with additional attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' # Make sure to set your needed API keys in environment variables
#'   # OpenAI Embedding Example (overwriting api_url):
#'   openai_embed_config <- llm_config(
#'     provider = "openai",
#'     model = "text-embedding-3-small",
#'     api_key = Sys.getenv("OPENAI_KEY"),
#'     temperature = 0.3,
#'     api_url = "https://api.openai.com/v1/embeddings"
#'   )
#'
#'   text_input <- c("Political science is a useful subject",
#'                   "We love sociology",
#'                   "German elections are different",
#'                   "A student was always curious.")
#'
#'   embed_response <- call_llm(openai_embed_config, text_input)
#'
#'   # Voyage AI Example:
#'   voyage_config <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-large-2",
#'     api_key = Sys.getenv("VOYAGE_API_KEY")
#'   )
#'
#'   embedding_response <- call_llm(voyage_config, text_input)
#'   embeddings <- parse_embeddings(embedding_response)
#'   embeddings |> cor() |> print()
#'
#'
#'   # Gemini Example
#'   gemini_config <- llm_config(
#'     provider = "gemini",
#'     model = "gemini-pro",
#'     api_key = Sys.getenv("GEMINI_API_KEY"),
#'     temperature = 0.9,
#'     max_tokens = 800,
#'     top_p = 0.9,
#'     top_k = 10
#'   )
#'
#'   gemini_message <- list(
#'     list(role = "user", content = "Explain the theory of relativity to a curious 3-year-old!")
#'   )
#'
#'   gemini_response <- call_llm(
#'     config = gemini_config,
#'     messages = gemini_message,
#'     json = TRUE
#'   )
#'
#'   cat("Gemini Response:", gemini_response, "\n")
#'   raw_json_gemini_response <- attr(gemini_response, "raw_json")
#'   print(raw_json_gemini_response)
#' }
call_llm <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (config$trouble_shooting == TRUE){
    cat("\n\nInside call_llm for troubleshooting\n")
    cat("BE CAREFUL: This prints your API key!\n")
    cat("Messages:\n")
    print(messages)
    cat("config:\n")
    print(config)
    cat("\n\n")
  }
  UseMethod("call_llm", config)
}

# Helper to determine the endpoint if user overwrote it
get_endpoint <- function(config, default_endpoint) {
  if (!is.null(config$model_params$api_url)) {
    return(config$model_params$api_url)
  }
  default_endpoint
}

#' @export
call_llm.default <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/chat/completions")
  message("Provider-specific function not present, defaulting to OpenAI format.")

  body <- list(
    model = config$model,
    messages = messages,
    temperature = rlang::`%||%`(config$model_params$temperature, 1),
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    top_p = rlang::`%||%`(config$model_params$top_p, 1),
    frequency_penalty = rlang::`%||%`(config$model_params$frequency_penalty, 0),
    presence_penalty = rlang::`%||%`(config$model_params$presence_penalty, 0)
  )

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}

#' @export
call_llm.openai <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/chat/completions")
  body <- list(
    model = config$model,
    messages = messages,
    temperature = rlang::`%||%`(config$model_params$temperature, 1),
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    top_p = rlang::`%||%`(config$model_params$top_p, 1),
    frequency_penalty = rlang::`%||%`(config$model_params$frequency_penalty, 0),
    presence_penalty = rlang::`%||%`(config$model_params$presence_penalty, 0)
  )

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
  endpoint <- get_endpoint(config, default_endpoint = "https://api.anthropic.com/v1/messages")
  formatted <- format_anthropic_messages(messages)

  body <- list(
    model = config$model,
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    temperature = rlang::`%||%`(config$model_params$temperature, 1)
  )

  if (!is.null(formatted$system_text)) {
    body$system <- formatted$system_text
  }
  if (length(formatted$formatted_messages) > 0) {
    body$messages <- formatted$formatted_messages
  }

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = config$api_key,
      "anthropic-version" = "2023-06-01"
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}

#' @export
call_llm.groq <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.groq.com/openai/v1/chat/completions")
  body <- list(
    model = config$model,
    messages = messages,
    temperature = rlang::`%||%`(config$model_params$temperature, 0.7),
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024)
  )

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
  endpoint <- get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/chat/completions")
  body <- list(
    model = config$model,
    messages = messages,
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    temperature = rlang::`%||%`(config$model_params$temperature, 0.7),
    top_p = rlang::`%||%`(config$model_params$top_p, 0.7),
    top_k = rlang::`%||%`(config$model_params$top_k, 50),
    repetition_penalty = rlang::`%||%`(config$model_params$repetition_penalty, 1),
    stop = rlang::`%||%`(config$model_params$stop, c("<|eot_id|>", "<|eom_id|>")),
    stream = rlang::`%||%`(config$model_params$stream, FALSE)
  )

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
  endpoint <- get_endpoint(config, default_endpoint = "https://api.deepseek.com/chat/completions")
  body <- list(
    model = rlang::`%||%`(config$model, "deepseek-chat"),
    messages = messages,
    temperature = rlang::`%||%`(config$model_params$temperature, 1),
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    top_p = rlang::`%||%`(config$model_params$top_p, 1),
    frequency_penalty = rlang::`%||%`(config$model_params$frequency_penalty, 0),
    presence_penalty = rlang::`%||%`(config$model_params$presence_penalty, 0)
  )

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}

#' @export
call_llm.voyage <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.voyageai.com/v1/embeddings")
  body <- list(
    input = messages,
    model = config$model
  )

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}

#' @export
call_llm.gemini <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(
    config,
    default_endpoint = paste0("https://generativelanguage.googleapis.com/v1beta/models/", config$model, ":generateContent")
  )

  # Extract system messages and combine their content
  system_messages <- purrr::keep(messages, ~ .x$role == "system")
  other_messages <- purrr::keep(messages, ~ .x$role != "system")

  if (length(system_messages) > 0) {
    system_text <- paste(sapply(system_messages, function(x) x$content), collapse = " ")
    system_instruction <- list(parts = list(list(text = system_text)))
  } else {
    system_instruction <- NULL
  }

  # Format non-system messages, mapping "assistant" to "model" for Gemini
  formatted_messages <- lapply(other_messages, function(msg) {
    role <- if (msg$role == "assistant") "model" else "user"
    list(
      role = role,
      parts = list(list(text = msg$content))
    )
  })

  body <- list(
    contents = formatted_messages,
    generationConfig = list(
      temperature = rlang::`%||%`(config$model_params$temperature, 1),
      maxOutputTokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
      topP = rlang::`%||%`(config$model_params$top_p, 1),
      topK = rlang::`%||%`(config$model_params$top_k, 1)
    )
  )
  if (!is.null(system_instruction)) {
    body$systemInstruction <- system_instruction
  }

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-goog-api-key" = config$api_key
    ) |>
    httr2::req_body_json(body)

  perform_request(req, verbose, json)
}

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
#'   embeddings |> cor() |> print()
#' }
parse_embeddings <- function(embedding_response) {
  embeddings <- embedding_response$data |>
    purrr::map(~ as.numeric(.x$embedding)) |>
    as.data.frame() |>
    as.matrix()
  colnames(embeddings) <- seq_len(ncol(embeddings))
  embeddings
}



############ Zagent.R
# -----------------------------------------------------------------------------
# ZAGENT.R
#
# This file contains the Agent and LLMConversation classes for building multi-agent
# conversational simulations. Agents can be added to a conversation, share memory,
# and respond to prompts
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# UTILITY
# -----------------------------------------------------------------------------

#' @keywords internal
#' Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# S3 "PLUS" DISPATCH FOR LLMConversation
# -----------------------------------------------------------------------------

#' @export
`+.LLMConversation` <- function(e1, e2) {
  # e1 = LLMConversation, e2 = something
  if (inherits(e2, "AgentAction")) {
    return(`+.LLMConversation.AgentAction`(e1, e2))
  } else {
    stop("Unsupported addition: LLMConversation + ", class(e2)[1])
  }
}

#' @export
`+.LLMConversation.AgentAction` <- function(e1, e2) {
  # e1 is LLMConversation, e2 is AgentAction
  agent_id <- e2$agent$id
  # If agent isn't in conversation, add it
  if (is.null(e1$agents[[agent_id]])) {
    e1$add_agent(e2$agent)
  }
  # Capture the agent's response
  response <- e1$converse(
    agent_id        = agent_id,
    prompt_template = e2$prompt_template,
    replacements    = e2$replacements,
    verbose         = e2$verbose
  )
  e1$last_response <- response

  # Update cumulative token counts
  e1$total_tokens_sent <- (e1$total_tokens_sent %||% 0) + response$tokens_sent
  e1$total_tokens_received <- (e1$total_tokens_received %||% 0) + response$tokens_received

  e1
}

# -----------------------------------------------------------------------------
# AGENTACTION S3 CLASS
# -----------------------------------------------------------------------------

#' AgentAction S3 Class
#'
#' @description
#' An object that bundles an Agent together with a prompt and replacements so that
#' it can be chained onto a conversation with the `+` operator.
#'
#' When `conversation + AgentAction` is called:
#' \enumerate{
#'   \item If the agent is not yet in the conversation, it is added.
#'   \item The agent is prompted with the provided prompt template (and replacements).
#'   \item The conversation is updated with the agent's response.
#' }
#'
#' @param agent An \code{Agent} object.
#' @param prompt_template A character string (the prompt).
#' @param replacements A named list for placeholder substitution (optional).
#' @param verbose Logical. If \code{TRUE}, prints verbose LLM response info. Default \code{FALSE}.
#'
#' @return An object of class \code{AgentAction}, used in conversation chaining.
#'
#' @export
AgentAction <- function(agent, prompt_template, replacements = list(), verbose = FALSE) {
  structure(
    list(
      agent           = agent,
      prompt_template = prompt_template,
      replacements    = replacements,
      verbose         = verbose
    ),
    class = "AgentAction"
  )
}


# -----------------------------------------------------------------------------
# AGENT R6 CLASS
# -----------------------------------------------------------------------------

#' @title Agent Class for LLM Interactions
#'
#' @description
#' An R6 class representing an agent that interacts with language models.
#'
#' *At agent-level we do not automate summarization.* The `maybe_summarize_memory()`
#'  function can be called manually if the user wishes to compress the agent's memory.
#'
#' @export
Agent <- R6::R6Class(
  "Agent",

  public = list(

    #' @field id Unique ID for this Agent.
    id = NULL,

    #' @field context_length Maximum number of conversation turns stored in memory.
    context_length = 5,

    #' @field model_config The \code{llm_config} specifying which LLM to call.
    model_config = NULL,

    #' @field memory A list of speaker/text pairs that the agent has memorized.
    memory = list(),

    #' @field persona Named list for additional agent-specific details (e.g., role, style).
    persona = list(),

    #' @field enable_summarization Logical. If TRUE, user *may* call `maybe_summarize_memory()`.
    enable_summarization = TRUE,

    #' @field token_threshold Numeric. If manually triggered, we can compare total_tokens.
    token_threshold = 1000,

    #' @field total_tokens Numeric. Estimated total tokens in memory.
    total_tokens = 0,

    #' @field summarization_density Character. "low", "medium", or "high".
    summarization_density = "medium",

    #' @field summarization_prompt Character. Optional custom prompt for summarization.
    summarization_prompt = NULL,

    #' @field summarizer_config Optional \code{llm_config} for summarizing the agent's memory.
    summarizer_config = NULL,

    #' @field auto_inject_conversation Logical. If TRUE, automatically prepend conversation memory if missing.
    auto_inject_conversation = TRUE,

    #' @description
    #' Create a new Agent instance.
    #'
    #' @param id Character. The agent's unique identifier.
    #' @param context_length Numeric. The maximum number of messages stored (default = 5).
    #' @param persona A named list of persona details.
    #' @param model_config An \code{llm_config} object specifying LLM settings.
    #' @param enable_summarization Logical. If TRUE, you can manually call summarization.
    #' @param token_threshold Numeric. If you're calling summarization, use this threshold if desired.
    #' @param summarization_density Character. "low", "medium", "high" for summary detail.
    #' @param summarization_prompt Character. Optional custom prompt for summarization.
    #' @param summarizer_config Optional \code{llm_config} for summarization calls.
    #' @param auto_inject_conversation Logical. If TRUE, auto-append conversation memory to prompt if missing.
    #'
    #' @return A new \code{Agent} object.
    initialize = function(id,
                          context_length = 5,
                          persona = NULL,
                          model_config,
                          enable_summarization = TRUE,
                          token_threshold = 1000,
                          summarization_density = "medium",
                          summarization_prompt = NULL,
                          summarizer_config = NULL,
                          auto_inject_conversation = TRUE) {

      if (missing(id)) stop("Agent id is required.")
      if (missing(model_config)) stop("model_config is required.")
      if (!inherits(model_config, "llm_config")) {
        stop("model_config must be an llm_config object.")
      }

      self$id <- id
      self$context_length <- context_length
      self$persona <- persona %||% list()
      self$model_config <- model_config
      self$memory <- list()

      self$enable_summarization <- enable_summarization
      self$token_threshold <- token_threshold
      self$total_tokens <- 0
      self$summarization_density <- match.arg(summarization_density, c("low", "medium", "high"))
      self$summarization_prompt <- summarization_prompt
      self$summarizer_config <- summarizer_config

      self$auto_inject_conversation <- auto_inject_conversation
    },

    #' @description
    #' Add a new message to the agent's memory.
    #' We do NOT automatically call summarization here.
    #'
    #' @param speaker Character. The speaker name or ID.
    #' @param text Character. The message content.
    add_memory = function(speaker, text) {
      # Estimate tokens: naive approach ~ words * 1.3
      new_tokens <- length(strsplit(text, "\\s+")[[1]]) * 1.3
      self$total_tokens <- self$total_tokens + new_tokens
      self$memory <- append(self$memory, list(list(speaker = speaker, text = text)))
    },

    #' @description
    #' Manually compress the agent's memory if desired.
    #' Summarizes all memory into a single "summary" message.
    maybe_summarize_memory = function() {
      if (!self$enable_summarization) {
        message("Summarization is disabled for this agent.")
        return(invisible())
      }
      if (length(self$memory) == 0) {
        message("No memory to summarize.")
        return(invisible())
      }

      conversation_text <- paste(
        sapply(self$memory, function(msg) paste0(msg$speaker, ": ", msg$text)),
        collapse = "\n"
      )

      density_instruction <- switch(
        self$summarization_density,
        "low"    = "Provide a concise summary with minimal detail.",
        "medium" = "Summarize the key points and main ideas.",
        "high"   = "Create a detailed summary including significant details."
      )

      # If user provided a summarization_prompt, use it. Otherwise default
      prompt <- self$summarization_prompt %||% paste(
        "Summarize this conversation:\n\n",
        conversation_text,
        "\n\n", density_instruction
      )

      # If we have a summarizer_config, use that. Otherwise, use the agent's own model_config
      if (!is.null(self$summarizer_config)) {
        summary_response <- call_llm(
          self$summarizer_config,
          list(list(role = "user", content = prompt)),
          verbose = FALSE,
          json = TRUE
        )
        summary_text <- extract_text(attr(summary_response, "full_response"))
      } else {
        # Fallback: use the agent's own model_config
        tmp <- self$call_llm_agent(prompt, verbose = FALSE)
        summary_text <- tmp$text
      }

      # Reset and store only the summary
      self$reset_memory()
      self$add_memory("summary", summary_text)
      self$total_tokens <- length(strsplit(summary_text, "\\s+")[[1]]) * 1.3
      message("Agent memory has been summarized.")
    },

    #' @description
    #' Internal helper to prepare final prompt by substituting placeholders.
    #'
    #' @param template Character. The prompt template.
    #' @param replacements A named list of placeholder values.
    #' @return Character. The prompt with placeholders replaced.
    generate_prompt = function(template, replacements = list()) {
      replace_placeholders <- function(templ, reps) {
        if (length(reps) == 0) return(templ)
        out <- templ
        for (nm in names(reps)) {
          ph <- paste0("{{", nm, "}}")
          out <- gsub(ph, as.character(reps[[nm]]), out, fixed = TRUE)
        }
        out
      }
      replace_placeholders(template, replacements)
    },

    #' @description
    #' Low-level call to the LLM (via \code{call_llm()}) with a final prompt.
    #' If persona is defined, a system message is prepended to help set the role.
    #'
    #' @param prompt Character. The final prompt text.
    #' @param verbose Logical. If TRUE, prints debug info. Default FALSE.
    #'
    #' @return A list with:
    #'   * text
    #'   * tokens_sent
    #'   * tokens_received
    #'   * full_response (raw list)
    call_llm_agent = function(prompt, verbose = FALSE) {
      if (length(self$persona) > 0) {
        role <- self$persona$role %||% "agent"
        other_attrs <- self$persona[names(self$persona) != "role"]
        attrs_str <- if (length(other_attrs) > 0) {
          paste(names(other_attrs), "=", other_attrs, collapse = ", ")
        } else {
          ""
        }
        persona_str <- paste0(
          'Pretend you are a "', role, '"',
          if (nchar(attrs_str) > 0) paste0(" with: ", attrs_str, ".") else "."
        )
        messages <- list(
          list(role = "system", content = persona_str),
          list(role = "user", content = prompt)
        )
      } else {
        messages <- list(list(role = "user", content = prompt))
      }

      response <- tryCatch({
        call_llm(self$model_config, messages, json = TRUE, verbose = verbose)
      }, error = function(e) {
        stop("LLM API call failed: ", e$message)
      })

      # Extract text
      full_resp <- attr(response, "full_response")
      text_out <- ""
      tryCatch({
        text_out <- private$extract_text_from_response(full_resp)
      }, error = function(e) {
        warning("Error extracting text: ", e$message)
      })

      # Token usage
      token_info <- list(tokens_sent = 0, tokens_received = 0)
      tryCatch({
        token_info <- private$extract_token_counts(full_resp, self$model_config$provider)
      }, error = function(e) {
        warning("Error counting tokens: ", e$message)
      })

      list(
        text            = if (is.null(text_out)) "" else as.character(text_out),
        tokens_sent     = as.numeric(token_info$tokens_sent),
        tokens_received = as.numeric(token_info$tokens_received),
        full_response   = full_resp
      )
    },

    #' @description
    #' Generate a response from the LLM using a prompt template and optional replacements.
    #' Substitutes placeholders, calls the LLM, saves output to memory, returns the response.
    #'
    #' @param prompt_template Character. The prompt template.
    #' @param replacements A named list of placeholder values.
    #' @param verbose Logical. If TRUE, prints extra info. Default FALSE.
    #'
    #' @return A list with fields \code{text}, \code{tokens_sent}, \code{tokens_received}, \code{full_response}.
    generate = function(prompt_template, replacements = list(), verbose = FALSE) {
      prompt <- self$generate_prompt(prompt_template, replacements)
      out <- self$call_llm_agent(prompt, verbose)
      # Append LLM's output to memory
      self$add_memory(self$id, out$text)
      out
    },

    #' @description
    #' The agent "thinks" about a topic, possibly using the entire memory in the prompt.
    #' If auto_inject_conversation is TRUE and the template lacks \{\{conversation\}\}, we prepend the memory.
    #'
    #' @param topic Character. Label for the thought.
    #' @param prompt_template Character. The prompt template.
    #' @param replacements Named list for additional placeholders.
    #' @param verbose Logical. If TRUE, prints info.
    think = function(topic, prompt_template, replacements = list(), verbose = FALSE) {
      if (missing(topic)) stop("Topic is required for thinking.")
      if (missing(prompt_template)) stop("Prompt template is required for thinking.")

      conversation <- paste(
        sapply(self$memory, function(msg) paste0(msg$speaker, ": ", msg$text)),
        collapse = "\n"
      )
      last_output <- if (length(self$memory) > 0) self$memory[[length(self$memory)]]$text else ""

      full_replacements <- c(
        list(topic = topic,
             conversation = conversation,
             last_output = last_output),
        replacements,
        self$persona
      )

      if (!grepl("\\{\\{conversation\\}\\}", prompt_template, fixed = TRUE) &&
          self$auto_inject_conversation) {
           if (length(self$memory) >0 )
                     prompt_template <- paste(
                    "Conversation so far:\n{{conversation}}\n\nNow think:\n",
                      prompt_template)
      }

      self$generate(prompt_template, full_replacements, verbose)
    },

    #' @description
    #' The agent produces a public "response" about a topic.
    #' If auto_inject_conversation is TRUE and the template lacks \{\{conversation\}\}, we prepend the memory.
    #'
    #' @param topic Character. A short label for the question/issue.
    #' @param prompt_template Character. The prompt template.
    #' @param replacements Named list of placeholder substitutions.
    #' @param verbose Logical. If TRUE, prints extra info.
    #'
    #' @return A list with \code{text}, \code{tokens_sent}, \code{tokens_received}, \code{full_response}.
    respond = function(topic, prompt_template, replacements = list(), verbose = FALSE) {
      if (missing(topic)) stop("Topic is required for responding.")
      if (missing(prompt_template)) stop("Prompt template is required for responding.")

      conversation <- paste(
        sapply(self$memory, function(msg) paste0(msg$speaker, ": ", msg$text)),
        collapse = "\n"
      )
      last_output <- if (length(self$memory) > 0) self$memory[[length(self$memory)]]$text else ""

      full_replacements <- c(
        list(topic = topic,
             conversation = conversation,
             last_output = last_output),
        replacements,
        self$persona
      )

      if (!grepl("\\{\\{conversation\\}\\}", prompt_template, fixed = TRUE) &&
          self$auto_inject_conversation) {

        if ( length(self$memory)>0  )
                     prompt_template <- paste(
                     "Conversation so far:\n{{conversation}}\n\nNow respond:\n",
                       prompt_template)
      }

      self$generate(prompt_template, full_replacements, verbose)
    },

    #' @description
    #' Reset the agent's memory.
    reset_memory = function() {
      self$memory <- list()
      self$total_tokens <- 0
    }
  ),

  private = list(

    extract_token_counts = function(response, provider) {
      usage <- response$usage %||% NULL
      if (is.null(usage)) {
        if (!is.null(response$usageMetadata)) {
          return(list(
            tokens_sent = as.numeric(response$usageMetadata$promptTokenCount %||% 0),
            tokens_received = as.numeric(response$usageMetadata$candidatesTokenCount %||% 0)
          ))
        }
        return(list(tokens_sent = 0, tokens_received = 0))
      }

      tokens_sent <- switch(
        provider,
        "openai"    = usage$prompt_tokens,
        "anthropic" = usage$input_tokens,
        "groq"      = usage$prompt_tokens,
        "together"  = usage$prompt_tokens,
        "deepseek"  = usage$prompt_tokens,
        "gemini"    = if (!is.null(response$usageMetadata)) {
          response$usageMetadata$promptTokenCount
        } else {
          usage$prompt_tokens
        },
        usage$prompt_tokens %||% 0
      )

      tokens_received <- switch(
        provider,
        "openai"    = usage$completion_tokens,
        "anthropic" = usage$output_tokens,
        "groq"      = usage$completion_tokens,
        "together"  = usage$completion_tokens,
        "deepseek"  = usage$completion_tokens,
        "gemini"    = if (!is.null(response$usageMetadata)) {
          response$usageMetadata$candidatesTokenCount
        } else {
          usage$completion_tokens
        },
        usage$completion_tokens %||% 0
      )

      list(
        tokens_sent     = as.numeric(tokens_sent %||% 0),
        tokens_received = as.numeric(tokens_received %||% 0)
      )
    },

    extract_text_from_response = function(response) {
      if (is.null(response)) return("")
      if (!is.null(response$choices) && length(response$choices) > 0) {
        choice <- response$choices[[1]]
        if (is.list(choice$message) && !is.null(choice$message$content)) {
          return(as.character(choice$message$content))
        }
        if (!is.null(choice$text)) {
          return(as.character(choice$text))
        }
      }
      if (!is.null(response$content) && length(response$content) > 0) {
        content_item <- response$content[[1]]
        if (!is.null(content_item$text)) {
          return(as.character(content_item$text))
        }
      }
      if (!is.null(response$candidates) && length(response$candidates) > 0) {
        candidate <- response$candidates[[1]]
        if (!is.null(candidate$content) &&
            !is.null(candidate$content$parts) &&
            length(candidate$content$parts) > 0) {
          part_text <- candidate$content$parts[[1]]$text
          if (!is.null(part_text)) {
            return(as.character(part_text))
          }
        }
      }
      ""
    }
  )
)


# -----------------------------------------------------------------------------
# LLMCONVERSATION R6 CLASS
# -----------------------------------------------------------------------------

#' @title LLMConversation Class for Coordinating Agents
#'
#' @description
#' An R6 class for managing a conversation among multiple \code{Agent} objects.
#' Includes optional conversation-level summarization if `summarizer_config` is provided:
#'
#' \enumerate{
#'   \item \strong{summarizer_config:} A list that can contain:
#'       \itemize{
#'         \item \code{llm_config}: The \code{llm_config} used for the summarizer call (default a basic OpenAI).
#'         \item \code{prompt}: A custom summarizer prompt (default provided).
#'         \item \code{threshold}: Word-count threshold (default 3000 words).
#'         \item \code{summary_length}: Target length in words for the summary (default 400).
#'       }
#'   \item Once the total conversation word count exceeds `threshold`, a summarization is triggered.
#'   \item The conversation is replaced with a single condensed message that keeps track of who said what.
#' }
#'
#' @export
LLMConversation <- R6::R6Class(
  "LLMConversation",
  public = list(
    #' @field agents A named list of \code{Agent} objects.
    agents = list(),
    #' @field conversation_history A list of speaker/text pairs for the entire conversation.
    conversation_history = list(),
    #' @field conversation_history_full A list of speaker/text pairs for the entire conversation that is never modified and never used directly.
    conversation_history_full = list(),
    #' @field topic A short string describing the conversation's theme.
    topic = NULL,
    #' @field prompts An optional list of prompt templates (may be ignored).
    prompts = NULL,
    #' @field shared_memory Global store that is also fed into each agent's memory.
    shared_memory = list(),
    #' @field last_response last response received
    last_response = NULL,
    #' @field total_tokens_sent total tokens sent in conversation
    total_tokens_sent = 0,
    #' @field total_tokens_received total tokens received in conversation
    total_tokens_received = 0,
    #' @field summarizer_config Config list controlling optional conversation-level summarization.
    summarizer_config = NULL,

    #' @description
    #' Create a new conversation.
    #' @param topic Character. The conversation topic.
    #' @param prompts Optional named list of prompt templates.
    #' @param summarizer_config Optional list controlling conversation-level summarization.
    initialize = function(topic,
                          prompts = NULL,
                          summarizer_config = NULL) {
      if (missing(topic)) stop("Conversation topic is required.")
      self$topic <- topic
      self$prompts <- prompts
      self$agents <- list()
      self$conversation_history <- list()
      self$conversation_history_full <- list()
      self$shared_memory <- list()
      self$last_response <- NULL
      self$total_tokens_sent <- 0
      self$total_tokens_received <- 0
      self$summarizer_config <- summarizer_config
    },

    #' @description
    #' Add an \code{Agent} to this conversation. The agent is stored by \code{agent$id}.
    #' @param agent An Agent object.
    add_agent = function(agent) {
      if (!inherits(agent, "Agent")) {
        stop("add_agent() requires an object of class Agent.")
      }
      self$agents[[agent$id]] <- agent
    },

    #' @description
    #' Add a message to the global conversation log. Also appended to shared memory.
    #' Then possibly trigger summarization if configured.
    #' @param speaker Character. Who is speaking?
    #' @param text Character. What they said.
    add_message = function(speaker, text) {

      new_msg <- list(speaker = speaker, text = text)

      self$conversation_history <- append(self$conversation_history,list(new_msg) )

      self$conversation_history_full <- append(self$conversation_history_full, list(new_msg))  # Always accumulate

      self$shared_memory <- append(self$shared_memory,list(new_msg) )

      # Attempt conversation-level summarization if summarizer_config is provided
      if (!is.null(self$summarizer_config)) {
        self$maybe_summarize_conversation()
      }
    },

    #' @description
    #' Have a specific agent produce a response. The entire global conversation plus
    #' shared memory is temporarily loaded into that agent. Then the new message is
    #' recorded in the conversation. The agent's memory is then reset except for its new line.
    #'
    #' @param agent_id Character. The ID of the agent to converse.
    #' @param prompt_template Character. The prompt template for the agent.
    #' @param replacements A named list of placeholders to fill in the prompt.
    #' @param verbose Logical. If TRUE, prints extra info.
    converse = function(agent_id, prompt_template, replacements = list(), verbose = FALSE) {
      if (is.null(self$agents[[agent_id]])) {
        stop("Agent ", agent_id, " is not in the conversation.")
      }
      agent <- self$agents[[agent_id]]

      # Temporarily feed entire conversation history to the agent
      for (msg in self$conversation_history) {
        agent$add_memory(msg$speaker, msg$text)
      }
      # Also feed shared memory
      for (msg in self$shared_memory) {
        agent$add_memory("shared", msg$text)
      }

      # Let the agent respond
      response <- agent$respond(self$topic, prompt_template, replacements, verbose)

      # Store in the conversation
      self$add_message(agent_id, response$text)

      # Agent memory: reset then store only the new line
      agent$reset_memory()
      agent$add_memory(agent_id, response$text)

      invisible(response)
    },

    #' @description
    #' Run a multi-step conversation among a sequence of agents.
    #' @param agent_sequence Character vector of agent IDs in the order they speak.
    #' @param prompt_template Single string or named list of strings keyed by agent ID.
    #' @param replacements Single list or list-of-lists with per-agent placeholders.
    #' @param verbose Logical. If TRUE, prints extra info.
    run = function(agent_sequence, prompt_template, replacements = list(), verbose = FALSE) {
      if (is.null(agent_sequence)) {
        stop("agent_sequence cannot be NULL.")
      }
      for (i in seq_along(agent_sequence)) {
        agent_id <- agent_sequence[i]
        current_prompt <- if (is.list(prompt_template)) {
          prompt_template[[agent_id]] %||% stop("No prompt for agent: ", agent_id)
        } else {
          prompt_template
        }
        current_reps <- if (is.list(replacements) && length(replacements) == length(agent_sequence)) {
          replacements[[i]]
        } else {
          replacements
        }
        self$converse(agent_id, current_prompt, current_reps, verbose)
      }
    },

    #' @description
    #' Print the conversation so far to the console.
    print_history = function() {
      cat("Conversation History:\n")
      for (msg in self$conversation_history) {
        cat(sprintf("\n-------------\n<%s>: %s\n", msg$speaker, msg$text))
      }
    },

    #' @description
    #' Clear the global conversation and reset all agents' memories.
    reset_conversation = function() {
      self$conversation_history <- list()
      self$shared_memory <- list()
      for (agent in self$agents) {
        agent$reset_memory()
      }
      # Note: conversation_history_full remains untouched
    },

    #' @description
    #' Pipe-like operator to chain conversation steps. E.g., conv |> "Solver"(...)
    #'
    #' @param agent_id Character. The ID of the agent to call next.
    #'
    #' @return A function that expects (prompt_template, replacements, verbose).
    `|>` = function(agent_id) {
      if (!is.character(agent_id) || length(agent_id) != 1) {
        stop("agent_id must be a single character string.")
      }
      if (is.null(self$agents[[agent_id]])) {
        stop("Agent ", agent_id, " not in conversation.")
      }
      force(agent_id)
      function(prompt_template, replacements = list(), verbose = FALSE) {
        self$converse(agent_id, prompt_template, replacements, verbose)
      }
    },

    #' @description
    #' Possibly summarize the conversation if summarizer_config is non-null and
    #' the word count of conversation_history exceeds summarizer_config$threshold.
    maybe_summarize_conversation = function() {
      cfg <- self$summarizer_config

      # Defaults
      threshold <- cfg$threshold %||% 3000
      summary_length <- cfg$summary_length %||% 400

      total_words <- sum(
        sapply(self$conversation_history, function(x) {
          length(strsplit(x$text, "\\s+")[[1]])
        })
      )

      if (total_words > threshold) {
        self$summarize_conversation()
      }
    },

    #' @description
    #' Summarize the conversation so far into one condensed message.
    #' The new conversation history becomes a single message with speaker = "summary".
    summarize_conversation = function() {
      if (is.null(self$summarizer_config)) {
        return(invisible())
      }
      cfg <- self$summarizer_config
      threshold <- cfg$threshold %||% 3000
      summary_length <- cfg$summary_length %||% 400

      # Build a string: "Speaker: text" lines
      conversation_text <- paste(
        sapply(self$conversation_history, function(msg) {
          paste0(msg$speaker, ": ", msg$text)
        }),
        collapse = "\n"
      )

      # Default summarizer prompt if none given

      default_prompt <- paste0(
        "Summarize the following conversation into roughly ", summary_length, " words.\n",
        "Your summary should:\n",
        "- Identify each speaker by role or name.\n",
        "- Preserve their main arguments, concerns, and proposals, pieces of evidence, anecdotes, etc. \n",
        "- Keep important details that is relevant \n",
        "- Maintain a concise structure without omitting critical points.\n\n",
        "CONVERSATION:\n",
        conversation_text,
        "\n\nSUMMARY:"
      )

      prompt <- cfg$prompt %||% default_prompt

      # If summarizer_config has llm_config, use it; else raise an error
      if (!is.null(cfg$llm_config)) {
        summarizer_llm <- cfg$llm_config
      } else {
        stop("summarizer_config must contain an llm_config object.")
        # summarizer_llm <- llm_config(
        #   provider = "openai",
        #   model = "gpt-4o",
        #   api_key = "YOUR_DEFAULT_SUMMARY_API_KEY"
        # )
      }

      # Summarize
      summary_response <- call_llm(
        summarizer_llm,
        list(list(role = "user", content = prompt)),
        json = TRUE
      )
      # Extract text
      full_resp <- attr(summary_response, "full_response")
      summary_text <- ""
      tryCatch({
        summary_text <- extract_text(full_resp)
      }, error = function(e) {
        warning("Error extracting summary text: ", e$message)
      })

      # Replace conversation with a single summary message
      self$conversation_history <- list(list(speaker = "summary", text = summary_text))
      self$shared_memory <- list(list(speaker = "summary", text = summary_text))
    }
  )
)
