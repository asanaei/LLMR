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

  if (is.null(content$choices) && is.null(content$content)) {
    stop("No recognizable content returned from API.")
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
#'   "voyage" for Voyage AI.
#' @param model The model name to use. This depends on the provider.
#' @param api_key Your API key for the provider.
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
#'   # Additional processing:
#'   embeddings |> cor() |> print()
#' }
llm_config <- function(provider, model, api_key, ...) {
  model_params <- list(...)
  config <- list(
    provider = provider,
    model = model,
    api_key = api_key,
    model_params = model_params
  )
  class(config) <- c("llm_config", provider)
  return(config)
}

#' Call LLM API
#'
#' Sends a message to the specified LLM API and retrieves the response.
#'
#' @param config An `llm_config` object created by `llm_config()`.
#' @param messages A list of message objects (or a character vector for embeddings) to send to the API.
#' @param verbose Logical. If `TRUE`, prints the full API response.
#' @param json Logical. If `TRUE`, returns the raw JSON response as an attribute.
#'
#' @return The generated text response or embedding results with additional attributes.
#' @export
#'
#' @examples
#' \dontrun{
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
#' }
call_llm <- function(config, messages, verbose = FALSE, json = FALSE) {
  UseMethod("call_llm", config)
}

# Helper to determine the endpoint
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

  # Build the request body as in call_llm.openai
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
  # Use overwrite if provided, otherwise default to chat completions endpoint.
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
  embeddings <- embedding_response$data |>
    purrr::map(~ as.numeric(.x$embedding)) |>
    as.data.frame() |>
    as.matrix()
  colnames(embeddings) <- seq_len(ncol(embeddings))
  embeddings
}
