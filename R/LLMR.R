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
#' @param provider Provider name (openai, anthropic, groq, together, voyage, gemini, deepseek)
#' @param model Model name to use
#' @param api_key API key for authentication
#' @param troubleshooting Prints out all api calls. USE WITH EXTREME CAUTION as it prints your API key.
#' @param base_url Optional base URL override
#' @param embedding Logical indicating embedding mode: NULL (default, used for backward compatibility, uses prior defaults), TRUE (force embeddings), FALSE (force generative)
#' @param ... Additional provider-specific parameters#'
#' @return Configuration object for use with call_llm()
#' @export
#'
#' @examples
#' \dontrun{
#' ### Generative example
#'   openai_config <- llm_config(
#'     provider = "openai",
#'     model = "gpt-4.1-mini",
#'     api_key = Sys.getenv("OPENAI_KEY"),
#'     temperature = 0.7,
#'     max_tokens = 500)
#'
#' the_message <- list(
#' list(role = "system", content = "You are an expert data scientist."),
#' list(role = "user", content = "When will you ever use the OLS?") )
#'
#' #Call the LLM api
#' response <- call_llm(
#' config = openai_config,
#' messages = the_message)
#' cat("Response:", response, "\n")
#'
#' ### Embedding example
#'   # Voyage AI Example:
#'   voyage_config <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-large-2",
#'     api_key = Sys.getenv("VOYAGE_API_KEY"),
#'     embedding = TRUE
#'   )
#'
#'   embedding_response <- call_llm(voyage_config, text_input)
#'   embeddings <- parse_embeddings(embedding_response)
#'   # Additional processing:
#'   embeddings |> cor() |> print()
#' }
llm_config <- function(provider, model, api_key, troubleshooting = FALSE, base_url = NULL, embedding = NULL, ...) {
  model_params <- list(...)
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
#'   # Voyage AI embedding Example:
#'   voyage_config <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-large-2",
#'     embedding = TRUE,
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
#'     model = "gemini-pro",          # Or another Gemini model
#'     api_key = Sys.getenv("GEMINI_API_KEY"),
#'     temperature = 0.9,               # Controls randomness
#'     max_tokens = 800,              # Maximum tokens to generate
#'     top_p = 0.9,                     # Nucleus sampling parameter
#'     top_k = 10                      # Top K sampling parameter
#'   )
#'
#'   gemini_message <- list(
#'     list(role = "user", content = "Explain the theory of relativity to a curious 3-year-old!")
#'   )
#'
#'   gemini_response <- call_llm(
#'     config = gemini_config,
#'     messages = gemini_message,
#'     json = TRUE # Get raw JSON for inspection if needed
#'   )
#'
#'   # Display the generated text response
#'   cat("Gemini Response:", gemini_response, "\n")
#'
#'   # Access and print the raw JSON response
#'   raw_json_gemini_response <- attr(gemini_response, "raw_json")
#'   print(raw_json_gemini_response)
#' }
call_llm <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (config$troubleshooting == TRUE){
   print("\n\n Inside call_llm for troubleshooting\n")
   print("\nBE CAREFUL THIS BIT CONTAINS YOUR API KEY! DO NOT REPORT IT AS IS!")
   print(messages)
   print(config)
   print("\n\n")
   }
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
    # Check explicit embedding request
    if (isTRUE(config$embedding)) {
      return(call_llm.openai_embedding(config, messages, verbose, json))
    }

    # Check explicit non-embedding request (force chat mode)
    if (isFALSE(config$embedding)) {
      # Skip any auto-detection, go straight to chat completion
      endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/chat/completions")
      # ... existing chat logic continues ...
    }

    # If embedding is NULL, use existing OpenAI logic (backward compatibility)
    if (is.null(config$embedding)) {
      # Existing logic: check if api_url/endpoint suggests embeddings
      endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/chat/completions")
      if (grepl("embeddings", endpoint, ignore.case = TRUE)) {
        return(call_llm.openai_embedding(config, messages, verbose, json))
      }
      # Continue with existing chat logic...
    }
  # Use overwrite if provided, otherwise default to openai chat completions endpoint.
  # this can be used to call other providers which are openai-compatible
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
#' @keywords internal
call_llm.openai_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.openai.com/v1/embeddings")

  texts <- if (is.character(messages)) {
    messages
  } else {
    sapply(messages, function(msg) if (is.list(msg)) msg$content else as.character(msg))
  }

  body <- list(
    model = config$model,
    input = texts
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
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently built for Anthropic!")
  }
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
  # Check if this is an embedding request
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for Groq!")
  }
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
    # Check explicit embedding request
    if (isTRUE(config$embedding)) {
      return(call_llm.together_embedding(config, messages, verbose, json))
    }

    # Check explicit non-embedding request (force chat mode)
    # if (isFALSE(config$embedding)) {
    #   # Skip any auto-detection, go straight to chat completion
    # }
    # # If embedding is NULL, use existing Together AI logic (backward compatibility)
    # if (is.null(config$embedding)) {
    # }

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

#' Call LLM API for Together AI Embedding Models
#'
#' This function handles embedding requests specifically for Together AI embedding models.
#' It processes text inputs and returns embedding vectors using Together AI's embeddings API.
#'
#' @param config A configuration object created by llm_config() with provider="together"
#' @param messages Character vector of texts to embed, or list of message objects
#' @param verbose Logical indicating whether to print request details (default: FALSE)
#' @param json Logical indicating whether to return raw JSON response (default: FALSE)
#' @return List containing embedding data in standard LLMR format
#' @export
#' @keywords internal
call_llm.together_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/embeddings")

  # Handle both character vectors and message lists
  texts <- if (is.character(messages)) {
    messages
  } else {
    sapply(messages, function(msg) if (is.list(msg)) msg$content else as.character(msg))
  }

  body <- list(
    model = config$model,
    input = texts
  )

  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", config$api_key)
    ) |>
    httr2::req_body_json(body)

  if (verbose) {
    cat("Making embedding request to:", endpoint, "\n")
    cat("Model:", config$model, "\n")
    cat("Number of texts:", length(texts), "\n")
  }

  response <- perform_request(req, verbose, json)

  if (json) {
    return(response)
  }

  # Transform Together AI response to LLMR standard format
  embeddings_data <- lapply(response$data, function(item) {
    list(embedding = item$embedding)
  })

  return(list(data = embeddings_data))
}






#' @export
call_llm.deepseek <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for DeepSeek!")
  }
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
  # Check explicit embedding request
  if (isTRUE(config$embedding)) {
    return(call_llm.voyage_embedding(config, messages, verbose, json))
  }

  # Check explicit non-embedding request
  if (isFALSE(config$embedding)) {
    stop("Only embedding models are set up for Voyage")
  }

  # If embedding is NULL, use existing Voyage behavior (backward compatibility)
  if (is.null(config$embedding)) {
    # Existing behavior: Voyage is embeddings-only, so default to embeddings
    return(call_llm.voyage_embedding(config, messages, verbose, json))
  }

  # Fallback: default to embeddings (Voyage's specialty)
  return(call_llm.voyage_embedding(config, messages, verbose, json))
}

#' @export
#' @keywords internal
call_llm.voyage_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  # Move existing voyage logic here
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
    # Check explicit embedding request
    if (isTRUE(config$embedding)) {
      return(call_llm.gemini_embedding(config, messages, verbose, json))
    }

  # Auto-detect embedding models and route to embedding handler
    if (grepl("embedding", config$model, ignore.case = TRUE)) {
      return(call_llm.gemini_embedding(config, messages, verbose, json))
    }
  # Define the API endpoint using the model from config
  endpoint <- get_endpoint(config, default_endpoint = paste0("https://generativelanguage.googleapis.com/v1beta/models/", config$model, ":generateContent"))

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

  # Construct the request body with contents and optional systemInstruction
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

  # Build and send the HTTP request
  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-goog-api-key" = config$api_key
    ) |>
    httr2::req_body_json(body)

  # Perform the request and return the response
  perform_request(req, verbose, json)
}

# call_llm.gemini <- function(config, messages, verbose = FALSE, json = FALSE) {
#   endpoint <- get_endpoint(config, default_endpoint = paste0("https://generativelanguage.googleapis.com/v1beta/models/", config$model, ":generateContent"))
#
#   # Format messages for Gemini API
#   formatted_messages <- lapply(messages, function(msg) {
#     list(
#       role = msg$role,
#       parts = list(list(text = msg$content)) # Gemini expects content in 'parts' as a list of lists
#     )
#   })
#
#   body <- list(
#     contents = formatted_messages,
#     generationConfig = list(
#       temperature = rlang::`%||%`(config$model_params$temperature, 1),
#       maxOutputTokens = rlang::`%||%`(config$model_params$max_tokens, 1024), # Gemini uses maxOutputTokens
#       topP = rlang::`%||%`(config$model_params$top_p, 1),
#       topK = rlang::`%||%`(config$model_params$top_k, 1) # Added topK as it's common
#       # frequency_penalty and presence_penalty are not standard Gemini parameters.
#     )
#   )
#
#   req <- httr2::request(endpoint) |>
#     httr2::req_headers(
#       "Content-Type" = "application/json",
#       "x-goog-api-key" = config$api_key # Gemini uses x-goog-api-key header
#     ) |>
#     httr2::req_body_json(body)
#
#   perform_request(req, verbose, json)
# }

#' Call LLM API for Gemini Embedding Models
#'
#' This function handles embedding requests specifically for Gemini embedding models.
#' It processes text inputs and returns embedding vectors using Google's Generative Language API.
#'
#' @param config A configuration object created by llm_config() with provider="gemini"
#' @param messages Character vector of texts to embed, or list of message objects
#' @param verbose Logical indicating whether to print request details (default: FALSE)
#' @param json Logical indicating whether to return raw JSON response (default: FALSE)
#' @return List containing embedding data in standard LLMR format
#' @export
#' @keywords internal
call_llm.gemini_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", config$model, ":embedContent")

  # Handle both character vectors and message lists
  texts <- if (is.character(messages)) {
    messages
  } else {
    sapply(messages, function(msg) if (is.list(msg)) msg$content else as.character(msg))
  }

  results <- list()

  for (i in seq_along(texts)) {
    body <- list(
      content = list(
        parts = list(list(text = texts[i]))
      )
    )

    req <- httr2::request(endpoint) |>
      httr2::req_url_query(key = config$api_key) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body)

    if (verbose) {
      cat("Making embedding request to:", endpoint, "\n")
      cat("Text length:", nchar(texts[i]), "characters\n")
    }

    resp <- httr2::req_perform(req)

    if (json) {
      results[[i]] <- httr2::resp_body_json(resp)
    } else {
      result <- httr2::resp_body_json(resp)
      results[[i]] <- list(embedding = result$embedding$values)
    }
  }

  return(list(data = results))
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

#' Generate Embeddings in Batches
#'
#' A wrapper function that processes a list of texts in batches to generate embeddings,
#' avoiding rate limits. This function calls \code{\link{call_llm_robust}} for each
#' batch and stitches the results together.
#'
#' @param texts Character vector of texts to embed.
#' @param embed_config An \code{llm_config} object configured for embeddings.
#' @param batch_size Integer. Number of texts to process in each batch. Default is 5.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#'
#' @return A numeric matrix where each row is an embedding vector for the corresponding text.
#'   If embedding fails for certain texts, those rows will be filled with NA values.
#'   The matrix will always have the same number of rows as the input texts.
#'   Returns NULL if no embeddings were successfully generated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   texts <- c("Hello world", "How are you?", "Machine learning is great")
#'
#'   embed_cfg <- llm_config(
#'     provider = "voyage",
#'     model = "voyage-3-large",
#'     api_key = Sys.getenv("VOYAGE_KEY")
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
                                   batch_size = 5,
                                   verbose = TRUE) {

  # Input validation
  if (length(texts) == 0) {
    if (verbose) message("No texts provided. Returning NULL.")
    return(NULL)
  }

  # Setup
  n_docs <- length(texts)
  batches <- split(seq_len(n_docs), ceiling(seq_len(n_docs) / batch_size))
  emb_list <- vector("list", n_docs)

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
      # Call LLM for this batch
      resp <- call_llm_robust(embed_config, batch_texts, verbose = FALSE)

      # Parse embeddings and transpose to get one row per text
      emb_chunk <- parse_embeddings(resp) |> t()

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

  # Check if we have any successful embeddings
  if (all(vapply(emb_list, function(x) length(x) == 1 && is.na(x), TRUE))) {
    if (verbose) message("No embeddings were successfully generated.")
    return(NULL)
  }

  # Combine all embeddings into final matrix
  final_embeddings <- do.call(rbind, emb_list)

  if(!is.null(names(texts))){
    row.names(final_embeddings) = names(texts)
  }

  if (verbose) {
    n_successful <- sum(!is.na(final_embeddings[, 1]))
    message("Successfully generated embeddings for ", n_successful,
            "/", n_docs, " texts (", ncol(final_embeddings), " dimensions)")
  }

  return(final_embeddings)
}
