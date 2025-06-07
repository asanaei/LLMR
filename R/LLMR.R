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

    if (!is.null(content$candidates)) {
      # For Gemini API
      if (length(content$candidates) == 0 ||
          is.null(content$candidates[[1]]$content$parts) ||
          length(content$candidates[[1]]$content$parts) == 0 ||
          is.null(content$candidates[[1]]$content$parts[[1]]$text)) {
        return(NA_character_)
      }
      return(content$candidates[[1]]$content$parts[[1]]$text)
    }

    # Fallback - return content as-is instead of throwing error
    return(content)
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
#' @export
#' @examples
#' \dontrun{
#'   openai_config <- llm_config(
#'     provider = "openai",
#'     model = "gpt-4o-mini",
#'     api_key = Sys.getenv("OPENAI_API_KEY"),
#'     temperature = 0.7,
#'     max_tokens = 500)
#' }
llm_config <- function(provider, model, api_key, troubleshooting = FALSE, base_url = NULL, embedding = NULL, ...) {
  model_params <- list(...)
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
#' Sends a message to the specified LLM API and retrieves the response.
#'
#' @param config An `llm_config` object created by `llm_config()`.
#' @param messages A list of message objects (or a character vector for embeddings).
#'   For multimodal requests, the `content` of a message can be a list of parts,
#'   e.g., `list(list(type="text", text="..."), list(type="file", path="..."))`.
#' @param verbose Logical. If `TRUE`, prints the full API response.
#' @param json Logical. If `TRUE`, the returned text will have the raw JSON response
#'   and the parsed list as attributes.
#'
#' @return The generated text response or embedding results. If `json=TRUE`,
#'   attributes `raw_json` and `full_response` are attached.
#' @export
#' @examples
#' \dontrun{
#'   # Standard text call
#'   config <- llm_config(provider = "openai", model = "gpt-4o-mini", api_key = "...")
#'   messages <- list(list(role = "user", content = "Hello!"))
#'   response <- call_llm(config, messages)
#'
#'   # Multimodal call (for supported providers like Gemini, Claude 3, GPT-4o)
#'   # Make sure to use a vision-capable model in your config
#'   multimodal_config <- llm_config(provider = "openai", model = "gpt-4o", api_key = "...")
#'   multimodal_messages <- list(list(role = "user", content = list(
#'     list(type = "text", text = "What is in this image?"),
#'     list(type = "file", path = "path/to/your/image.png")
#'   )))
#'   image_response <- call_llm(multimodal_config, multimodal_messages)
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

  body <- list(
    model = config$model,
    messages = formatted_messages,
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
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for Anthropic!")
  }
  endpoint <- get_endpoint(config, default_endpoint = "https://api.anthropic.com/v1/messages")

  # Use the helper to separate system messages
  formatted <- format_anthropic_messages(messages)

  # Process user messages for multimodal content
  processed_user_messages <- lapply(formatted$user_messages, function(msg) {
    content_blocks <- list()
    if (is.character(msg$content)) {
      content_blocks <- list(list(type = "text", text = msg$content))
    } else if (is.list(msg$content)) {
      content_blocks <- lapply(msg$content, function(part) {
        if (part$type == "text") {
          return(list(type = "text", text = part$text))
        } else if (part$type == "file") {
          file_data <- .process_file_content(part$path)
          return(list(
            type = "image",
            source = list(
              type = "base64",
              media_type = file_data$mime_type,
              data = file_data$base64_data
            )
          ))
        } else {
          return(NULL)
        }
      })
      content_blocks <- purrr::compact(content_blocks)
    }
    list(role = msg$role, content = content_blocks)
  })

  body <- list(
    model = config$model,
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    temperature = rlang::`%||%`(config$model_params$temperature, 1),
    messages = processed_user_messages
  )
  if (!is.null(formatted$system_text)) {
    body$system <- formatted$system_text
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
call_llm.gemini <- function(config, messages, verbose = FALSE, json = FALSE) {
  if (isTRUE(config$embedding) || grepl("embedding", config$model, ignore.case = TRUE)) {
    return(call_llm.gemini_embedding(config, messages, verbose, json))
  }

  endpoint <- get_endpoint(config, default_endpoint = paste0("https://generativelanguage.googleapis.com/v1beta/models/", config$model, ":generateContent"))

  system_messages <- purrr::keep(messages, ~ .x$role == "system")
  other_messages <- purrr::keep(messages, ~ .x$role != "system")
  system_instruction <- if (length(system_messages) > 0) {
    list(parts = list(list(text = paste(sapply(system_messages, function(x) x$content), collapse = " "))))
  } else {
    NULL
  }

  formatted_messages <- lapply(other_messages, function(msg) {
    role <- if (msg$role == "assistant") "model" else "user"
    content_parts <- list()
    if (is.character(msg$content)) {
      content_parts <- list(list(text = msg$content))
    } else if (is.list(msg$content)) {
      content_parts <- lapply(msg$content, function(part) {
        if (part$type == "text") {
          return(list(text = part$text))
        } else if (part$type == "file") {
          file_data <- .process_file_content(part$path)
          return(list(inlineData = list(mimeType = file_data$mime_type, data = file_data$base64_data)))
        } else {
          return(NULL)
        }
      })
      content_parts <- purrr::compact(content_parts)
    }
    list(role = role, parts = content_parts)
  })

  body <- list(
    contents = formatted_messages,
    generationConfig = list(
      temperature = rlang::`%||%`(config$model_params$temperature, 1),
      maxOutputTokens = rlang::`%||%`(config$model_params$max_tokens, 2048),
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


# ----- Unmodified Provider Functions (for non-vision tasks) -----

#' @export
call_llm.groq <- function(config, messages, verbose = FALSE, json = FALSE) {
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
  if (isTRUE(config$embedding)) {
    return(call_llm.together_embedding(config, messages, verbose, json))
  }
  endpoint <- get_endpoint(config, default_endpoint = "https://api.together.xyz/v1/chat/completions")
  body <- list(
    model = config$model,
    messages = messages,
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    temperature = rlang::`%||%`(config$model_params$temperature, 0.7),
    top_p = rlang::`%||%`(config$model_params$top_p, 0.7),
    top_k = rlang::`%||%`(config$model_params$top_k, 50),
    repetition_penalty = rlang::`%||%`(config$model_params$repetition_penalty, 1)
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
  if (isTRUE(config$embedding)) {
    stop("Embedding models are not currently supported for DeepSeek!")
  }
  endpoint <- get_endpoint(config, default_endpoint = "https://api.deepseek.com/chat/completions")
  body <- list(
    model = rlang::`%||%`(config$model, "deepseek-chat"),
    messages = messages,
    temperature = rlang::`%||%`(config$model_params$temperature, 1),
    max_tokens = rlang::`%||%`(config$model_params$max_tokens, 1024),
    top_p = rlang::`%||%`(config$model_params$top_p, 1)
  )
  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
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
call_llm.gemini_embedding <- function(config, messages, verbose = FALSE, json = FALSE) {
  # Endpoint for single content embedding
  endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", config$model, ":embedContent")

  # Handle both character vectors and message lists to get texts
  texts <- if (is.character(messages)) {
    messages
  } else {
    # Assuming messages is a list of lists like list(list(role="user", content="..."))
    # or just a list of character strings if not strictly following message format.
    # This part might need adjustment if 'messages' can be complex lists.
    # For get_batched_embeddings, 'messages' will be a character vector (batch_texts).
    sapply(messages, function(msg) {
      if (is.list(msg) && !is.null(msg$content)) {
        msg$content
      } else if (is.character(msg)) {
        msg
      } else {
        as.character(msg) # Fallback
      }
    })
  }

  results <- vector("list", length(texts)) # Pre-allocate list

  for (i in seq_along(texts)) {
    current_text <- texts[i]
    body <- list(
      content = list(
        parts = list(list(text = current_text))
      )
    )

    req <- httr2::request(endpoint) |>
      httr2::req_url_query(key = config$api_key) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body)

    if (verbose) {
      cat("Making single embedding request to:", endpoint, "for text", i, "\n")
      # cat("Text snippet:", substr(current_text, 1, 50), "...\n") # Optional: for debugging
    }

    api_response_content <- NULL # Initialize
    tryCatch({
      resp <- httr2::req_perform(req)
      api_response_content <- httr2::resp_body_json(resp)
    }, error = function(e) {
      if (verbose) {
        message("LLMR Error during Gemini embedding for text ", i, ": ", conditionMessage(e))
      }
      # api_response_content remains NULL if error
    })

    if (!is.null(api_response_content) && !is.null(api_response_content$embedding$values)) {
      results[[i]] <- list(embedding = api_response_content$embedding$values)
    } else {
      # Store a placeholder that parse_embeddings can handle (e.g., leading to NAs)
      # The dimension of NA_real_ doesn't matter here, as parse_embeddings and get_batched_embeddings
      # will determine dimensionality from successful calls or handle it.
      results[[i]] <- list(embedding = NA_real_)
      if (verbose && !is.null(api_response_content)) {
        message("Unexpected response structure or missing embedding values for text ", i)
      } else if (verbose && is.null(api_response_content)) {
        message("No response content (likely due to API error) for text ", i)
      }
    }
  }

  # The 'json' parameter in the function signature is a bit tricky here.
  # we ignore it here
  final_output <- list(data = results)
  # If json=TRUE was intended to get the raw responses, this structure doesn't fully provide that
  # but this was really made for the generative calls!
  return(final_output)
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
#' batch and stitches the results together.
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
