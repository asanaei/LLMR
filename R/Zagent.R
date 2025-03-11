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
  # We only care about AgentAction chaining now
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
  # Prompt that agent
  e1$converse(
    agent_id        = agent_id,
    prompt_template = e2$prompt_template,
    replacements    = e2$replacements,
    verbose         = e2$verbose
  )
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
#' The prompt template can include reserved placeholders that are automatically substituted:
#' \itemize{
#'   \item \code{\{\{conversation\}\}}: Inserts the current conversation history.
#'   \item \code{\{\{last_output\}\}}: Inserts the output from the last agent.
#'   \item \code{\{\{topic\}\}}: Inserts the conversation topic.
#'   \item Any key from the agent's \code{persona} list (e.g., \code{\{\{party\}\}}).
#' }
#'
#' Additional custom placeholders can be provided via the \code{replacements} argument.
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
#' Each agent maintains its own memory, persona, and model configuration for
#' consistent and contextual interactions with a language model.
#'
#' The `Agent` class enables the creation of agents with defined personas
#' (e.g., ideology, verbosity, role) and memory (conversation history). In the new version,
#' the persona is pushed to the LLM via a system message instead of being concatenated
#' into the prompt. All previous functionality (e.g., thinking and responding) is preserved,
#' with the old `knowledge` parameter replaced by `persona`.
#'
#' Additionally, dynamic memory summarization can be enabled to handle longer
#' contexts gracefully. If the memory exceeds a threshold, the agent will
#' summarize its past memory.
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
    #' @field memory A list of speaker/text pairs that the agent has "memorized."
    memory = list(),
    #' @field persona Named list for additional agent-specific persona details.
    persona = list(),
    #' @field enable_summarization Toggle whether memory summarization is used when threshold is exceeded.
    enable_summarization = TRUE,
    #' @field summarization_threshold The maximum number of memory items before summarization is triggered.
    summarization_threshold = 5,

    #' @description
    #' Create a new Agent instance.
    #'
    #' @param id Character. The agent's unique identifier.
    #' @param context_length Numeric. The maximum memory length (default 5).
    #' @param persona A named list of persona details (replaces old "knowledge").
    #' @param model_config An \code{llm_config} object specifying LLM settings.
    #' @param enable_summarization Logical, if \code{TRUE}, agent will attempt to summarize
    #'   memory when it exceeds the summarization threshold. Default \code{TRUE}.
    #'
    #' @return A new \code{Agent} object.
    initialize = function(id,
                          context_length = 5,
                          persona = NULL,
                          model_config,
                          enable_summarization = TRUE) {

      if (missing(id)) stop("Agent id is required.")
      if (missing(model_config)) stop("model_config is required.")
      if (!inherits(model_config, "llm_config")) {
        stop("model_config must be an llm_config object.")
      }

      self$id                    <- id
      self$context_length        <- context_length
      self$persona               <- persona %||% list()
      self$model_config          <- model_config
      self$memory                <- list()
      self$enable_summarization  <- enable_summarization
      # By default, set the summarization threshold to match the context length
      self$summarization_threshold <- context_length
    },

    #' @description
    #' Add a new message to the agent's memory. If summarization is enabled and the memory
    #' size exceeds the threshold, it automatically summarizes the memory.
    #'
    #' @param speaker Character. The speaker name or ID.
    #' @param text Character. The message content.
    add_memory = function(speaker, text) {
      self$memory <- append(self$memory, list(list(speaker = speaker, text = text)))

      # If summarization is enabled and memory is large, summarize it
      if (self$enable_summarization && length(self$memory) > self$summarization_threshold) {
        self$maybe_summarize_memory()
      }
    },

    #' @description
    #' Summarize the current memory by calling the LLM, then replace the old memory
    #' with the single summarized block. This helps keep the agent's memory manageable.
    maybe_summarize_memory = function() {
      conversation <- paste(
        sapply(self$memory, function(msg) paste0(msg$speaker, ": ", msg$text)),
        collapse = "\n"
      )
      prompt <- paste(
        "Summarize the following conversation succinctly, capturing only key points:\n\n",
        conversation
      )

      # Call LLM to obtain summary
      summary_result <- self$call_llm_agent(prompt, verbose = FALSE)$text

      # Replace the entire memory with a single summary line
      self$reset_memory()
      self$add_memory("summary", summary_result)
    },

    #' @description
    #' Replace placeholders in a prompt template with values from `replacements`.
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
    #' Call the underlying LLM with a plain text `prompt`.
    #' If a persona is defined, it is pushed as a system message.
    #'
    #' @param prompt Character. The final prompt to send.
    #' @param verbose Logical. If TRUE, prints verbose info. Default FALSE.
    #'
    #' @return A list with $text (the LLM response) plus token usage, etc.
    call_llm_agent = function(prompt, verbose = FALSE) {

      # If persona is defined, push it as a system message
      if (length(self$persona) > 0) {
        role <- self$persona$role %||% "agent"
        other_attrs <- self$persona[names(self$persona) != "role"]
        attrs_str <- if (length(other_attrs) > 0) {
          paste(names(other_attrs), "=", other_attrs, collapse = ", ")
        } else {
          ""
        }
        persona_str <- paste0('Pretend that you are a "', role, '"',
                              if(nchar(attrs_str) > 0) paste0(" with the following description: ", attrs_str, ".") else ".")
        messages <- list(
          list(role = "system", content = persona_str),
          list(role = "user",   content = prompt)
        )
      } else {
        messages <- list(list(role = "user", content = prompt))
      }

      tryCatch({
        response   <- call_llm(self$model_config, messages, json = TRUE, verbose = verbose)
        full_resp  <- attr(response, "full_response")
        text_out   <- ""
        token_info <- list(tokens_sent = 0, tokens_received = 0)

        # Extract text from the response
        tryCatch({
          text_out <- private$extract_text_from_response(full_resp)
        }, error = function(e) {
          warning("Error extracting text: ", e$message)
        })

        # Extract token usage
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
      }, error = function(e) {
        stop("LLM API call failed: ", e$message)
      })
    },

    #' @description
    #' Generate an LLM response using a prompt template and optional replacements.
    #' @param prompt_template Character. The prompt template.
    #' @param replacements A named list for placeholder substitution.
    #' @param verbose Logical. If TRUE, prints extra info. Default FALSE.
    #' @return A list with $text, $tokens_sent, $tokens_received, and $full_response.
    generate = function(prompt_template, replacements = list(), verbose = FALSE) {
      prompt <- self$generate_prompt(prompt_template, replacements)
      out    <- self$call_llm_agent(prompt, verbose)
      # Add the agent's newly generated text to memory
      self$add_memory(self$id, out$text)
      out
    },

    #' @description
    #' Have the agent produce an "internal" thought about a topic using its memory.
    #' @param topic Character. A short description or label for the thought.
    #' @param prompt_template Character. The prompt template.
    #' @param replacements A named list of additional placeholder values.
    #' @param verbose Logical. If TRUE, prints extra info. Default FALSE.
    #' @return A list with the LLM's response and related metadata.
    think = function(topic, prompt_template, replacements = list(), verbose = FALSE) {
      if (missing(topic)) stop("Topic is required for thinking.")
      if (missing(prompt_template)) stop("Prompt template is required for thinking.")

      conversation <- paste(
        sapply(self$memory, function(msg) paste0(msg$speaker, ": ", msg$text)),
        collapse = "\n"
      )
      last_output <- if (length(self$memory) > 0) {
        self$memory[[length(self$memory)]]$text
      } else {
        ""
      }

      # Merge placeholders with persona
      full_replacements <- c(
        list(
          topic        = topic,
          conversation = conversation,
          last_output  = last_output
        ),
        replacements,
        self$persona
      )

      self$generate(prompt_template, full_replacements, verbose)
    },

    #' @description
    #' Have the agent produce a "public" answer or response about a topic.
    #' @param topic Character. A short label for the question or request.
    #' @param prompt_template Character. The prompt template.
    #' @param replacements A named list for placeholder substitution.
    #' @param verbose Logical. If TRUE, prints extra info. Default FALSE.
    #' @return A list with $text, $tokens_sent, $tokens_received, and $full_response.
    respond = function(topic, prompt_template, replacements = list(), verbose = FALSE) {
      if (missing(topic)) stop("Topic is required for responding.")
      if (missing(prompt_template)) stop("Prompt template is required for responding.")

      conversation <- paste(
        sapply(self$memory, function(msg) paste0(msg$speaker, ": ", msg$text)),
        collapse = "\n"
      )
      last_output <- if (length(self$memory) > 0) {
        self$memory[[length(self$memory)]]$text
      } else {
        ""
      }

      # Merge placeholders with persona
      full_replacements <- c(
        list(
          topic        = topic,
          conversation = conversation,
          last_output  = last_output
        ),
        replacements,
        self$persona
      )

      self$generate(prompt_template, full_replacements, verbose)
    },

    #' @description
    #' Reset the agent's memory (clear any stored conversation context).
    reset_memory = function() {
      self$memory <- list()
    }
  ),

  private = list(
    # Extract token counts from the API response based on provider.
    extract_token_counts = function(response, provider) {
      usage <- response$usage %||% NULL
      if (is.null(usage)) {
        return(list(tokens_sent = 0, tokens_received = 0))
      }
      tokens_sent <- switch(
        provider,
        "openai"    = usage$prompt_tokens,
        "anthropic" = usage$input_tokens,
        "groq"      = usage$prompt_tokens,
        "gemini"    = usage$prompt_tokens,
        usage$prompt_tokens %||% 0
      )
      tokens_received <- switch(
        provider,
        "openai"    = usage$completion_tokens,
        "anthropic" = usage$output_tokens,
        "groq"      = usage$completion_tokens,
        "gemini"    = usage$completion_tokens,
        usage$completion_tokens %||% 0
      )
      list(
        tokens_sent     = as.numeric(tokens_sent %||% 0),
        tokens_received = as.numeric(tokens_received %||% 0)
      )
    },

    # Extract text from the API response in a provider-specific manner.
    extract_text_from_response = function(response) {
      if (is.null(response)) return("")
      # OpenAI style
      if (!is.null(response$choices) && length(response$choices) > 0) {
        choice <- response$choices[[1]]
        if (is.list(choice$message) && !is.null(choice$message$content)) {
          return(as.character(choice$message$content))
        }
        if (!is.null(choice$text)) {
          return(as.character(choice$text))
        }
      }
      # Anthropic style
      if (!is.null(response$content) && length(response$content) > 0) {
        content_item <- response$content[[1]]
        if (!is.null(content_item$text)) {
          return(as.character(content_item$text))
        }
      }
      # Gemini style
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
#' Holds:
#' \itemize{
#'   \item \code{agents}: Named list of Agents, keyed by \code{agent$id}.
#'   \item \code{conversation_history}: The full log of messages so far.
#'   \item \code{topic}: A short label or theme for the conversation.
#'   \item \code{prompts}: Optional named list of prompt templates.
#'   \item \code{shared_memory}: A global shared memory that is also fed into agents.
#' }
#'
#' Agents do \emph{not} automatically share memory. Whenever you call \code{converse()},
#' the entire conversation history and any \code{shared_memory} items are
#' \emph{temporarily} loaded into that agent's memory. Then the agent responds, and
#' we prune again. This new \code{shared_memory} field allows a global store for items
#' that any agent can read.
#'
#' @export
LLMConversation <- R6::R6Class(
  "LLMConversation",
  public = list(
    #' @field agents A named list of \code{Agent} objects.
    agents = list(),
    #' @field conversation_history A list of speaker/text pairs for the entire conversation.
    conversation_history = list(),
    #' @field topic A short string describing the conversation's theme.
    topic = NULL,
    #' @field prompts An optional list of prompt templates (may be ignored).
    prompts = NULL,
    #' @field shared_memory Global store that is also fed into each agent's memory at conversation time.
    shared_memory = list(),

    #' @description
    #' Create a new conversation.
    #' @param topic Character. The conversation topic.
    #' @param prompts Optional named list of prompt templates.
    initialize = function(topic, prompts = NULL) {
      if (missing(topic)) stop("Conversation topic is required.")
      self$topic                <- topic
      self$prompts              <- prompts
      self$agents               <- list()
      self$conversation_history <- list()
      self$shared_memory        <- list()
    },

    #' @description
    #' Add an \code{Agent} to this conversation. The agent is stored by its \code{id}.
    #' @param agent The \code{Agent} to add.
    add_agent = function(agent) {
      if (!inherits(agent, "Agent")) {
        stop("add_agent() requires an object of class Agent.")
      }
      self$agents[[agent$id]] <- agent
    },

    #' @description
    #' Add a message to the global conversation log. Also appends to shared memory.
    #' @param speaker Character. Who is speaking?
    #' @param text Character. What they said.
    add_message = function(speaker, text) {
      self$conversation_history <- append(
        self$conversation_history,
        list(list(speaker = speaker, text = text))
      )
      # Also update shared memory with the new message
      self$shared_memory <- append(
        self$shared_memory,
        list(list(speaker = speaker, text = text))
      )
    },

    #' @description
    #' Have a specific agent produce a response. The entire global conversation so far
    #' plus the shared memory is temporarily loaded into that agent, which responds. Then
    #' the new message is recorded in the conversation, and the agent's memory is reset.
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

      # Temporarily feed entire conv history to the agent
      for (msg in self$conversation_history) {
        agent$add_memory(msg$speaker, msg$text)
      }
      # Also feed shared memory
      for (msg in self$shared_memory) {
        agent$add_memory("shared", msg$text)
      }

      # Let the agent produce a response
      response <- agent$respond(self$topic, prompt_template, replacements, verbose)

      # Store it globally
      self$add_message(agent_id, response$text)

      # Trim agent's memory, re-add only the final new line
      agent$reset_memory()
      agent$add_memory(agent_id, response$text)
      invisible(response)
    },

    #' @description
    #' Run a multi-step conversation among a sequence of agents.
    #' @param agent_sequence Character vector of agent IDs in the order they will speak.
    #' @param prompt_template Either a single string or a named list of templates (keyed by agent ID).
    #' @param replacements Either a single list or a list-of-lists with per-agent placeholders.
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
        current_reps   <- if (is.list(replacements) && length(replacements) == length(agent_sequence)) {
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
    #' Clear the global conversation and reset all agents' memories. Also clears shared memory.
    reset_conversation = function() {
      self$conversation_history <- list()
      self$shared_memory        <- list()
      for (agent in self$agents) {
        agent$reset_memory()
      }
    },

    #' @description
    #' A pipe-like operator to chain conversation steps. E.g.,
    #'   conv |> "Solver"(prompt_template, replacements)
    #'
    #' @param agent_id Character. The ID of the agent to call next.
    #'
    #' @return A function that expects \code{(prompt_template, replacements, verbose)}.
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
    }
  )
)


