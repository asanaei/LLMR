#################
# Zagent.R
#################

# -----------------------------------------------------------------------------
# UTILITY
# -----------------------------------------------------------------------------

#' @keywords internal
# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x


# -----------------------------------------------------------------------------
# S3 "PLUS" DISPATCH FOR LLMConversation
# -----------------------------------------------------------------------------
# Because `+` in R is an S3 generic that dispatches on the class of the FIRST
# argument, we define `+.LLMConversation` to dispatch on the second argument's class.
# This solves the "non-numeric argument" issue when doing:
#     conv + AgentAction(...) + AgentAction(...)

#' @export
`+.LLMConversation` <- function(e1, e2) {
  # We only care about AgentAction chaining now
  if (inherits(e2, "AgentAction")) {
    return(`+.LLMConversation.AgentAction`(e1, e2))
  } else {
    # Anything else is not supported
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
  # Return updated conversation
  e1
}

# -----------------------------------------------------------------------------
# WE REMOVE Ops.LLMConversation & Ops.Agent
# -----------------------------------------------------------------------------

# No more support for conv + agent or agent + agent.

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
#'   \item Any key from the agent's \code{knowledge} list (e.g., \code{\{\{party\}\}} or \code{\{\{role\}\}})
#'         is also available.
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
#' @details
#' The `Agent` class enables the creation of agents with defined personas
#' (e.g., role, ideology, age) and memory (conversation history). The persona
#' is automatically prepended to prompts sent to the language model, while
#' memory can be included in prompts via the `{{conversation}}` placeholder.
#' This allows for personalized and context-aware responses from the LLM.
#'
#' @examples
#' \dontrun{
#' # Define a model configuration for OpenAI's gpt-4o-mini
#' agentcfg <- llm_config(
#'   model = "gpt-4o-mini",
#'   provider = "openai",
#'   api_key = Sys.getenv("OPENAI_KEY")
#' )
#'
#' # Create an agent with a specific persona
#' analyst_agent <- Agent$new(
#'   id = "analyst",
#'   model_config = agentcfg,
#'   persona = list(
#'     role = "data analyst",
#'     experience = "10 years",
#'     style = "concise"
#'   )
#' )
#'
#' # Generate a response with a specified topic
#' response <- analyst_agent$respond(
#'   topic = "data trends",
#'   prompt_template = "Provide your thoughts on {{topic}} based on your experience."
#' )
#'
#' # The resulting prompt sent to the LLM will be:
#' # "Pretend that you are a \"data analyst\" with the following description: experience = 10 years, style = concise.
#' # Provide your thoughts on data trends based on your experience."
#'
#' # Access the response text
#' print(response$text)
#'
#' # Generate a response without specifying a topic (uses default)
#' response_default <- analyst_agent$respond(
#'   prompt_template = "Discuss the {{topic}}."
#' )
#'
#' # The resulting prompt will be:
#' # "Pretend that you are a \"data analyst\" with the following description: experience = 10 years, style = concise.
#' # Discuss the conversation with simulated agents."
#' }
#'
#' @export
Agent <- R6::R6Class(
  "Agent",
  public = list(
    #' @field id Unique identifier for the agent.
    id = NULL,
    #' @field context_length Maximum number of conversation turns stored in memory.
    context_length = 5,
    #' @field model_config Configuration object specifying the LLM to use.
    model_config = NULL,
    #' @field memory List of speaker/text pairs representing conversation history.
    memory = list(),
    #' @field persona Named list defining the agent's characteristics (e.g., role, traits).
    persona = list(),

    #' @description
    #' Initialize a new Agent instance.
    #' @param id Character. Unique identifier for the agent.
    #' @param context_length Numeric. Maximum number of conversation turns to retain (default 5).
    #' @param persona Named list. Characteristics defining the agent’s persona (optional).
    #' @param model_config Object of class `llm_config`. Specifies LLM settings.
    initialize = function(id, context_length = 5, persona = NULL, model_config) {
      if (missing(id)) stop("Agent id is required.")
      if (missing(model_config)) stop("model_config is required.")
      if (!inherits(model_config, "llm_config")) {
        stop("model_config must be an llm_config object.")
      }
      self$id             <- id
      self$context_length <- context_length
      self$persona        <- persona %||% list()
      self$model_config   <- model_config
      self$memory         <- list()
    },

    #' @description
    #' Add a message to the agent’s memory.
    #' @param speaker Character. Identifier of the speaker.
    #' @param text Character. Content of the message.
    add_memory = function(speaker, text) {
      if (length(self$memory) >= self$context_length) {
        self$memory <- self$memory[-1]
      }
      self$memory <- append(self$memory, list(list(speaker = speaker, text = text)))
    },

    #' @description
    #' Substitute placeholders in a prompt template with provided values.
    #' @param template Character. Prompt template containing placeholders like `{{name}}`.
    #' @param replacements Named list. Values to replace placeholders.
    #' @return Character. The prompt with all placeholders substituted.
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
    #' Send a prompt to the configured LLM and retrieve the response.
    #' @param prompt Character. The complete prompt to send to the LLM.
    #' @param verbose Logical. If TRUE, prints additional details (default FALSE).
    #' @return List. Contains `$text` (LLM response), `$tokens_sent`, `$tokens_received`, and `$full_response`.
    call_llm_agent = function(prompt, verbose = FALSE) {
      messages <- list(list(role = "user", content = prompt))
      tryCatch({
        response   <- call_llm(self$model_config, messages, json = TRUE, verbose = verbose)
        full_resp  <- attr(response, "full_response")
        text_out   <- ""
        token_info <- list(tokens_sent = 0, tokens_received = 0)
        tryCatch({
          text_out <- private$extract_text_from_response(full_resp)
        }, error = function(e) {
          warning("Error extracting text: ", e$message)
        })
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
    #' @param prompt_template Character. Template for the prompt with placeholders.
    #' @param replacements Named list. Values to substitute into the prompt template.
    #' @param verbose Logical. If TRUE, prints additional details (default FALSE).
    #' @return List. Contains `$text`, `$tokens_sent`, `$tokens_received`, and `$full_response`.
    generate = function(prompt_template, replacements = list(), verbose = FALSE) {
      prompt <- self$generate_prompt(prompt_template, replacements)
      out    <- self$call_llm_agent(prompt, verbose)
      self$add_memory(self$id, out$text)
      out
    },

    #' @description
    #' Generate a response from the agent about a topic, incorporating its persona.
    #' @param topic Character. Subject or question for the agent to address. If not provided, defaults to "conversation with simulated agents".
    #' @param prompt_template Character. Template for the prompt with placeholders.
    #' @param replacements Named list. Additional placeholder values (optional).
    #' @param verbose Logical. If TRUE, prints additional details (default FALSE).
    #' @return List. Contains `$text`, `$tokens_sent`, `$tokens_received`, and `$full_response`.
    respond = function(topic = "conversation with simulated agents", prompt_template, replacements = list(), verbose = FALSE) {
      if (missing(prompt_template)) stop("Prompt template is required for responding.")

      # Construct persona description for the prompt
      persona_str <- if (length(self$persona) > 0) {
        role <- self$persona$role %||% "agent"
        other_attrs <- self$persona[names(self$persona) != "role"]
        attrs_str <- if (length(other_attrs) > 0) {
          paste(names(other_attrs), "=", other_attrs, collapse = ", ")
        } else {
          ""
        }
        paste0('Pretend that you are a "', role, '" with the following description: ', attrs_str, ".")
      } else {
        ""
      }

      # Combine persona with the prompt template
      full_prompt_template <- if (nchar(persona_str) > 0) {
        paste(persona_str, prompt_template, sep = "\n")
      } else {
        prompt_template
      }

      # Aggregate memory into a conversation string
      conversation <- paste(
        sapply(self$memory, function(msg) paste0(msg$speaker, ": ", msg$text)),
        collapse = "\n"
      )
      last_output <- if (length(self$memory) > 0) {
        self$memory[[length(self$memory)]]$text
      } else {
        ""
      }

      # Assemble all replacements for the prompt
      full_replacements <- c(
        list(
          topic        = topic,
          conversation = conversation,
          last_output  = last_output
        ),
        replacements,
        self$persona
      )
      self$generate(full_prompt_template, full_replacements, verbose)
    },

    #' @description
    #' Clear the agent’s conversation memory.
    reset_memory = function() {
      self$memory <- list()
    }
  ),
  private = list(
    extract_text_from_response = function(response) {
      # Implementation details for extracting text from the LLM response.
    },
    extract_token_counts = function(response, provider) {
      # Implementation details for extracting token usage from the LLM response.
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
#' }
#'
#' Agents do \emph{not} automatically share memory. Whenever you call \code{converse()},
#' the entire conversation history is \emph{temporarily} loaded into the targeted
#' agent, which responds, then is pruned again.
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
    #' Add a message to the global conversation log.
    #' @param speaker Character. Who is speaking?
    #' @param text Character. What they said.
    add_message = function(speaker, text) {
      self$conversation_history <- append(
        self$conversation_history,
        list(list(speaker = speaker, text = text))
      )
    },
    #' @description
    #' Have a specific agent produce a response. The entire global conversation so far
    #' is temporarily loaded into that agent's memory, the agent responds, and then
    #' we store the agent's new message in this conversation.
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
    #' @param prompt_template Either a single string or a named list of templates.
    #' @param replacements Either a single list or a list-of-lists with per-agent placeholders.
    #' @param verbose Logical. If TRUE, prints extra info.
    run = function(agent_sequence, prompt_template, replacements = list(), verbose = FALSE) {
      if (is.null(agent_sequence)) {
        stop("agent_sequence cannot be NULL.")
      }
      for (i in seq_along(agent_sequence)) {
        agent_id <- agent_sequence[i]
        # If prompt_template is a named list, use that for each agent ID
        current_prompt <- if (is.list(prompt_template)) {
          prompt_template[[agent_id]] %||% stop("No prompt for agent: ", agent_id)
        } else {
          prompt_template
        }
        # If replacements is a list-of-lists, index by i
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
    #' Clear the global conversation and reset all agent memories.
    reset_conversation = function() {
      self$conversation_history <- list()
      for (agent in self$agents) {
        agent$reset_memory()
      }
    },
    #' @description
    #' A pipe-like operator to chain conversation steps. E.g.,
    #' \code{conv |> "Solver"(prompt_template, replacements)}.
    #' @param agent_id Character. The ID of the agent to call next.
    `|>` = function(agent_id) {
      if (!is.character(agent_id) || length(agent_id) != 1) {
        stop("agent_id must be a single character string.")
      }
      if (is.null(self$agents[[agent_id]])) {
        stop("Agent ", agent_id, " not in conversation.")
      }
      force(agent_id)  # ensure it's captured
      function(prompt_template, replacements = list(), verbose = FALSE) {
        self$converse(agent_id, prompt_template, replacements, verbose)
      }
    }
  )
)
