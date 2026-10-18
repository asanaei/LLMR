# tools.R ---------------------------------------------------------------------
# Native tool (function) calling: definitions, extraction, and an execution
# loop. The model decides WHEN to call a tool; LLMR runs the R function and
# feeds the result back until the model produces a final text answer.

#' Define a tool the model may call
#'
#' Wraps an R function with the name, description, and JSON-Schema argument
#' specification that providers need for native tool calling. Pass a list of
#' tools to [call_llm_tools()] (or to a `chat_session()` via that function),
#' which executes the calls the model makes and returns the model's final
#' answer.
#'
#' @param fn The R function to expose. It is called with the model's arguments
#'   matched by name, so use the same parameter names as in `parameters`.
#' @param name Tool name shown to the model (letters, digits, `_`, `-`).
#' @param description One or two sentences telling the model what the tool does
#'   and when to use it. Write it for the model, not for a human reader; it is
#'   the only documentation the model sees.
#' @param parameters Either a named list of JSON-Schema property definitions,
#'   e.g. `list(city = list(type = "string", description = "City name"))`, or a
#'   complete JSON-Schema object (a list with `type = "object"` and
#'   `properties`). A tool with no arguments may omit it.
#' @param required Character vector of required argument names. Defaults to all
#'   parameter names when `parameters` is a property list.
#' @return An object of class `llmr_tool`.
#' @examples
#' weather <- llm_tool(
#'   fn = function(city) paste0("22C and clear in ", city),
#'   name = "get_weather",
#'   description = "Current weather for a city.",
#'   parameters = list(city = list(type = "string", description = "City name"))
#' )
#' @seealso [call_llm_tools()], [tool_calls()]
#' @export
llm_tool <- function(fn, name, description, parameters = NULL, required = NULL) {
  stopifnot(is.function(fn),
            is.character(name), length(name) == 1L,
            grepl("^[A-Za-z][A-Za-z0-9_-]*$", name),
            is.character(description), length(description) == 1L)
  schema <- if (is.null(parameters)) {
    list(type = "object", properties = stats::setNames(list(), character(0)))
  } else if (!is.null(parameters$type) && !is.null(parameters$properties)) {
    parameters
  } else {
    list(
      type = "object",
      properties = parameters,
      required = as.list(required %||% names(parameters))
    )
  }
  structure(
    list(name = name, description = description, schema = schema, fn = fn),
    class = "llmr_tool"
  )
}

#' A stable signature for a tool's identity
#'
#' Returns a SHA-256 hash over a tool's `name`, `description`, and parameter
#' `schema` (its implementation function is deliberately excluded, so the
#' signature tracks the contract a model sees, not the R code). Use it to detect
#' when a tool's advertised interface has changed, for example to pin a tool
#' schema and notice a silent redefinition.
#'
#' @param tool An `llmr_tool` (from [llm_tool()]), or a list carrying `name`,
#'   `description`, and `schema`.
#' @return A 64-character lowercase hex string.
#' @seealso [llm_tool()], [llm_hash()].
#' @examples
#' tl <- llm_tool(function(x) x, "echo", "Echo a value",
#'                parameters = list(x = list(type = "string")))
#' llm_tool_signature(tl)
#' @export
llm_tool_signature <- function(tool) {
  if (!is.list(tool))
    stop("`tool` must be an `llmr_tool` or a list with name/description/schema.")
  llm_hash(list(
    name        = tool$name %||% "",
    description = tool$description %||% "",
    schema      = tool$schema %||% list()
  ))
}

#' @export
print.llmr_tool <- function(x, ...) {
  args <- names(x$schema$properties %||% list())
  cat(sprintf("<llmr_tool %s(%s)>\n  %s\n", x$name,
              paste(args, collapse = ", "), x$description))
  invisible(x)
}

# ---- provider-shape conversion ----------------------------------------------

.llmr_tools_check <- function(tools) {
  if (inherits(tools, "llmr_tool")) tools <- list(tools)
  ok <- vapply(tools, inherits, logical(1), what = "llmr_tool")
  if (!length(tools) || !all(ok)) {
    stop("`tools` must be one or more llm_tool() objects.", call. = FALSE)
  }
  nm <- vapply(tools, `[[`, "", "name")
  if (anyDuplicated(nm)) stop("Tool names must be unique.", call. = FALSE)
  tools
}

.llmr_tools_openai <- function(tools) {
  lapply(tools, function(t) {
    list(type = "function",
         "function" = list(name = t$name, description = t$description,
                           parameters = t$schema))
  })
}

.llmr_tools_anthropic <- function(tools) {
  lapply(tools, function(t) {
    list(name = t$name, description = t$description, input_schema = t$schema)
  })
}

# ---- extraction ---------------------------------------------------------------

#' Extract tool calls from a response
#'
#' When a model decides to call tools, `finish_reason(x)` is `"tool"` and this
#' helper returns what it asked for. [call_llm_tools()] uses it internally; it
#' is exported so custom loops can be built on it.
#'
#' @param x An [llmr_response] object.
#' @return A list with one element per requested call:
#'   `list(id =, name =, arguments =, arguments_raw =, arguments_ok =)`.
#'   `arguments` is a named list. `arguments_raw` is the argument text as the
#'   model sent it (`NA` when the provider delivers parsed arguments).
#'   `arguments_ok` is `FALSE` when that text is not a JSON object, for example
#'   when the reply was cut off mid-call; `arguments` is then empty and the call
#'   should not be run. `list()` when the response contains no tool calls.
#' @seealso [call_llm_tools()], [llm_tool()]
#' @export
tool_calls <- function(x) {
  if (!inherits(x, "llmr_response")) return(list())
  content <- x$raw
  out <- list()

  # OpenAI-compatible chat completions: arguments arrive as JSON text
  tcs <- tryCatch(content$choices[[1]]$message$tool_calls, error = function(e) NULL)
  if (!is.null(tcs) && length(tcs)) {
    for (tc in tcs) {
      parsed <- .llmr_parse_tool_arguments(tc$`function`$arguments)
      out[[length(out) + 1L]] <- list(
        id = tc$id %||% paste0("call_", length(out) + 1L),
        name = tc$`function`$name %||% "",
        arguments = parsed$arguments,
        arguments_raw = parsed$raw,
        arguments_ok = parsed$ok
      )
    }
    return(out)
  }

  # Anthropic tool_use blocks
  if (!is.null(content$content) && is.list(content$content)) {
    for (b in content$content) {
      if (identical(b$type, "tool_use")) {
        out[[length(out) + 1L]] <- list(
          id = b$id %||% paste0("call_", length(out) + 1L),
          name = b$name %||% "",
          arguments = b$input %||% list(),
          arguments_raw = NA_character_,
          arguments_ok = is.list(b$input %||% list())
        )
      }
    }
    if (length(out)) return(out)
  }

  # Gemini functionCall parts
  parts <- tryCatch(content$candidates[[1]]$content$parts, error = function(e) NULL)
  if (is.list(parts)) {
    for (p in parts) {
      if (!is.null(p$functionCall)) {
        out[[length(out) + 1L]] <- list(
          id = paste0("call_", length(out) + 1L),
          name = p$functionCall$name %||% "",
          arguments = p$functionCall$args %||% list(),
          arguments_raw = NA_character_,
          arguments_ok = is.list(p$functionCall$args %||% list())
        )
      }
    }
  }
  out
}

# Parse OpenAI-compatible tool arguments. Absent, empty, or `null` text means
# no arguments, which tools without parameters depend on. Text that does not
# parse to a JSON object is kept as sent and flagged, so that no caller runs the
# tool with its defaults in place of the arguments the model meant to send.
.llmr_parse_tool_arguments <- function(raw) {
  none <- structure(list(), names = character(0))
  if (is.null(raw)) return(list(arguments = none, raw = NA_character_, ok = TRUE))
  if (is.list(raw)) return(list(arguments = raw, raw = NA_character_, ok = TRUE))
  if (!is.character(raw) || length(raw) != 1L || is.na(raw)) {
    return(list(arguments = none, raw = paste(format(raw), collapse = " "), ok = FALSE))
  }
  if (!nzchar(trimws(raw)) || identical(trimws(raw), "null")) {
    return(list(arguments = none, raw = raw, ok = TRUE))
  }
  args <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                   error = function(e) NULL)
  is_object <- is.list(args) && (!length(args) || !is.null(names(args)))
  if (!is_object) return(list(arguments = none, raw = raw, ok = FALSE))
  list(arguments = args, raw = raw, ok = TRUE)
}

# The tool result sent back instead of running a call, or NULL when the call
# may run: a reply cut off by the output-token limit may hold a partial call,
# and arguments that did not parse would leave the tool to its defaults.
.llmr_tool_refusal <- function(call, truncated) {
  if (isTRUE(truncated)) {
    return(paste0("ERROR: the reply was cut off by the output-token limit while ",
                  "this tool call was being written, so the tool was not run. ",
                  "Call it again with complete arguments."))
  }
  if (!isTRUE(call$arguments_ok %||% TRUE)) {
    return(paste0("ERROR: the arguments for this tool call are not valid JSON, ",
                  "so the tool was not run. Received: ",
                  substr(call$arguments_raw %||% "", 1L, 500L)))
  }
  NULL
}

# ---- execution loop -----------------------------------------------------------

# run one tool; errors become text the model can react to (a crashed tool must
# not crash the loop)
.llmr_run_tool <- function(tool, arguments) {
  res <- tryCatch(
    do.call(tool$fn, arguments),
    error = function(e) paste0("ERROR: ", conditionMessage(e))
  )
  if (is.character(res) && length(res) == 1L) return(res)
  tryCatch(
    as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null")),
    error = function(e) paste(utils::capture.output(print(res)), collapse = "\n")
  )
}

#' Call an LLM with tools and run the tool loop
#'
#' Sends `messages` together with native tool definitions, executes every tool
#' the model calls, feeds the results back, and repeats until the model
#' answers in plain text (or `max_rounds` is reached). Supported for
#' OpenAI-compatible providers (openai, groq, together, deepseek, xai,
#' alibaba, zhipu, moonshot, xiaomi, openrouter, meta, ollama) and Anthropic.
#'
#' A tool call whose arguments are not a valid JSON object, and every tool call
#' in a reply cut off by the output-token limit, is not run: the model receives
#' an error result instead and can call the tool again. Such calls do not count
#' toward `max_tool_calls`, and `tool_history` records their argument text as
#' received.
#'
#' @param config An [llm_config] for a generative model.
#' @param messages Messages as in [call_llm()].
#' @param tools One [llm_tool()] or a list of them.
#' @param max_rounds Maximum model turns (a turn may contain several tool
#'   calls). When reached, the last response is returned as-is with a warning.
#' @param max_tool_calls Maximum tool executions across the whole loop.
#'   Exceeding it raises a condition of class `llmr_tool_limit` rather than
#'   continuing to spend; the default is unlimited. Callers enforcing spend
#'   ceilings (e.g. agent frameworks) can catch that class.
#' @param verbose Print each tool invocation as it happens.
#' @param tries,wait_seconds Retry controls passed to [call_llm_robust()].
#' @return The final [llmr_response]. Its `messages` field contains the full
#'   conversation, including tool results; `tool_history` is a tibble of
#'   executed calls with columns `round`, `name`, `arguments` (JSON), and
#'   `result`; and `tool_loop` is a list with `model_calls`, `sent`, `rec`
#'   (token totals over every internal model call, `NA` when the provider
#'   reported none), `tool_calls`, and `n_unknown_usage` (model calls whose
#'   usage was not reported, and so is missing from `sent` and `rec`). The same values remain available as
#'   attributes for backward compatibility. Note that `tokens(x)` alone covers
#'   only the final model call.
#' @examples
#' \dontrun{
#' weather <- llm_tool(
#'   function(city) paste0("22C and clear in ", city),
#'   name = "get_weather",
#'   description = "Current weather for a city.",
#'   parameters = list(city = list(type = "string"))
#' )
#' cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
#' r <- call_llm_tools(cfg, "What is the weather in Tunis?", tools = weather)
#' as.character(r)
#' r$tool_history
#' }
#' @seealso [llm_tool()], [tool_calls()], [call_llm()]
#' @export
call_llm_tools <- function(config, messages, tools,
                           max_rounds = 8L,
                           max_tool_calls = Inf,
                           verbose = FALSE,
                           tries = 3L,
                           wait_seconds = 2) {
  stopifnot(inherits(config, "llm_config"))
  tools <- .llmr_tools_check(tools)
  if (identical(config$provider, "gemini")) {
    stop("call_llm_tools() does not yet drive the Gemini tool protocol; use an ",
         "OpenAI-compatible provider or Anthropic.", call. = FALSE)
  }
  is_anthropic <- identical(config$provider, "anthropic")

  tool_index <- stats::setNames(tools, vapply(tools, `[[`, "", "name"))
  cfg <- config
  mp <- cfg$model_params %||% list()
  mp$tools <- if (is_anthropic) .llmr_tools_anthropic(tools) else .llmr_tools_openai(tools)
  cfg$model_params <- mp

  convo <- .normalize_messages(messages)
  history <- list()

  # aggregate spend across the whole loop (tokens(resp) covers only the last
  # call, which silently undercounts multi-round loops)
  n_model_calls <- 0L
  agg_sent <- 0L; agg_rec <- 0L; saw_usage <- FALSE
  n_unknown <- 0L
  executed <- 0L
  account_call <- function(resp) {
    n_model_calls <<- n_model_calls + 1L
    u <- tokens(resp)
    s <- suppressWarnings(as.integer(u$sent)); r <- suppressWarnings(as.integer(u$rec))
    if (length(s) == 1L && !is.na(s)) { agg_sent <<- agg_sent + s; saw_usage <<- TRUE }
    if (length(r) == 1L && !is.na(r)) { agg_rec  <<- agg_rec + r;  saw_usage <<- TRUE }
    if (length(s) != 1L || length(r) != 1L || is.na(s) || is.na(r)) {
      n_unknown <<- n_unknown + 1L
    }
  }
  loop_attr <- function() list(
    model_calls = n_model_calls,
    sent = if (saw_usage) agg_sent else NA_integer_,
    rec  = if (saw_usage) agg_rec  else NA_integer_,
    tool_calls = executed,
    n_unknown_usage = n_unknown
  )
  assert_tool_limit <- function() {
    if (executed + 1L > max_tool_calls) {
      stop(structure(
        class = c("llmr_tool_limit", "error", "condition"),
        list(message = sprintf(
          "call_llm_tools() would exceed max_tool_calls = %s; stopping rather than continuing to spend.",
          format(max_tool_calls)), call = NULL)))
    }
  }

  for (round in seq_len(max_rounds)) {
    resp <- call_llm_robust(cfg, convo, tries = tries,
                            wait_seconds = wait_seconds, verbose = FALSE)
    account_call(resp)
    calls <- tool_calls(resp)
    if (!length(calls)) {
      resp$messages <- convo
      resp$tool_history <- .llmr_tool_history(history)
      resp$tool_loop <- loop_attr()
      attr(resp, "messages") <- resp$messages
      attr(resp, "tool_history") <- resp$tool_history
      attr(resp, "tool_loop") <- resp$tool_loop
      return(resp)
    }

    # Every call gets a result, as both protocols require, but a call from a
    # truncated reply or with unparseable arguments is answered with an error
    # instead of being run, and does not count toward max_tool_calls.
    truncated <- identical(resp$finish_reason, "length")
    run_call <- function(cl) {
      refusal <- .llmr_tool_refusal(cl, truncated)
      if (!is.null(refusal)) {
        if (verbose) message(sprintf("[tool] %s not run: %s", cl$name, refusal))
        return(refusal)
      }
      assert_tool_limit()
      executed <<- executed + 1L
      tool <- tool_index[[cl$name]]
      if (is.null(tool)) return(paste0("ERROR: unknown tool '", cl$name, "'"))
      if (verbose) message(sprintf("[tool] %s(%s)", cl$name,
                                   jsonlite::toJSON(cl$arguments, auto_unbox = TRUE)))
      .llmr_run_tool(tool, cl$arguments)
    }
    record <- function(cl, result) {
      history[[length(history) + 1L]] <<- list(
        round = round, name = cl$name, arguments = cl$arguments,
        arguments_raw = if (isTRUE(cl$arguments_ok %||% TRUE)) NULL else cl$arguments_raw,
        result = result)
    }

    if (is_anthropic) {
      # assistant turn: the raw content blocks (text + tool_use), verbatim
      convo <- append(convo, list(list(role = "assistant",
                                       content = resp$raw$content)))
      result_blocks <- list()
      for (cl in calls) {
        result <- run_call(cl)
        record(cl, result)
        result_blocks[[length(result_blocks) + 1L]] <- list(
          type = "tool_result", tool_use_id = cl$id, content = result
        )
      }
      convo <- append(convo, list(list(role = "user", content = result_blocks)))
    } else {
      # OpenAI-compatible: assistant turn carries the tool_calls verbatim,
      # then one role="tool" message per result
      amsg <- resp$raw$choices[[1]]$message
      amsg$content <- amsg$content %||% ""
      convo <- append(convo, list(amsg))
      for (cl in calls) {
        result <- run_call(cl)
        record(cl, result)
        convo <- append(convo, list(list(role = "tool",
                                         tool_call_id = cl$id,
                                         content = result)))
      }
    }
  }

  warning("call_llm_tools() reached max_rounds = ", max_rounds,
          " with tool calls still pending; returning the last response.",
          call. = FALSE)
  resp$messages <- convo
  resp$tool_history <- .llmr_tool_history(history)
  resp$tool_loop <- loop_attr()
  attr(resp, "messages") <- resp$messages
  attr(resp, "tool_history") <- resp$tool_history
  attr(resp, "tool_loop") <- resp$tool_loop
  resp
}

.llmr_tool_history <- function(history) {
  if (!length(history)) {
    return(tibble::tibble(round = integer(0), name = character(0),
                          arguments = character(0), result = character(0)))
  }
  tibble::tibble(
    round = vapply(history, function(h) as.integer(h$round), integer(1)),
    name = vapply(history, function(h) as.character(h$name), character(1)),
    arguments = vapply(history, function(h)
      h$arguments_raw %||%
        as.character(jsonlite::toJSON(h$arguments, auto_unbox = TRUE, null = "null")),
      character(1)),
    result = vapply(history, function(h) as.character(h$result), character(1))
  )
}
