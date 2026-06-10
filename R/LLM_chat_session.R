# chat_session.R  ------------------------------------------------------------
# Stateful chat wrapper for LLMR (list-based, no closure trickery)
# ---------------------------------------------------------------------------

#' @importFrom utils head tail
#' @importFrom utils modifyList

## helper: make a well-formed message ---------------------------------------
.msg <- function(role, content) {
  # Multimodal/part-list content is stored as-is; plain text is coerced to a
  # length-1 string.
  if (is.list(content)) return(list(role = role, content = content))
  list(role = role, content = as.character(content)[1])
}

## helper: append user input, which may be a named vector with file parts ----
## Returns the number of messages appended so a failed send can roll back.
.chat_append_input <- function(e, text, role) {
  if (is.character(text) && length(text) > 1L && !is.null(names(text))) {
    msgs <- .normalize_messages(text)
    e$messages <- append(e$messages, msgs)
    return(length(msgs))
  }
  if (is.character(text) && length(text) > 1L) {
    # unnamed vector: each element its own user turn, as in call_llm()
    msgs <- lapply(text, function(t) .msg(role, t))
    e$messages <- append(e$messages, msgs)
    return(length(msgs))
  }
  e$messages <- append(e$messages, list(.msg(role, text)))
  1L
}

## helper: render message content (possibly part lists) as a display string --
.chat_content_string <- function(content) {
  if (is.character(content)) return(content[1])
  if (is.list(content)) {
    parts <- vapply(content, function(p) {
      if (identical(p$type, "file")) paste0("[file: ", p$path %||% "?", "]")
      else p$text %||% ""
    }, character(1))
    return(paste(parts[nzchar(parts)], collapse = " "))
  }
  as.character(content)[1]
}

## helper: NA-safe accumulation ----------------------------------------------
## An unknown usage value (NA) contributes nothing to a running total rather
## than poisoning it to NA.
.add0 <- function(x) { v <- suppressWarnings(as.integer(x %||% 0L)); if (length(v) != 1L || is.na(v)) 0L else v }

## provider-agnostic token counter ------------------------------------------
.token_counts <- function(j) {
  if (!is.null(j$usage)) {
    u <- j$usage
    if (!is.null(u$prompt_tokens)  && !is.null(u$completion_tokens))
      return(list(sent = u$prompt_tokens, rec = u$completion_tokens))
    if (!is.null(u$input_tokens)   && !is.null(u$output_tokens))
      return(list(sent = u$input_tokens,  rec = u$output_tokens))
  }
  if (!is.null(j$usageMetadata)) {
    m <- j$usageMetadata
    # Gemini omits zero-valued integer fields, so candidatesTokenCount can be
    # absent (e.g. thinking models that produce no visible output). Keep the
    # prompt count rather than discarding the whole usage record.
    if (!is.null(m$promptTokenCount) || !is.null(m$candidatesTokenCount)) {
      sent <- m$promptTokenCount %||% 0L
      rec  <- m$candidatesTokenCount %||%
        (if (!is.null(m$totalTokenCount)) max(0L, m$totalTokenCount - sent) else 0L)
      return(list(sent = sent, rec = rec))
    }
  }
  # No usage metadata at all: unknown, not zero. Report NA so callers do not
  # mistake "the provider told us nothing" for "this response was free". The
  # running totals in chat_session() add NA as 0 (see $send()) so one unknown
  # response cannot poison the cumulative count.
  list(sent = NA_integer_, rec = NA_integer_)
}

# ---------------------------------------------------------------------------#
# MASTER DOCUMENTATION BLOCK                                                 #
# ---------------------------------------------------------------------------#
#' Chat Session Object and Methods
#'
#' Create and interact with a stateful chat session object that retains
#' message history. This documentation page covers the constructor function
#' `chat_session()` as well as all S3 methods for the `llm_chat_session` class.
#'
#' @md
#'
#' @details
#' The `chat_session` object provides a simple way to hold a conversation with
#' a generative model. It wraps [call_llm_robust()] to benefit from retry logic,
#' caching, and error logging.
#'
#' @section How it works:
#'   1.  A private environment stores the running list of
#'       `list(role, content)` messages.
#'   2.  At each `$send()` the history is sent *in full* to the model.
#'   3.  Provider-agnostic token counts are extracted from the JSON response.
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{$send(text, ..., role = "user")}}{
#'     Append a message (default role `"user"`), query the model,
#'     print the assistant's reply (unless `quiet = TRUE`), and invisibly
#'     return it. `text` may also be a named character vector using the same
#'     multimodal shortcut as [call_llm()], e.g.
#'     `chat$send(c(user = "Describe this image.", file = "plot.png"))`.}
#'   \item{\code{$send_structured(text, schema, ..., role = "user", .fields = NULL, .validate_local = TRUE)}}{
#'     Send a message with structured-output enabled using `schema`, append the assistant's reply,
#'     parse JSON (and optionally validate locally when `.validate_local = TRUE`),
#'     returning the parsed result invisibly.}
#'   \item{\code{$send_tags(text, .tags, ..., role = "user", .fields = NULL)}}{
#'     Send a message with XML-like tag instructions injected, append the
#'     assistant's reply, parse the requested tags, and invisibly return
#'     the parsed list.}
#'   \item{\code{$history()}}{Raw list of messages.}
#'   \item{\code{$history_df()}}{Two-column data frame (`role`, `content`).}
#'   \item{\code{$tokens_sent()}/\code{$tokens_received()}}{Running token totals.}
#'   \item{\code{$reset()}}{Clear history (retains the optional system message).}
#' }
#'
#' @param config  An [llm_config] **for a generative model** (`embedding = FALSE`).
#' @param system  Optional system prompt inserted once at the beginning.
#' @param quiet   Logical. If `TRUE`, `$send()` and friends do not print the
#'   reply; they still return it invisibly. Useful inside scripts and loops.
#' @param ...     Default arguments forwarded to every [call_llm_robust()] call (e.g. `verbose = TRUE`).
#' @param x,object An `llm_chat_session` object.
#' @param n Number of turns to display.
#' @param width Character width for truncating long messages.
#'
#' @return For `chat_session()`, an object of class **`llm_chat_session`**.
#'         Other methods return what their titles state.
#'
#' @seealso
#' [llm_config()], [call_llm()], [call_llm_robust()], [llm_fn()], [llm_mutate()]
#'
#' @name llm_chat_session
#'
#' @examples
#' if (interactive()) {
#'   cfg  <- llm_config("openai", "gpt-5-nano")
#'   chat <- chat_session(cfg, system = "Be concise.")
#'   chat$send("Who invented the moon?")
#'   chat$send("Explain why in one short sentence.")
#'   chat           # print() shows a summary and first 10 turns
#'   summary(chat)  # stats
#'   tail(chat, 2)
#'   as.data.frame(chat)
#' }
NULL
#' @title Stateful chat session constructor
#' @rdname llm_chat_session
#' @export
chat_session <- function(config, system = NULL, quiet = FALSE, ...) {

  stopifnot(inherits(config, "llm_config"))
  if (isTRUE(config$embedding))
    stop("chat_session requires a generative model (embedding = FALSE).")
  quiet <- isTRUE(quiet)

  ## private state ----------------------------------------------------------
  e <- new.env(parent = emptyenv())
  e$messages <- if (is.null(system)) list() else list(.msg("system", system))
  e$raw      <- list()
  e$sent     <- 0
  e$received <- 0
  defaults   <- list(...)

  call_robust <- function(extra = list()) {
    # Keep only role/content; part lists (multimodal) pass through unchanged.
    clean <- unname(lapply(e$messages, function(m) list(role = m$role, content = m$content)))
    # Allow a per-call config override but avoid duplicate formal args
    cfg2 <- extra$config %||% config
    extra$config <- NULL
    # Guard against accidental 'messages' in extra
    extra$messages <- NULL

    do.call(
      call_llm_robust,
      c(
        list(config = cfg2, messages = clean),
        modifyList(defaults, extra)  # tries, wait_seconds, etc. still work
      )
    )
  }

  ## exposed methods --------------------------------------------------------
  send <- function(text, ..., role = "user") {
    n_appended <- .chat_append_input(e, text, role)

    resp <- tryCatch(
      call_robust(list(...)),
      error = function(err) {
        e$messages <- e$messages[seq_len(length(e$messages) - n_appended)]
        stop(err)
      }
    )

    if (inherits(resp, "llmr_response")) {
      txt  <- as.character(resp)
      raw  <- attr(resp, "full_response") %||% resp$raw %||% NULL
      fr   <- finish_reason(resp)
      u    <- tokens(resp)
      tc   <- list(sent = u$sent %||% 0L, rec = u$rec %||% 0L)
    } else {
      raw  <- attr(resp, "full_response")
      txt  <- resp
      fr   <- NULL
      tc   <- .token_counts(raw)
    }

    if (is.null(txt)) txt <- "Error: Failed to get a response."

    e$sent     <- e$sent     + .add0(tc$sent)
    e$received <- e$received + .add0(tc$rec)
    e$raw      <- append(e$raw, list(if (inherits(resp, "llmr_response")) resp else raw))

    e$messages <- append(e$messages, list(.msg("assistant", txt)))

    if (!quiet) {
      if (inherits(resp, "llmr_response")) {
        print(resp)  # text + status line [model | finish | tokens | t]
      } else {
        cat(txt, "\n")
      }
      if (!is.null(fr) && !identical(fr, "stop")) {
        msg <- switch(fr,
                      "length" = "finish_reason=length; increase max_tokens or shorten context.",
                      "filter" = "finish_reason=filter; adjust prompt/safety settings.",
                      "tool"   = "finish_reason=tool; provide tool handler or disable tools.",
                      sprintf("finish_reason=%s.", fr)
        )
        message(msg)
      }
    }
    invisible(txt)
  }

  send_structured <- function(text, schema, ..., role = "user",
                              .fields = NULL, .validate_local = TRUE) {
    stopifnot(is.list(schema))
    e$messages <- append(e$messages, list(.msg(role, text)))

    cfgs <- enable_structured_output(config, schema = schema)
    resp <- tryCatch(
      call_robust(list(config = cfgs, ...)),
      error = function(err) {
        e$messages <- e$messages[-length(e$messages)]
        stop(err)
      }
    )

    txt <- as.character(resp)
    parsed <- llm_parse_structured(txt)
    if (!is.null(schema) && isTRUE(.validate_local)) {
      # local single-row validation
      df <- data.frame(response_text = txt, stringsAsFactors = FALSE)
      df$structured_data <- list(parsed)
      df2 <- llm_validate_structured_col(df, schema, structured_list_col = "structured_data")
      if (isFALSE(df2$structured_valid[[1]])) {
        message("Structured validation error: ", df2$structured_error[[1]])
      }
    }

    e$sent     <- e$sent     + .add0(tokens(resp)$sent)
    e$received <- e$received + .add0(tokens(resp)$rec)
    e$raw      <- append(e$raw, list(resp))
    e$messages <- append(e$messages, list(.msg("assistant", txt)))
    if (!quiet) print(resp)
    invisible(parsed)
  }

  send_tags <- function(text, .tags, ..., role = "user", .fields = NULL) {
    tags <- .validate_tags(.tags)
    instruction <- .tag_prompt(tags)
    augmented <- paste(text, instruction, sep = "\n\n")
    e$messages <- append(e$messages, list(.msg(role, augmented)))

    resp <- tryCatch(
      call_robust(list(...)),
      error = function(err) {
        e$messages <- e$messages[-length(e$messages)]
        stop(err)
      }
    )
    txt <- as.character(resp)
    parsed <- llm_parse_tags(txt, tags = tags)

    e$sent     <- e$sent     + .add0(tokens(resp)$sent)
    e$received <- e$received + .add0(tokens(resp)$rec)
    e$raw      <- append(e$raw, list(resp))
    e$messages <- append(e$messages, list(.msg("assistant", txt)))
    if (!quiet) print(resp)
    invisible(parsed)
  }

  history    <- function()  e$messages
  history_df <- function()  data.frame(
    role    = vapply(e$messages, `[[`, "", "role"),
    content = vapply(e$messages, function(m) .chat_content_string(m$content), character(1)),
    stringsAsFactors = FALSE
  )
  tokens_sent      <- function() e$sent
  tokens_received  <- function() e$received
  reset <- function() {
    e$messages <- if (is.null(system)) list() else list(.msg("system", system))
    e$raw <- list(); e$sent <- 0; e$received <- 0
    invisible(NULL)
  }

  structure(
    list(
      send            = send,
      send_structured = send_structured,
      send_tags       = send_tags,
      history         = history,
      history_df      = history_df,
      tokens_sent     = tokens_sent,
      tokens_received = tokens_received,
      reset           = reset
    ),
    class = "llm_chat_session"
  )
}

# ---------------------------------------------------------------------------#
# S3 helpers so base verbs behave naturally                                  #
# ---------------------------------------------------------------------------#

#' @title Coerce a chat session to a data frame
#' @rdname llm_chat_session
#' @export
as.data.frame.llm_chat_session <- function(x, ...) {
  x$history_df()
}

#' @title Summary statistics for a chat session
#' @rdname llm_chat_session
#' @export
summary.llm_chat_session <- function(object, ...) {
  hist <- object$history_df()
  out  <- list(
    messages         = nrow(hist),
    exchanges        = sum(hist$role == "assistant"),
    tokens_sent      = object$tokens_sent(),
    tokens_received  = object$tokens_received(),
    last_assistant   = tail(hist$content[hist$role == "assistant"], 1)
  )
  class(out) <- "summary.llm_chat_session"
  out
}

#' @export
print.summary.llm_chat_session <- function(x, ...) {
  cat("llm_chat_session summary\n",
      "-----------------------\n",
      "Messages:         ", x$messages,        "\n",
      "Exchanges:        ", x$exchanges,       "\n",
      "Tokens sent:      ", x$tokens_sent,     "\n",
      "Tokens received:  ", x$tokens_received, "\n",
      "Last assistant:   ", x$last_assistant,  "\n", sep = "")
  invisible(x)
}

# ---------------------------------------------------------------------------#
# Custom print: row-by-row display with truncation                           #
# ---------------------------------------------------------------------------#

## ------------------------------------------------------------------ ##
##  helper: row-by-row pretty printer with truncation                 ##
## ------------------------------------------------------------------ ##
.format_rows <- function(df, width = getOption("width") - 15) {
  for (i in seq_len(nrow(df))) {
    txt <- df$content[i]
    if (nchar(txt) > width)
      txt <- paste0(substr(txt, 1, width - 3), "...")
    cat(sprintf("[%s] %s\n", df$role[i], txt))
  }
}

#' @title Display the first part of a chat session
#' @rdname llm_chat_session
#' @export
head.llm_chat_session <- function(x, n = 6L, width = getOption("width") - 15, ...) {
  slice <- utils::head(x$history_df(), n, ...)
  .format_rows(slice, width)
  invisible(slice)
}

#' @title Display the last part of a chat session
#' @rdname llm_chat_session
#' @export
tail.llm_chat_session <- function(x, n = 6L, width = getOption("width") - 15, ...) {
  slice <- utils::tail(x$history_df(), n, ...)
  .format_rows(slice, width)
  invisible(slice)
}

#' @title Print a chat session object
#' @rdname llm_chat_session
#' @export
print.llm_chat_session <- function(x, width = getOption("width") - 15, ...) {
  hist <- x$history_df()
  cat("llm_chat_session (turns:", nrow(hist),
      "| sent:", x$tokens_sent(),
      "| rec:",  x$tokens_received(), ")\n\n")

  .format_rows(utils::head(hist, 10), width)
  if (nrow(hist) > 10) cat("...\n")
  invisible(x)
}
