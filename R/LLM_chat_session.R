# chat_session.R  ------------------------------------------------------------
# Stateful chat wrapper for LLMR (list-based, no closure trickery)
# ---------------------------------------------------------------------------

#' @importFrom utils head tail
#' @importFrom utils modifyList

## helper: make a well-formed message ---------------------------------------
.msg <- function(role, content) list(role = role, content = as.character(content)[1])

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
    if (!is.null(m$promptTokenCount) && !is.null(m$candidatesTokenCount))
      return(list(sent = m$promptTokenCount, rec = m$candidatesTokenCount))
  }
  list(sent = 0, rec = 0)
}

#' @md
# ---------------------------------------------------------------------------#
# MASTER DOCUMENTATION BLOCK                                                 #
# ---------------------------------------------------------------------------#
#' Chat Session Object and Methods
#'
#' Create and interact with a stateful chat session object that retains
#' message history. This documentation page covers the constructor function
#' `chat_session()` as well as all S3 methods for the `llm_chat_session` class.
#'
#' @details
#' The `chat_session` object provides a simple way to hold a conversation with
#' a generative model. It wraps \code{\link{call_llm_robust}} to benefit from
#' retry logic, caching, and error logging.
#'
#' @section How it works:
#'   1.  A private environment stores the running list of
#'       \code{list(role, content)} messages.
#'   2.  At each \code{$send()} the history is sent *in full* to the model.
#'   3.  Provider-agnostic token counts are extracted from the JSON response
#'       (fields are detected by name, so new providers continue to work).
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{$send(text, ..., role = "user")}}{
#'     Append a message (default role \code{"user"}), query the model,
#'     print the assistant’s reply, and invisibly return it.}
#'   \item{\code{$history()}}{Raw list of messages.}
#'   \item{\code{$history_df()}}{Two-column data frame (\code{role},
#'     \code{content}).}
#'   \item{\code{$tokens_sent()}/\code{$tokens_received()}}{Running token
#'     totals.}
#'   \item{\code{$reset()}}{Clear history (retains the optional system
#'     message).}
#' }
#'
#' @param config  An \code{\link{llm_config}} **for a generative model**
#'                (i.e. \code{embedding = FALSE}).
#' @param system  Optional system prompt inserted once at the beginning.
#' @param ...     Arguments passed to other methods. For `chat_session`, these
#'                are default arguments forwarded to every
#'                \code{\link{call_llm_robust}} call (e.g.
#'                \code{verbose = TRUE}, \code{json = TRUE}).
#' @param x,object An `llm_chat_session` object.
#' @param n Number of turns to display.
#' @param width Character width for truncating long messages.
#'
#' @return For `chat_session()`, an object of class **`llm_chat_session`**.
#'   For other methods, the return value is described by their respective titles.
#' @seealso
#' \code{\link{llm_config}} to create the configuration object.
#' \code{\link{call_llm_robust}} for single, stateless API calls.
#' \code{\link{llm_fn}} for applying a prompt to many items in a vector or data frame.
#'
#' @name llm_chat_session
#'
#' @examples
#' \dontrun{
#' cfg  <- llm_config("openai", "gpt-4o-mini", Sys.getenv("OPENAI_API_KEY"))
#' chat <- chat_session(cfg, system = "Be concise.")
#' chat$send("Who invented the moon?")
#' chat$send("Explain why in one short sentence.")
#'
#' # Using S3 methods
#' chat           # print() shows a summary and first 10 turns
#' summary(chat)  # Get session statistics
#' tail(chat, 2)  # See the last 2 turns of the conversation
#' df <- as.data.frame(chat) # Convert the full history to a data frame
#' }
NULL

#' @title Stateful chat session constructor
#' @rdname llm_chat_session
#' @export
chat_session <- function(config, system = NULL, ...) {

  stopifnot(inherits(config, "llm_config"))
  if (isTRUE(config$embedding))
    stop("chat_session requires a generative model (embedding = FALSE).")

  ## private state ----------------------------------------------------------
  e <- new.env(parent = emptyenv())
  e$messages <- if (is.null(system)) list() else list(.msg("system", system))
  e$raw      <- list()
  e$sent     <- 0
  e$received <- 0
  defaults   <- list(...)

  call_robust <- function(extra = list()) {
    clean <- unname(lapply(e$messages, function(m) .msg(m$role, m$content)))
    do.call(
      call_llm_robust,
      c(list(config   = config,
             messages = clean,
             json     = TRUE),
        modifyList(defaults, extra))
    )
  }

  ## exposed methods --------------------------------------------------------
  send <- function(text, ..., role = "user") {
    e$messages <- append(e$messages, list(.msg(role, text)))

    resp <- call_robust(list(...))
    raw  <- attr(resp, "full_response")
    txt  <- resp #extract_text(raw)

    if (is.null(txt)) {
      txt <- "Error: Failed to get a response."
    }

    tc <- .token_counts(raw)
    e$sent     <- e$sent     + tc$sent
    e$received <- e$received + tc$rec
    e$raw      <- append(e$raw, list(raw))

    e$messages <- append(e$messages, list(.msg("assistant", txt)))

    cat(txt, "\n")
    invisible(txt)
  }

  history    <- function()  e$messages
  history_df <- function()  data.frame(
    role    = vapply(e$messages, `[[`, "", "role"),
    content = vapply(e$messages, `[[`, "", "content"),
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
    turns            = nrow(hist),
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
      "Turns:            ", x$turns,           "\n",
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
