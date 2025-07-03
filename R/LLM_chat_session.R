# chat_session.R  ------------------------------------------------------------
# Stateful chat wrapper for LLMR (list-based, no closure trickery)
# ---------------------------------------------------------------------------

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

#' Stateful chat session
#'
#' Create a lightweight, in-memory conversation object that retains message
#' history between calls to the LLM.  Internally it wraps
#' \code{call_llm_robust()} so you still benefit from retry logic,
#' caching, and error logging.
#'
#' @section How it works:
#'   1.  A private environment stores the running list of
#'       \code{list(role, content)} messages.
#'   2.  At each \code{$send()} the history is sent *in full* to the model.
#'   3.  Provider-agnostic token counts are extracted from the JSON response
#'       (fields are detected by name, so new providers continue to work).
#'
#' @param config  An [\code{llm_config}] **for a generative model**
#'                (i.e. \code{embedding = FALSE}).
#' @param system  Optional system prompt inserted once at the beginning.
#' @param ...     Default arguments forwarded to every
#'                [\code{call_llm_robust()}] call (e.g.
#'                \code{verbose = TRUE}, \code{json = TRUE}).
#'
#' @return An object of class **\code{llm_chat_session}** with the methods
#'   listed below.
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
#' @examples
#' \dontrun{
#' cfg  <- llm_config("openai", "gpt-4o-mini", Sys.getenv("OPENAI_API_KEY"))
#' chat <- chat_session(cfg, system = "Be concise.")
#' chat$send("Who invented the moon?")
#' chat$send("Explain why in one short sentence.")
#' chat           # snapshot (first 10 turns)
#' tail(chat, 2)  # last 2 turns
#' }
#' @export
#'
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
    txt  <- extract_text(raw)

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


#' @describeIn chat_session Coerce a session to a two-column data frame.
#' @export
as.data.frame.llm_chat_session <- function(x, ...) {
  x$history_df()
}

#' @describeIn chat_session Summary statistics for a chat session.
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

#' @describeIn chat_session First *n* rows of the conversation.
#' @export
head.llm_chat_session <- function(x, n = 6L, width = getOption("width") - 15, ...) {
  slice <- utils::head(x$history_df(), n, ...)
  .format_rows(slice, width)
  invisible(slice)
}

#' @describeIn chat_session Last *n* rows of the conversation.
#' @export
tail.llm_chat_session <- function(x, n = 6L, width = getOption("width") - 15, ...) {
  slice <- utils::tail(x$history_df(), n, ...)
  .format_rows(slice, width)
  invisible(slice)
}

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


