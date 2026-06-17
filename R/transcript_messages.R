# transcript_messages.R ------------------------------------------------------
# Generic, provider-safe helpers for turning a multi-speaker transcript into a
# message array from one speaker's perspective ("role flip"), plus the
# alternation normalizer they depend on. These are pure functions over the
# message shape `call_llm()` already accepts (a list of
# `list(role=, content=)`); they know nothing about agents, moderators, or any
# package above LLMR.

#' Sanitize transcript text so it cannot forge a role header
#'
#' Strips chat-template control tokens and leading role labels from a piece of
#' another speaker's text before it is embedded in a `user` turn, so a
#' participant cannot inject a fake `System:`/`Assistant:` instruction or a
#' `<|im_start|>` boundary.
#' @keywords internal
#' @noRd
.llm_sanitize_turn <- function(text) {
  x <- as.character(text)
  x <- gsub("(?i)<\\|?im_(start|end|sep)\\|?>", " ", x, perl = TRUE)
  x <- gsub("(?im)^\\s*(system|assistant|user|developer|tool)\\s*:", "\\1 -",
            x, perl = TRUE)
  x
}

#' Make a message array provider-safe (alternating, user-leading)
#'
#' Some providers (Anthropic, Gemini) reject a message array whose first
#' non-system turn is not `user`, or that contains two consecutive turns of the
#' same role. `ensure_alternating_messages()` repairs an assembled array so it
#' satisfies both constraints: it keeps a single leading `system` message
#' untouched, merges any run of adjacent same-role messages into one (joining
#' their content), and guarantees the first non-system message has role
#' `"user"`.
#'
#' This is the normalizer that makes hand-built multi-turn arrays (for example
#' from [transcript_as_messages()]) safe to send to any supported provider. It
#' is a pure transformation: no network or file I/O.
#'
#' @param messages A list of message objects, each `list(role=, content=)`.
#'   A single leading `system` message is allowed and preserved. (Multiple
#'   leading system messages are merged like any other same-role run.)
#' @return A list of message objects that strictly alternates roles after any
#'   leading `system`, beginning with `user`.
#' @seealso [transcript_as_messages()], [call_llm()]
#' @examples
#' msgs <- list(
#'   list(role = "system", content = "be terse"),
#'   list(role = "user",   content = "Ana: hello"),
#'   list(role = "user",   content = "Ben: hi"),     # adjacent user -> merged
#'   list(role = "assistant", content = "my prior reply"),
#'   list(role = "user",   content = "your turn")
#' )
#' ensure_alternating_messages(msgs)
#' @export
ensure_alternating_messages <- function(messages) {
  if (!is.list(messages) || length(messages) == 0L) return(messages)

  # Peel a single leading system message; the rest is the alternating body.
  sys <- NULL
  body <- messages
  if (identical(messages[[1]]$role, "system")) {
    sys  <- messages[[1]]
    body <- messages[-1]
  }
  if (length(body) == 0L) return(messages)

  merge_runs <- function(msgs) {
    out <- list()
    for (m in msgs) {
      if (length(out) > 0L && identical(out[[length(out)]]$role, m$role)) {
        out[[length(out)]]$content <-
          paste(out[[length(out)]]$content, m$content, sep = "\n")
      } else {
        out[[length(out) + 1L]] <- m
      }
    }
    out
  }

  out <- merge_runs(body)

  # Guarantee a user-leading body. A leading assistant turn (the speaker has
  # only its own prior turns and nothing from others yet) is not provider-safe;
  # prepend a minimal user turn and re-merge.
  if (!identical(out[[1]]$role, "user")) {
    out <- merge_runs(c(list(list(role = "user", content = "(continue)")), out))
  }

  if (!is.null(sys)) c(list(sys), out) else out
}

#' Build a role-flipped message array from a multi-speaker transcript
#'
#' Renders a shared, speaker-attributed transcript into a message array from
#' one speaker's point of view, the way a stateful chat is post-trained to read
#' it: the current speaker's own past turns become `assistant` messages (their
#' own voice), and every other speaker's turns become `user` messages labeled
#' with the speaker's name. An optional `system` block (persona, instructions)
#' leads the array, and an optional `instruction` (the current question or "your
#' turn" cue) closes it as a trailing `user` message. The result is passed
#' through [ensure_alternating_messages()] so it is provider-safe.
#'
#' Putting the speaker's own turns in the `assistant` role gives the model a
#' structural handle on "what I already said", which reduces self-repetition in
#' multi-agent conversations relative to flattening the whole transcript into one
#' `user` block. Other speakers' text stays in `user` turns (never `assistant`),
#' which preserves the trust boundary and is sanitized against forged role
#' headers.
#'
#' @param transcript A data.frame/tibble with character columns `speaker` and
#'   `text`, in chronological order.
#' @param speaker The current speaker's id (matched against `transcript$speaker`).
#'   Rows with this speaker become `assistant` turns; all others become labeled
#'   `user` turns.
#' @param system Optional system text (persona, standing instructions) placed as
#'   the leading `system` message. `NULL` to omit.
#' @param instruction Optional trailing instruction (current question, "your
#'   turn" cue) placed as the final `user` message. `NULL` to omit.
#' @param label A function `(speaker_id) -> prefix` producing the in-content
#'   label for other speakers' turns; the default renders `"<id>: "`. The
#'   speaker's own turns are never labeled.
#' @param sanitize If `TRUE` (default), strip forged role headers and chat
#'   control tokens from other speakers' text before embedding.
#' @return A list of message objects (`list(role=, content=)`) suitable for
#'   [call_llm()], guaranteed alternating and user-leading after any system.
#' @seealso [ensure_alternating_messages()], [call_llm()]
#' @examples
#' tx <- data.frame(
#'   speaker = c("Ana", "Ben", "Cy"),
#'   text    = c("I think we ban it.", "Too costly for renters.", "Phase it in."),
#'   stringsAsFactors = FALSE
#' )
#' # From Ben's perspective: his line is assistant, Ana's and Cy's are user.
#' transcript_as_messages(tx, speaker = "Ben",
#'                        system = "You are Ben, a renter.",
#'                        instruction = "Your turn, Ben.")
#' @export
transcript_as_messages <- function(transcript, speaker,
                                   system = NULL, instruction = NULL,
                                   label = function(id) paste0(id, ": "),
                                   sanitize = TRUE) {
  if (!is.data.frame(transcript))
    stop("`transcript` must be a data.frame/tibble with `speaker` and `text` columns.",
         call. = FALSE)
  if (!all(c("speaker", "text") %in% names(transcript)))
    stop("`transcript` must have columns `speaker` and `text`.", call. = FALSE)
  if (length(speaker) != 1L || is.na(speaker))
    stop("`speaker` must be a single non-NA value.", call. = FALSE)

  msgs <- list()
  if (!is.null(system) && nzchar(system))
    msgs[[length(msgs) + 1L]] <- list(role = "system", content = as.character(system))

  spk <- as.character(transcript$speaker)
  txt <- as.character(transcript$text)
  clean <- if (sanitize) .llm_sanitize_turn else identity
  for (i in seq_along(spk)) {
    if (identical(spk[i], as.character(speaker))) {
      # own turn -> assistant, verbatim, no label
      msgs[[length(msgs) + 1L]] <- list(role = "assistant", content = txt[i])
    } else {
      msgs[[length(msgs) + 1L]] <- list(role = "user",
        content = paste0(label(spk[i]), clean(txt[i])))
    }
  }

  if (!is.null(instruction) && nzchar(instruction))
    msgs[[length(msgs) + 1L]] <- list(role = "user", content = as.character(instruction))

  ensure_alternating_messages(msgs)
}
