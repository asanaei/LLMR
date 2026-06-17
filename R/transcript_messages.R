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
#' satisfies both constraints: it merges all leading `system` messages into one
#' system block, merges any run of adjacent same-role messages into one (joining
#' their content), and guarantees the first non-system message has role
#' `"user"`.
#'
#' This is the normalizer that makes hand-built multi-turn arrays (for example
#' from [transcript_as_messages()]) safe to send to any supported provider. It
#' is a pure transformation: no network or file I/O.
#'
#' Merging assumes scalar character content: if two adjacent same-role messages
#' carry non-scalar content (for example multimodal/list content), they cannot
#' be concatenated and the function errors rather than corrupt them. Supply an
#' array that is already alternating in that case.
#'
#' @param messages A list of message objects, each `list(role=, content=)`. Any
#'   number of leading `system` messages is allowed; they are merged into one
#'   system block (most providers take system as a single top-level field).
#' @return A list of message objects that strictly alternates roles after the
#'   merged leading `system` block, beginning with `user`.
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

  # Content-merge helper: only scalar character content can be merged. Adjacent
  # same-role messages with list/multimodal content cannot be concatenated with
  # paste() without corruption, so error clearly rather than silently mangle.
  merge_two <- function(a, b) {
    if (!is.character(a) || length(a) != 1L || !is.character(b) || length(b) != 1L) {
      stop("ensure_alternating_messages(): cannot merge adjacent same-role ",
           "messages with non-scalar (e.g. multimodal/list) content. Supply a ",
           "message array that is already alternating, or use scalar text content.",
           call. = FALSE)
    }
    paste(a, b, sep = "\n")
  }

  # Peel ALL leading system messages and merge them into one system block (the
  # roxygen promise). Multiple leading systems otherwise survive into the body
  # and break Anthropic/Gemini, which take system as a single top-level field.
  sys <- NULL
  i <- 1L
  while (i <= length(messages) && identical(messages[[i]]$role, "system")) i <- i + 1L
  if (i > 1L) {
    sys_content <- messages[[1L]]$content
    if (i > 2L) for (j in 2L:(i - 1L)) sys_content <- merge_two(sys_content, messages[[j]]$content)
    sys <- list(role = "system", content = sys_content)
  }
  body <- if (i > length(messages)) list() else messages[i:length(messages)]
  if (length(body) == 0L) return(if (!is.null(sys)) list(sys) else messages)

  merge_runs <- function(msgs) {
    out <- list()
    for (m in msgs) {
      if (length(out) > 0L && identical(out[[length(out)]]$role, m$role)) {
        out[[length(out)]]$content <- merge_two(out[[length(out)]]$content, m$content)
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
#' @param ensure_final_user If `TRUE` (default) and no `instruction` is supplied
#'   and the last turn is the speaker's own (an `assistant` turn), append a
#'   minimal `user` cue so the array ends on a user turn (a sound next-turn
#'   generation boundary). Set `FALSE` for verbatim archival conversion. Missing
#'   `text` is coerced to `""`; a missing `speaker` is an error.
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
                                   sanitize = TRUE,
                                   ensure_final_user = TRUE) {
  if (!is.data.frame(transcript))
    stop("`transcript` must be a data.frame/tibble with `speaker` and `text` columns.",
         call. = FALSE)
  if (!all(c("speaker", "text") %in% names(transcript)))
    stop("`transcript` must have columns `speaker` and `text`.", call. = FALSE)
  if (length(speaker) != 1L || is.na(speaker))
    stop("`speaker` must be a single non-NA value.", call. = FALSE)

  spk <- as.character(transcript$speaker)
  txt <- as.character(transcript$text)
  # A missing speaker cannot be labeled or matched against `speaker`; reject it.
  # Missing text is coerced to "" so it does not become the literal string "NA".
  if (any(is.na(spk)))
    stop("`transcript$speaker` has missing values; every turn needs a speaker.",
         call. = FALSE)
  txt[is.na(txt)] <- ""

  msgs <- list()
  if (!is.null(system) && nzchar(system))
    msgs[[length(msgs) + 1L]] <- list(role = "system", content = as.character(system))

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

  # For next-turn generation the array should end on a user turn. If no
  # instruction was supplied and the last turn is the speaker's own (assistant),
  # that is a poor generation boundary (the model is asked to continue after its
  # own answer, not after a cue). Append a minimal user cue unless the caller
  # opts out (e.g. archival conversion that wants the transcript verbatim).
  if (isTRUE(ensure_final_user) && length(msgs) &&
      identical(msgs[[length(msgs)]]$role, "assistant")) {
    msgs[[length(msgs) + 1L]] <- list(role = "user", content = "(continue)")
  }

  ensure_alternating_messages(msgs)
}
