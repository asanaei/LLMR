# log_messages.R ----------------------------------------------------------------
# Canonicalization of an LLM call's messages and generation parameters, so that
# the same call hashes identically whether it is described by a config plus
# messages (the calling side) or by a logged provider-specific request body (the
# audit-log side). These are the shared internals behind llm_request_hash() and
# llm_log_read(); the record/dedup/replay layers downstream rely on the two sides
# agreeing.

# Map EITHER a logged provider-specific request body OR a messages argument
# (character scalar, named character vector, or list of role/content turns) to
# one canonical list of (role, content) turns. Provider body shapes covered:
# Gemini (systemInstruction/contents/parts), OpenAI/Anthropic (system/messages).
#' @keywords internal
#' @noRd
.llmr_turns <- function(provider = NULL, request = NULL,
                        config = NULL, messages = NULL) {
  if (!is.null(request)) {
    turns <- list()
    if (!is.null(request$systemInstruction) || !is.null(request$contents)) {
      sys <- .llmr_text(request$systemInstruction)
      if (nzchar(sys)) turns[[length(turns) + 1L]] <- list(role = "system", content = sys)
      for (msg in request$contents %||% list()) {
        turns[[length(turns) + 1L]] <- list(
          role = .llmr_role(msg$role %||% "user"),
          content = .llmr_text(msg$parts %||% msg$content %||% msg$text))
      }
      return(turns)
    }
    sys <- .llmr_text(request$system)
    if (nzchar(sys)) turns[[length(turns) + 1L]] <- list(role = "system", content = sys)
    for (msg in request$messages %||% list()) {
      turns[[length(turns) + 1L]] <- list(
        role = .llmr_role(msg$role %||% "user"),
        content = .llmr_text(msg$content %||% msg$parts %||% msg$text))
    }
    return(turns)
  }

  if (is.null(messages)) return(list())
  if (is.character(messages)) {
    roles <- names(messages)
    if (is.null(roles)) roles <- rep("user", length(messages))
    return(lapply(seq_along(messages), function(i)
      list(role = .llmr_role(roles[i]), content = as.character(messages[[i]]))))
  }
  if (is.list(messages)) {
    if (.llmr_is_turn(messages)) messages <- list(messages)
    roles <- names(messages)
    if (is.null(roles)) roles <- rep("user", length(messages))
    return(lapply(seq_along(messages), function(i) {
      msg <- messages[[i]]
      if (.llmr_is_turn(msg)) {
        list(role = .llmr_role(msg$role %||% roles[i]),
             content = .llmr_text(msg$content))
      } else {
        list(role = .llmr_role(roles[i]), content = .llmr_text(msg))
      }
    }))
  }
  rlang::abort("`messages` must be a character vector or a list of role/content pairs.")
}

#' @keywords internal
#' @noRd
.llmr_is_turn <- function(x) {
  is.list(x) && !is.null(names(x)) && all(c("role", "content") %in% names(x))
}

#' @keywords internal
#' @noRd
.llmr_role <- function(role) {
  role <- as.character(role %||% "user")[1]
  if (is.na(role) || !nzchar(role)) role <- "user"
  role <- tolower(role)
  if (role %in% c("assistant", "model")) return("assistant")
  if (role %in% c("system", "developer")) return("system")
  # Preserve tool/function roles distinctly: a tool result is a different turn
  # from a user turn, so a tool-loop continuation must not collapse to "user".
  if (role %in% c("tool", "function")) return("tool")
  "user"
}

#' @keywords internal
#' @noRd
.llmr_text <- function(x) {
  parts <- .llmr_text_vec(x)
  paste(parts[nzchar(parts)], collapse = "\n")
}

#' @keywords internal
#' @noRd
.llmr_text_vec <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.character(x)) return(as.character(x))
  if (!is.list(x)) return(character(0))
  nm <- names(x)
  if (!is.null(nm) && "text" %in% nm) {
    typ <- tolower(as.character(x$type %||% "text")[1])
    if (!is.na(typ) && !identical(typ, "text")) return(character(0))
    return(.llmr_text_vec(x$text))
  }
  if (!is.null(nm) && "parts" %in% nm) return(.llmr_text_vec(x$parts))
  # A typed content block with no text field (image, tool_use, ...) carries no
  # text: do not let its other fields (type, source) leak in as content.
  if (!is.null(nm) && "type" %in% nm) return(character(0))
  unlist(lapply(x, .llmr_text_vec), use.names = FALSE)
}

# Coerce a possibly-NULL/empty value to a length-1 numeric, or NA.
#' @keywords internal
#' @noRd
.llmr_num1 <- function(x) {
  if (is.null(x) || !length(x)) return(NA_real_)
  suppressWarnings(as.numeric(x)[1])
}

# Inverse of .llmr_turns: canonical turns -> a named character vector suitable
# for passing back to llm_request_hash() or re-issuing a call.
#' @keywords internal
#' @noRd
.llmr_messages_from_turns <- function(turns) {
  if (!length(turns)) return(stats::setNames(character(0), character(0)))
  stats::setNames(vapply(turns, `[[`, character(1), "content"),
                  vapply(turns, `[[`, character(1), "role"))
}

# Transport knobs change HOW a call is issued, not WHAT is asked; they never
# enter the request identity. Drop them, drop NA/empty entries, sort by name so
# construction order does not matter.
#' @keywords internal
#' @noRd
.llmr_drop_transport <- function(model_params) {
  if (!is.list(model_params) || !length(model_params)) return(list())
  # Knobs that change HOW a call is issued or routed, not WHAT is asked, plus
  # local-only handles that never reach a provider body. None belong in request
  # identity.
  drop <- c("req_builder", "request_modifier", "response_modifier",
            "timeout", "api_url", "base_url", "max_tries", "verbose",
            "cache", "use_responses_api", "anthropic_beta",
            "vertex", "project", "location", "stream", "stream_options")
  keep <- model_params[setdiff(names(model_params), drop)]
  # Drop NULL, scalar NA, and zero-length entries so an absent or empty
  # parameter does not change the hash relative to a call that never set it.
  keep <- keep[vapply(keep, function(v)
    !(is.null(v) || length(v) == 0L ||
        (length(v) == 1L && is.atomic(v) && is.na(v))), logical(1))]
  # Normalize a single-element stop ("x") and a one-element list/array (["x"])
  # to the same value, since they are the same stop sequence.
  if (!is.null(keep$stop)) keep$stop <- as.character(unlist(keep$stop))
  if (!length(keep)) list() else keep[order(names(keep))]
}

# Pull the generation parameters out of a logged provider-specific request body:
# the non-structural top-level fields plus a Gemini-style generationConfig, with
# the common camelCase aliases (maxOutputTokens, topP) normalized to canonical
# names, then transport knobs dropped. The body mirror of
# .llmr_drop_transport(config$model_params): the two yield the same param object
# for the common chat path, which is what makes the config and log sides agree.
# It does not reverse every provider's full translation (e.g. an injected
# responseMimeType default, or Responses-API field renames); the archive's
# collision check is the backstop for calls that differ only in those.
#' @keywords internal
#' @noRd
.llmr_body_params <- function(request) {
  gen <- request$generationConfig %||% list()
  structural <- c("messages", "contents", "system", "systemInstruction",
                  "generationConfig", "model", "stream")
  top <- request[setdiff(names(request), structural)]
  mp <- c(top, gen)
  if (!is.null(mp$maxOutputTokens)) {
    mp$max_tokens <- mp$max_tokens %||% mp$maxOutputTokens; mp$maxOutputTokens <- NULL
  }
  if (!is.null(mp$topP)) {
    mp$top_p <- mp$top_p %||% mp$topP; mp$topP <- NULL
  }
  .llmr_drop_transport(mp)
}

# Narrow generation params from a logged body, kept for callers that want only
# the four most common keys (temperature, max_tokens, top_p, seed). Retained for
# completeness; the identity hash uses .llmr_body_params (all params).
#' @keywords internal
#' @noRd
.llmr_request_params <- function(request) {
  gen <- request$generationConfig %||% list()
  list(temperature = .llmr_num1(request$temperature %||% gen$temperature),
       max_tokens = .llmr_num1(request$max_tokens %||%
                                 request$max_completion_tokens %||%
                                 gen$maxOutputTokens),
       top_p = .llmr_num1(request$top_p %||% gen$topP),
       seed = .llmr_num1(request$seed %||% gen$seed))
}
