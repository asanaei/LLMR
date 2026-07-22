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
      sys <- .llmr_canonical_turn("system", request$systemInstruction)
      if (nzchar(sys$content)) turns[[length(turns) + 1L]] <- sys
      for (msg in request$contents %||% list()) {
        turns[[length(turns) + 1L]] <- .llmr_canonical_turn(
          msg$role %||% "user", msg$parts %||% msg$content %||% msg$text)
      }
      return(turns)
    }
    sys <- .llmr_canonical_turn("system", request$system)
    if (nzchar(sys$content)) turns[[length(turns) + 1L]] <- sys
    for (msg in request$messages %||% list()) {
      turns[[length(turns) + 1L]] <- .llmr_canonical_turn(
        msg$role %||% "user", msg$content %||% msg$parts %||% msg$text,
        msg$non_text)
    }
    return(turns)
  }

  if (is.null(messages)) return(list())
  if (is.character(messages)) {
    if (!is.null(names(messages)) && any(names(messages) %in% "file")) {
      return(.llmr_turns(messages = .normalize_messages(messages)))
    }
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
        .llmr_canonical_turn(msg$role %||% roles[i], msg$content, msg$non_text)
      } else {
        .llmr_canonical_turn(roles[i], msg)
      }
    }))
  }
  rlang::abort("`messages` must be a character vector or a list of role/content pairs.")
}

# Text-only turns retain their historical two-field representation. A third
# field appears only when a turn carries inline content, preserving every
# existing text-only request hash.
#' @keywords internal
#' @noRd
.llmr_canonical_turn <- function(role, content, non_text = NULL) {
  out <- list(role = .llmr_role(role), content = .llmr_text(content))
  hashes <- non_text %||% .llmr_nontext_hashes(content)
  if (length(hashes)) out$non_text <- unname(as.character(hashes))
  out
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

# Hash the decoded bytes of inline file/image content. Provider builders spell
# the same payload differently, so only the content digest enters the canonical
# turn, not the provider-specific block type or MIME wrapper.
#' @keywords internal
#' @noRd
.llmr_payload_hash <- function(x, encoding = c("auto", "base64", "file")) {
  encoding <- match.arg(encoding)
  if (encoding == "file") {
    if (!file.exists(x)) stop("File not found at path: ", x)
    x <- readBin(x, what = "raw", n = file.info(x)$size)
  }
  if (is.raw(x)) return(digest::digest(x, algo = "sha256", serialize = FALSE))

  x <- as.character(x)[1]
  marker <- regexec("sha256=([0-9a-f]{64})", x)
  hit <- regmatches(x, marker)[[1]]
  if (length(hit) == 2L) return(hit[[2]])

  if (startsWith(x, "data:")) {
    comma <- regexpr(",", x, fixed = TRUE)[1]
    meta <- substr(x, 1L, comma - 1L)
    payload <- substr(x, comma + 1L, nchar(x))
    x <- if (grepl(";base64", meta, ignore.case = TRUE)) {
      base64enc::base64decode(payload)
    } else {
      charToRaw(enc2utf8(utils::URLdecode(payload)))
    }
  } else if (encoding == "base64") {
    x <- base64enc::base64decode(x)
  } else {
    x <- charToRaw(enc2utf8(x))
  }
  digest::digest(x, algo = "sha256", serialize = FALSE)
}

#' @keywords internal
#' @noRd
.llmr_nontext_hashes <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.raw(x)) return(.llmr_payload_hash(x))
  if (!is.list(x)) return(character(0))

  nm <- names(x)
  if (!is.null(nm)) {
    type <- tolower(as.character(x$type %||% "")[1])
    if (type %in% c("text", "input_text", "output_text")) return(character(0))
    if ("path" %in% nm && type %in% c("file", "image", "document", "input_file")) {
      return(.llmr_payload_hash(path.expand(x$path), "file"))
    }
    if ("image_url" %in% nm) {
      url <- if (is.list(x$image_url)) x$image_url$url else x$image_url
      return(.llmr_payload_hash(url))
    }
    if (!is.null(x$data_uri)) return(.llmr_payload_hash(x$data_uri))
    inline <- x$inlineData %||% x$inline_data
    if (is.list(inline) && !is.null(inline$data)) {
      return(.llmr_payload_hash(inline$data, "base64"))
    }
    if (is.list(x$source) && !is.null(x$source$data)) {
      return(.llmr_payload_hash(x$source$data, "base64"))
    }
    if (!is.null(x$file_data)) return(.llmr_payload_hash(x$file_data))
    if (!is.null(x$data) && type %in% c("file", "image", "document", "input_image")) {
      return(.llmr_payload_hash(x$data, "base64"))
    }
  }
  unlist(lapply(x, .llmr_nontext_hashes), use.names = FALSE)
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
  if (any(vapply(turns, function(x) length(x$non_text %||% character()), integer(1)))) {
    return(unname(turns))
  }
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
  # identity. json_schema and llmr_schema_tool are enable_structured_output()'s
  # local bookkeeping (the schema itself travels in the provider-ready field:
  # response_format, tools, or response_json_schema), so a config and the body
  # built from it agree once they are excluded.
  drop <- c("req_builder", "request_modifier", "response_modifier",
            "timeout", "api_url", "base_url", "max_tries", "verbose",
            "cache", "use_responses_api", "anthropic_beta",
            "vertex", "project", "location", "stream", "stream_options",
            "json_schema", "llmr_schema_tool")
  keep <- model_params[setdiff(names(model_params), drop)]
  # Drop NULL, scalar NA, and zero-length entries so an absent or empty
  # parameter does not change the hash relative to a call that never set it.
  keep <- keep[vapply(keep, function(v)
    !(is.null(v) || length(v) == 0L ||
        (length(v) == 1L && is.atomic(v) && is.na(v))), logical(1))]
  # Normalize a single-element stop ("x") and a one-element list/array (["x"])
  # to the same value, since they are the same stop sequence.
  if (!is.null(keep$stop)) keep$stop <- as.character(unlist(keep$stop))
  # max_completion_tokens (the o-series / gpt-5 spelling, and what LLMR renames
  # max_tokens to for those models) is the same request as max_tokens; fold it in
  # so a config and its logged body agree whichever spelling was supplied.
  if (!is.null(keep$max_completion_tokens)) {
    keep$max_tokens <- keep$max_tokens %||% keep$max_completion_tokens
    keep$max_completion_tokens <- NULL
  }
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

  # Un-nest Gemini's thinkingConfig to canonical thinking_budget/include_thoughts.
  if (is.list(mp$thinkingConfig)) {
    tc <- mp$thinkingConfig; mp$thinkingConfig <- NULL
    if (!is.null(tc$thinkingBudget))  mp$thinking_budget  <- mp$thinking_budget  %||% tc$thinkingBudget
    if (!is.null(tc$includeThoughts)) mp$include_thoughts <- mp$include_thoughts %||% tc$includeThoughts
  }
  # Gemini injects responseMimeType = "text/plain" on every text call; the config
  # that produced it never set that default, so drop it (a non-default value is a
  # real request the config also carries and is renamed below).
  if (identical(mp$responseMimeType, "text/plain")) mp$responseMimeType <- NULL

  # Reverse the provider-specific renames back to canonical (OpenAI) spelling so a
  # logged provider body and the config that produced it hash identically. Only
  # unambiguous renames are reversed; the logprobs family, whose body key means
  # different things on OpenAI vs Gemini, is left as-is (documented above), as is
  # the structural OpenAI response_format wrapper.
  aliases <- c(maxOutputTokens = "max_tokens", max_completion_tokens = "max_tokens",
               topP = "top_p", topK = "top_k",
               presencePenalty = "presence_penalty",
               frequencyPenalty = "frequency_penalty",
               thinkingBudget = "thinking_budget", includeThoughts = "include_thoughts",
               responseMimeType = "response_mime_type",
               responseJsonSchema = "response_json_schema",
               responseSchema = "response_schema")
  for (from in names(aliases)) {
    to <- aliases[[from]]
    if (!is.null(mp[[from]])) { mp[[to]] <- mp[[to]] %||% mp[[from]]; mp[[from]] <- NULL }
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
