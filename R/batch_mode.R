# batch_mode.R
# -------------------------------------------------------------------
# Generative row batching for llm_fn() / llm_mutate() and their tag and
# structured variants.
#
# Several input rows are packed into ONE generative request, carried as
# numbered <row_i>...</row_i> XML blocks, then de-multiplexed back into the
# original rows. This is distinct from embedding batching
# (get_batched_embeddings(), which splits many texts across several embedding
# calls); see the "Row batching" discussion in llm_config()/llm_mutate().
#
# Everything here is a thin layer ABOVE call_llm_broadcast()/call_llm_par();
# those engines are unchanged. The default .rows_per_prompt = 1 never enters this
# file: the entry points keep their original one-call-per-row path.
# -------------------------------------------------------------------

# ----- Argument validation (shared by entry points) ------------------------

#' Validate `.rows_per_prompt` and report whether batching is active
#'
#' @param rows_per_prompt The user-supplied `.rows_per_prompt`.
#' @return `TRUE` when batching is active (`>1` or `Inf`), else `FALSE`.
#'   `is.infinite` is checked before any integer coercion, since
#'   `as.integer(Inf)` is `NA`.
#' @keywords internal
#' @noRd
.validate_rows_per_prompt <- function(rows_per_prompt) {
  if (!is.numeric(rows_per_prompt) || length(rows_per_prompt) != 1L ||
      is.na(rows_per_prompt) || rows_per_prompt < 1) {
    stop("`.rows_per_prompt` must be a single number >= 1, or Inf.", call. = FALSE)
  }
  is.infinite(rows_per_prompt) || rows_per_prompt > 1
}

#' Error if batching is requested for an embedding configuration
#' @keywords internal
#' @noRd
.assert_batch_not_embedding <- function(config) {
  if (isTRUE(config$embedding) ||
      grepl("embedding", config$model, ignore.case = TRUE)) {
    stop(".rows_per_prompt controls generative row batching and does not apply to ",
         "embeddings; use get_batched_embeddings(batch_size = ) for ",
         "embedding-call chunking.", call. = FALSE)
  }
  invisible(TRUE)
}

# ----- Partitioning --------------------------------------------------------

#' Partition row indices into contiguous batches
#'
#' Pure function of `(n, k)`: no RNG, no provider state, so the row-to-batch
#' assignment is fully reproducible. `k = Inf` yields a single batch.
#'
#' @param n Number of rows (non-negative integer).
#' @param k Batch size (positive integer, or `Inf`).
#' @return A list of integer vectors of contiguous indices. `list()` when
#'   `n == 0`.
#' @keywords internal
#' @noRd
.batch_partition <- function(n, k) {
  n <- as.integer(n)
  if (is.na(n) || n <= 0L) return(list())
  if (is.infinite(k)) return(list(seq_len(n)))
  k <- max(1L, as.integer(k))
  starts <- seq.int(1L, n, by = k)
  lapply(starts, function(s) seq.int(s, min(s + k - 1L, n)))
}

# ----- Content safety ------------------------------------------------------

#' Escape XML metacharacters in row content sent to the model
#'
#' User content may legitimately contain `<`, `>` or `&` (for example a row
#' whose text is itself `"</row_2>"`). Escaping the data we SEND prevents such
#' content from forging a `<row_i>` boundary or a field tag. The model's own
#' output is never escaped; we decode the entities back with
#' `.decode_tag_entities()` when surfacing a plain answer.
#'
#' @param x Character vector.
#' @return `x` with `&`, `<`, `>` replaced by entities (in that order).
#' @keywords internal
#' @noRd
.xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;",  x, fixed = TRUE)
}

#' Assemble per-row texts into a numbered <row_i> payload block
#'
#' The texts are already glue-expanded; they are wrapped with `paste0` only,
#' never a second glue/sprintf pass, so a resolved cell containing `{` or `}`
#' survives literally. Indices are within-batch `1..m`.
#'
#' @param texts Character vector of already-expanded per-row content.
#' @return A single character scalar: the concatenated `<row_i>` block.
#' @keywords internal
#' @noRd
.assemble_payload <- function(texts) {
  idx <- seq_along(texts)
  paste0("<row_", idx, ">\n", .xml_escape(texts), "\n</row_", idx, ">",
         collapse = "\n")
}

# ----- Prompt construction -------------------------------------------------

#' Imperative instruction for plain (non-tag) batched generation
#' @param m Number of items in the batch.
#' @keywords internal
#' @noRd
.batch_instruction_plain <- function(m) {
  paste(
    sprintf("You are answering %d independent items in ONE response.", m),
    "The items are wrapped in numbered tags <row_1> ... </row_1>,",
    sprintf("<row_2> ... </row_2>, and so on, up to <row_%d>.", m),
    "Return your answer for EVERY item inside a matching tag with the SAME",
    "number, like:",
    "<row_1>",
    "your full answer to item 1",
    "</row_1>",
    "Rules:",
    sprintf("- Emit exactly %d <row_i> blocks, one per item, using the SAME", m),
    sprintf("  numbers 1..%d you were given.", m),
    "- Put ONLY the answer inside each block. No prose, headings, Markdown, or",
    "  code fences outside the blocks.",
    "- Do not merge, skip, reorder by content, renumber, or invent items.",
    "- If an item's answer is empty, still emit an empty block: <row_i></row_i>.",
    sep = "\n"
  )
}

#' Imperative instruction for batched tag (soft-structured) generation
#'
#' Deliberately REQUIRES one level of nesting (outer `<row_i>` around the inner
#' field tags), reversing the flat-mode "do not nest" guidance. It is appended
#' after any user system text so the later, more specific instruction governs.
#'
#' @param tags Character vector of field tag names.
#' @param m Number of items in the batch.
#' @keywords internal
#' @noRd
.batch_tag_prompt <- function(tags, m) {
  tag_lines <- paste0("<", tags, ">...</", tags, ">")
  paste(
    c(
      sprintf("You are answering %d independent items in ONE response.", m),
      "The items are wrapped in numbered tags <row_1> ... </row_1>,",
      sprintf("<row_2> ... </row_2>, and so on, up to <row_%d>.", m),
      sprintf("For EACH of the %d items, return one <row_i> block using the SAME", m),
      "number, and INSIDE that block place these field tags exactly (this one",
      "level of nesting is REQUIRED here):",
      "<row_1>",
      tag_lines,
      "</row_1>",
      "Rules:",
      "- The outer <row_i> wraps the inner field tags. Nest only this one level.",
      sprintf("- Use the SAME numbers 1..%d you were given; do not skip, reorder by", m),
      "  content, renumber, or invent items.",
      paste0("- Inside each block use ONLY the field tags ",
             paste0("<", tags, ">", collapse = ", "),
             "; do not nest field tags inside one another, and do not emit a",
             " field tag outside a <row_i> block."),
      "- If a field is unknown, emit empty tags, e.g. <age></age>.",
      "- No prose, Markdown, or code fences outside the blocks."
    ),
    collapse = "\n"
  )
}

#' Imperative instruction for batched structured (JSON) generation
#'
#' Asks for a single wrapper object `{"results":[{"row":<int>, ...}]}` so the
#' reply stays valid for OpenAI-compatible `response_format = json_object`
#' (which forbids a top-level array). The integer `row` field is the join key
#' back to the source rows.
#'
#' @param m Number of items in the batch.
#' @keywords internal
#' @noRd
.batch_struct_instruction <- function(m) {
  paste(
    sprintf("You extract structured data for %d independent items in ONE response.", m),
    sprintf("The items are wrapped in numbered tags <row_1> ... </row_1> up to <row_%d>.", m),
    "Return ONLY a single JSON object of the form",
    '{"results": [ {"row": <int>, ...fields... }, ... ]}',
    sprintf("with exactly %d array elements, one per item, and \"row\" set to the SAME", m),
    "integer i from <row_i>. Each element must otherwise satisfy the requested",
    "schema. Do not skip, reorder by content, renumber, or invent items. If a",
    "value is unknown use null. No prose, no code fences.",
    sep = "\n"
  )
}

#' Append a batch instruction to the system channel of a message spec
#'
#' Mirrors `.add_tag_prompt()` in handling the three message shapes
#' (list-of-objects, named character vector, or `NULL` + system_prompt). The
#' instruction always lands on the system side; the data payload is placed
#' separately by `.compose_batch_message()`.
#'
#' @param .messages,.system_prompt As in `.add_tag_prompt()`.
#' @param instruction Character scalar to append to the system message.
#' @return `list(.messages =, .system_prompt =)`.
#' @keywords internal
#' @noRd
.add_batch_prompt <- function(.messages, .system_prompt, instruction) {
  # reuse the exact shape handling already proven in .add_tag_prompt()
  .add_tag_prompt_like(.messages, .system_prompt, instruction)
}

# Shared shape handler (the body of .add_tag_prompt with a free-form
# instruction). Kept separate so both tag and batch paths use one implementation.
#' @keywords internal
#' @noRd
.add_tag_prompt_like <- function(.messages, .system_prompt, instruction) {
  if (!is.null(.messages)) {
    if (is.list(.messages) && length(.messages) > 0 &&
        is.list(.messages[[1]]) && !is.null(.messages[[1]]$role)) {
      is_sys <- vapply(.messages, function(x) identical(x$role, "system"), logical(1))
      if (any(is_sys)) {
        idx <- which(is_sys)[1]
        .messages[[idx]]$content <- paste(.messages[[idx]]$content, instruction, sep = "\n\n")
      } else {
        .messages <- c(list(list(role = "system", content = instruction)), .messages)
      }
      return(list(.messages = .messages, .system_prompt = .system_prompt))
    }
    if (is.character(.messages)) {
      if (is.null(names(.messages))) {
        names(.messages) <- rep("user", length(.messages))
      }
      sys <- which(names(.messages) == "system")
      if (length(sys)) {
        .messages[[sys[[1]]]] <- paste(.messages[[sys[[1]]]], instruction, sep = "\n\n")
      } else {
        .messages <- c(system = instruction, .messages)
      }
      return(list(.messages = .messages, .system_prompt = .system_prompt))
    }
  }
  list(
    .messages = NULL,
    .system_prompt = paste(c(.system_prompt, instruction), collapse = "\n\n")
  )
}

#' Compose one batched request message (system instruction + data payload)
#'
#' Produces a named character vector `c(system = ..., user = ...)` shaped exactly
#' like what [call_llm_broadcast()] already accepts.
#'
#' @param per_row_texts Already-expanded per-row content for this batch.
#' @param system_text Optional shared system prompt supplied by the caller.
#' @param instruction The batch imperative instruction (always system-side).
#' @param payload_where `"user"` or `"system"`: where the `<row_i>` data block
#'   is appended.
#' @return Named character vector with `system` and `user` entries.
#' @keywords internal
#' @noRd
.compose_batch_message <- function(per_row_texts, system_text, instruction,
                                   payload_where = c("user", "system")) {
  payload_where <- match.arg(payload_where)
  payload <- .assemble_payload(per_row_texts)
  sys <- paste(c(system_text, instruction), collapse = "\n\n")

  if (payload_where == "user") {
    c(system = sys, user = payload)
  } else {
    c(system = paste(sys, payload, sep = "\n\n"),
      user   = "The items are listed in the system message above.")
  }
}

# ----- Batchability guard --------------------------------------------------

#' Reject inputs that cannot be carried as text <row_i> blocks
#'
#' Runs on the EVALUATED per-row messages (file paths may be data-conditional).
#' Batched generative mode supports a single shared system prompt and one user
#' text per row only.
#'
#' @param eval_msgs A list of evaluated per-row messages (named character
#'   vectors or bare strings).
#' @return Invisibly `TRUE`; throws on unsupported content.
#' @keywords internal
#' @noRd
.assert_batchable <- function(eval_msgs) {
  has_file <- any(vapply(eval_msgs, function(m) {
    !is.null(names(m)) && any(names(m) == "file")
  }, logical(1)))
  if (isTRUE(has_file)) {
    stop("Batched generative mode does not support file/multimodal content. ",
         "Use .rows_per_prompt = 1 for multimodal rows.", call. = FALSE)
  }
  # Assistant turns cannot be carried in a <row_i> payload at all; silently
  # dropping them would change the conversation, so refuse outright.
  has_assistant <- any(vapply(eval_msgs, function(m) {
    !is.null(names(m)) && any(names(m) == "assistant")
  }, logical(1)))
  if (isTRUE(has_assistant)) {
    stop("Batched generative mode does not support assistant turns in message ",
         "templates. Use .rows_per_prompt = 1 for multi-turn messages.", call. = FALSE)
  }
  # per-row system turns must be identical across rows (a single shared system
  # prompt); otherwise they cannot be hoisted out of the batch.
  nonuser <- lapply(eval_msgs, function(m) {
    if (is.null(names(m))) return(character(0))
    m[names(m) %in% "system"]
  })
  uniq <- unique(lapply(nonuser, unname))
  if (length(uniq) > 1L) {
    stop("Batched generative mode requires a single shared system prompt. ",
         "Use .rows_per_prompt = 1 for per-row system prompts.", call. = FALSE)
  }
  invisible(TRUE)
}

# ----- The nested one-level scanner ----------------------------------------

#' Split a batched response into per-row blocks (one-level nested scanner)
#'
#' A stateful tokenizer rather than a single nested regex, so it is robust to
#' the ways a model can deviate from the protocol: reordering, dropped or
#' duplicated blocks, hallucinated indices, accidental nesting, missing closing
#' tags, and truncated output. Only fully CLOSED blocks are committed, so a cut
#' tail is detected structurally even when the provider mislabels the finish
#' reason.
#'
#' Inner field tags (for example `<age>`) never match the `row[_-]?\\d+`
#' opener pattern, so they pass through into block content untouched and are
#' parsed later by the existing flat tag parser.
#'
#' @param text One batch response (character scalar).
#' @param m Expected within-batch count; local ids are `1..m`.
#' @return `list(blocks =, report =)`. `blocks` is a length-`m` list; element
#'   `i` is the raw content of `<row_i>` or `NA`/`NULL` when absent or suspect.
#'   `report$found` is the integer ids that resolved; `report$nesting` flags
#'   that an accidental-nesting/implicit-close event occurred.
#' @keywords internal
#' @noRd
.batch_split_rows <- function(text, m) {
  empty <- list(blocks = vector("list", m),
                report = list(found = integer(0), nesting = FALSE))
  if (is.null(text) || length(text) == 0L) return(empty)
  s <- text[[1]]
  if (is.na(s) || !nzchar(s)) return(empty)
  s <- .strip_code_fences(s)

  pat <- "(?i)<\\s*(/?)\\s*row[_-]?(\\d+)\\s*(?:[^>]*)>"
  g   <- gregexpr(pat, s, perl = TRUE)[[1]]
  if (g[1] == -1L) return(empty)

  starts <- as.integer(g)
  lens   <- attr(g, "match.length")
  ends   <- starts + lens - 1L
  caps   <- regmatches(s, gregexpr(pat, s, perl = TRUE))[[1]]
  is_close <- grepl("<\\s*/", caps)
  ids <- suppressWarnings(as.integer(sub(pat, "\\2", caps, perl = TRUE)))

  blocks <- vector("list", m)
  nesting_hit <- FALSE
  open_id <- NA_integer_
  content_start <- NA_integer_

  for (t in seq_along(caps)) {
    id <- ids[t]
    if (is.na(id)) next                       # malformed/overflow id -> ignore
    if (!is_close[t]) {                       # OPENER
      if (!is.na(open_id)) {
        if (id == open_id) next               # duplicate opener: ignore stutter
        nesting_hit <- TRUE                   # implicit close / accidental nest
        if (open_id >= 1L && open_id <= m) blocks[[open_id]] <- NA_character_
      }
      open_id <- id
      content_start <- ends[t] + 1L
    } else {                                  # CLOSER
      if (!is.na(open_id) && id == open_id) {
        if (id >= 1L && id <= m && content_start <= starts[t]) {
          val <- substr(s, content_start, starts[t] - 1L)
          prev <- blocks[[id]]
          empty_prev <- is.null(prev) || is.na(prev) || !nzchar(trimws(prev))
          if (empty_prev) blocks[[id]] <- val   # first-complete (non-empty) wins
        }
        open_id <- NA_integer_
        content_start <- NA_integer_
      }
      # closer with no/other open frame -> stray, ignore
    }
  }
  if (!is.na(open_id) && open_id >= 1L && open_id <= m) {
    blocks[[open_id]] <- NA_character_        # EOF with frame open -> truncated
  }

  found <- which(vapply(blocks, function(b) {
    !(is.null(b) || (length(b) == 1L && is.na(b)))
  }, logical(1)))
  list(blocks = blocks, report = list(found = found, nesting = nesting_hit))
}

#' Parse a batched, row-wrapped tag response into per-row field lists
#'
#' Splits a single batched reply into its numbered `<row_i>` blocks and then
#' applies the standard flat tag parser ([llm_parse_tags()]) inside each block.
#' This is the parsing counterpart to the `<row_i>` protocol that LLMR uses when
#' `.rows_per_prompt > 1` together with `.tags`; it is exported so the protocol is
#' inspectable and testable on its own.
#'
#' @param text Character scalar: one batched model response containing
#'   `<row_i>...</row_i>` blocks, each wrapping flat field tags.
#' @param tags Character vector of field tag names to extract within each block.
#' @param m Integer: the number of items expected in the batch (local ids
#'   `1..m`).
#' @return A list of length `m`. Element `i` is the named list returned by
#'   [llm_parse_tags()] for `<row_i>`, or `NULL` when that block is absent,
#'   truncated, or otherwise unrecoverable.
#'
#' @details
#' Robustness mirrors the internal scanner: reordered, duplicated, hallucinated,
#' truncated, or accidentally nested `<row_i>` blocks are handled; only fully
#' closed blocks contribute. Inner field tags are extracted by the same parser
#' used in non-batched tag mode, so values coerce and decode identically.
#'
#' @seealso [llm_parse_tags()], [llm_parse_tags_col()], [llm_mutate()],
#'   [llm_fn()]
#' @examples
#' txt <- paste(
#'   "<row_1><age>21</age><job>barista</job></row_1>",
#'   "<row_2><age>34</age><job>welder</job></row_2>",
#'   sep = "\n"
#' )
#' llm_parse_rowpack_tags(txt, tags = c("age", "job"), m = 2)
#' @export
llm_parse_rowpack_tags <- function(text, tags, m) {
  tags <- .validate_tags(tags)
  m <- as.integer(m)
  if (is.na(m) || m < 1L) stop("`m` must be a positive integer.", call. = FALSE)
  seg <- .batch_split_rows(text, m)
  lapply(seg$blocks, function(b) {
    if (is.null(b) || (length(b) == 1L && is.na(b))) return(NULL)
    llm_parse_tags(b, tags = tags)
  })
}

# ----- Structured de-multiplex ---------------------------------------------

#' De-multiplex a batched structured (wrapper-object) reply by integer `row`
#'
#' Parses `{"results":[{"row":i, ...}]}` and returns, for each local id `1..m`,
#' the JSON text of that element (so the existing per-row structured parser can
#' run unchanged), or `NA` when the element is absent/duplicated/out of range.
#'
#' @param text One batch response (character scalar).
#' @param m Expected within-batch count.
#' @return A length-`m` character vector; `NA_character_` for missing ids.
#' @keywords internal
#' @noRd
.batch_split_struct <- function(text, m) {
  out <- rep(NA_character_, m)
  if (is.null(text) || length(text) == 0L) return(out)
  s <- text[[1]]
  if (is.na(s) || !nzchar(s)) return(out)
  s <- .strip_code_fences(s)
  obj <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(obj)) return(out)

  # accept {"results":[...]} or a bare top-level array
  items <- if (is.list(obj) && !is.null(obj$results)) obj$results else
    if (is.null(names(obj))) obj else NULL
  if (is.null(items) || !length(items)) return(out)

  seen <- logical(m)
  for (el in items) {
    if (!is.list(el)) next
    r <- suppressWarnings(as.integer(el$row %||% NA))
    if (is.na(r) || r < 1L || r > m) next
    if (seen[r]) next                       # first occurrence wins
    seen[r] <- TRUE
    el$row <- NULL                          # the row key is protocol, not data
    out[r] <- jsonlite::toJSON(el, auto_unbox = TRUE, null = "null")
  }
  out
}

# ----- Shared metadata -----------------------------------------------------

#' Build the per-row diagnostic record from a shared batch response row
#'
#' Token counts and wall-clock duration are NOT row-separable, so both are
#' attributed to a single row (the engine assigns them to the first resolved
#' row and NA to the rest, so that summing either column over rows is correct);
#' descriptive scalars are copied. `finish_reason`/`success` are set by the
#' engine per row to reflect the row's actual outcome.
#'
#' @param res_b A one-row `call_llm_par`-shaped tibble for the batch call.
#' @param with_tokens Logical; whether this row carries the batch totals.
#' @return A one-row list of diagnostic fields.
#' @keywords internal
#' @noRd
.shared_batch_meta <- function(res_b, with_tokens) {
  na_int <- NA_integer_
  list(
    raw_response_json = res_b$raw_response_json[[1]] %||% NA_character_,
    error_message     = res_b$error_message[[1]]     %||% NA_character_,
    sent_tokens       = if (with_tokens) res_b$sent_tokens[[1]]      else na_int,
    rec_tokens        = if (with_tokens) res_b$rec_tokens[[1]]       else na_int,
    total_tokens      = if (with_tokens) res_b$total_tokens[[1]]     else na_int,
    reasoning_tokens  = if (with_tokens) res_b$reasoning_tokens[[1]] else na_int,
    cached_tokens     = if (with_tokens && "cached_tokens" %in% names(res_b)) res_b$cached_tokens[[1]] else na_int,
    response_id       = res_b$response_id[[1]] %||% NA_character_,
    duration          = if (with_tokens) (res_b$duration[[1]] %||% NA_real_) else NA_real_,
    status_code       = res_b$status_code[[1]]  %||% NA_integer_,
    error_code        = res_b$error_code[[1]]   %||% NA_character_,
    bad_param         = res_b$bad_param[[1]]    %||% NA_character_
  )
}

# ----- The batched driver --------------------------------------------------

#' Run a batched generative job end to end
#'
#' Partitions rows, composes one `<row_i>` request per batch, dispatches through
#' the unchanged [call_llm_broadcast()], de-multiplexes the reply back to rows,
#' and recovers unresolved rows per `.rowpack_recovery`. Returns a tibble with the
#' exact [call_llm_par()] column set (in original row order, always `n` rows)
#' plus `rowpack_id`, `rows_per_prompt`, `rowpack_row`.
#'
#' @param config An `llm_config`.
#' @param per_row_texts Character vector of already-expanded per-row content.
#' @param system_text Optional shared system prompt.
#' @param mode One of `"plain"`, `"tags"`, `"structured"`.
#' @param tags Field tags (tag mode).
#' @param rows_per_prompt,rowpack_payload,rowpack_recovery The batching controls.
#' @param dots Extra args forwarded to [call_llm_broadcast()] (e.g. `tries`).
#' @param .broadcast Test seam; defaults to [call_llm_broadcast()].
#' @return A tibble; see Description.
#' @keywords internal
#' @noRd
.run_batched <- function(config, per_row_texts, system_text = NULL,
                         mode = c("plain", "tags", "structured"),
                         tags = NULL,
                         rows_per_prompt = 1L,
                         rowpack_payload = "user",
                         rowpack_recovery = "halve_recursive",
                         dots = list(),
                         .broadcast = call_llm_broadcast) {
  mode <- match.arg(mode)
  n <- length(per_row_texts)

  answers <- rep(NA_character_, n)
  status  <- rep("unresolved", n)
  meta    <- vector("list", n)
  bid_col <- rep(NA_character_, n)
  bsz_col <- rep(NA_integer_, n)
  bri_col <- rep(NA_integer_, n)
  finish  <- rep(NA_character_, n)

  if (n == 0L) {
    return(.batch_assemble_res(answers, status, meta, finish,
                               bid_col, bsz_col, bri_col, grouped = FALSE))
  }

  # memoization is unsafe for multi-row payloads; force it off and warn once.
  if (isTRUE(dots$memoize)) {
    warning("Memoization is disabled for batched calls (multi-row payloads ",
            "are not safely cacheable).", call. = FALSE)
  }
  dots$memoize <- FALSE

  instruction_for <- function(m) {
    switch(mode,
      plain      = .batch_instruction_plain(m),
      tags       = .batch_tag_prompt(tags, m),
      structured = .batch_struct_instruction(m))
  }

  # Instruction for an UNWRAPPED singleton recovery (one row, no <row_i>). It is
  # the SINGLE-ITEM form, NOT the batch <row_i> wrapper: tags reuse the exact
  # non-batched tag instruction (.tag_prompt) so a recovered singleton is
  # format-identical to the .rows_per_prompt = 1 path; structured reuses the
  # canonical bare JSON-object instruction. plain needs none.
  singleton_instruction <- function() {
    switch(mode,
      plain      = NULL,
      tags       = .tag_prompt(tags),
      structured = .llmr_json_object_instruction)
  }

  call_budget <- 3L * n
  calls_made  <- 0L
  max_depth   <- ceiling(log2(max(if (is.infinite(rows_per_prompt)) n else rows_per_prompt, 2))) + 2L

  # one batch call -> a one-row res_b tibble (via the broadcast seam)
  do_call_batch <- function(rows) {
    msg <- .compose_batch_message(per_row_texts[rows], system_text,
                                  instruction_for(length(rows)), rowpack_payload)
    do.call(.broadcast, c(list(config = config, messages = list(msg)), dots))
  }

  commit_row <- function(g, ans, fr, st, bid, m, li, with_tokens, res_b) {
    if (status[g] != "unresolved") return(invisible())  # commit-once
    answers[g] <<- ans
    finish[g]  <<- fr
    status[g]  <<- st
    bid_col[g] <<- bid
    bsz_col[g] <<- as.integer(m)
    bri_col[g] <<- as.integer(li)
    meta[[g]]  <<- .shared_batch_meta(res_b, with_tokens)
  }

  fail_rows <- function(rows, fr, res_b = NULL, bid = NA_character_,
                        tokens_available = FALSE) {
    # When the failing call's spend has not been credited to any committed row,
    # attach it to the first failed row so token sums still reflect reality.
    first_assigned <- !isTRUE(tokens_available)
    for (g in rows) {
      if (status[g] != "unresolved") next
      finish[g]  <<- fr
      status[g]  <<- "failed"
      bid_col[g] <<- bid
      meta[[g]]  <<- if (is.null(res_b))
        .empty_meta() else .shared_batch_meta(res_b, !first_assigned)
      first_assigned <- TRUE
    }
  }

  # ---- queue-driven recovery ------------------------------------------------
  # initial queue: partition the n rows by rows_per_prompt (Inf -> one batch)
  init_parts <- .batch_partition(n, rows_per_prompt)
  queue <- lapply(seq_along(init_parts), function(j) {
    list(rows = init_parts[[j]], depth = 0L, bid = as.character(j))
  })

  while (length(queue)) {
    b <- queue[[1]]; queue[[1]] <- NULL
    g_rows <- b$rows
    m <- length(g_rows)
    if (!length(g_rows)) next

    # m == 1 -> UNWRAPPED singleton (single parse semantics). The user payload
    # stays the bare row text (no <row_i>), but the mode's single-item field/JSON
    # instruction is carried SYSTEM-side so a recovered tags/structured singleton
    # emits the right format (previously it carried none and was parsed as if it
    # did). The parse below is unchanged and matches the non-batched single-row
    # path. plain mode is unaffected (singleton_instruction() is NULL).
    if (m == 1L) {
      if (calls_made >= call_budget) { fail_rows(g_rows, "error:rowpack_budget"); next }
      g <- g_rows[1]
      sys1 <- paste(c(system_text, singleton_instruction()), collapse = "\n\n")
      smsg <- if (!nzchar(sys1)) per_row_texts[g] else
        c(system = sys1, user = per_row_texts[g])
      rb <- do.call(.broadcast, c(list(config = config, messages = list(smsg)), dots))
      calls_made <- calls_made + 1L
      if (isTRUE(rb$success[1])) {
        ans <- if (mode == "tags") rb$response_text[1] else
          .decode_tag_entities(trimws(rb$response_text[1]))
        commit_row(g, ans, rb$finish_reason[1],
                   if (b$depth == 0L) "ok" else "recovered",
                   b$bid, 1L, 1L, TRUE, rb)
      } else {
        fail_rows(g, rb$finish_reason[1] %||% "error:rowpack_call", rb, b$bid,
                  tokens_available = TRUE)
      }
      next
    }

    if (calls_made >= call_budget) { fail_rows(g_rows, "error:rowpack_budget"); next }
    res_b <- do_call_batch(g_rows)
    calls_made <- calls_made + 1L

    U <- integer(0)
    head_only <- FALSE
    ctxerr <- FALSE
    tokens_credited <- FALSE

    if (!isTRUE(res_b$success[1])) {
      U <- g_rows
      ctxerr <- grepl("context_length|maximum context|too many tokens|context window",
                      paste(res_b$error_message[1], res_b$error_code[1]),
                      ignore.case = TRUE)
    } else {
      trunc <- identical(res_b$finish_reason[1], "length")
      if (mode == "structured") {
        blk <- .batch_split_struct(res_b$response_text[1], m)
        found <- which(!is.na(blk))
        present_fun <- function(li) !is.na(blk[[li]])
        get_ans <- function(li) blk[[li]]   # JSON text, already row-key-stripped
        nesting_suspect <- integer(0)
      } else {
        seg <- .batch_split_rows(res_b$response_text[1], m)
        found <- seg$report$found
        present_fun <- function(li) {
          bl <- seg$blocks[[li]]
          !(is.null(bl) || (length(bl) == 1L && is.na(bl)))
        }
        get_ans <- function(li) {
          bl <- seg$blocks[[li]]
          if (mode == "tags") bl else .decode_tag_entities(trimws(bl))
        }
        nesting_suspect <- if (isTRUE(seg$report$nesting)) found else integer(0)
      }

      head_only <- length(found) <= 1L
      last_present <- if (length(found)) max(found) else 0L
      first_tok_assigned <- FALSE

      for (li in seq_len(m)) {
        g <- g_rows[li]
        present <- present_fun(li)
        suspect_tail <- trunc && present && li == last_present
        if (!present || suspect_tail) { U <- c(U, g); next }
        ans <- get_ans(li)
        # plain-mode contamination guard: a synthesized row token survived
        if (mode == "plain" && grepl("</?row[_-]?\\d+", ans %||% "")) {
          U <- c(U, g); next
        }
        commit_row(g, ans, "stop",
                   if (b$depth == 0L) "ok" else "recovered",
                   b$bid, m, li, !first_tok_assigned, res_b)
        first_tok_assigned <- TRUE
      }
      tokens_credited <- first_tok_assigned
    }

    if (!length(U)) next

    # ---- decide recovery for the unresolved set U --------------------------
    if (identical(rowpack_recovery, "none") || b$depth >= max_depth) {
      fail_rows(U, .terminal_reason(res_b), res_b, b$bid,
                tokens_available = !tokens_credited); next
    }

    U <- sort(U)
    enqueue_chunks <- function(k2) {
      parts <- .batch_partition(length(U), k2)
      j <- 0L
      for (chunk in parts) {
        j <- j + 1L
        queue[[length(queue) + 1L]] <<- list(
          rows  = U[chunk], depth = b$depth + 1L,
          bid   = paste0(b$bid, ".", j))
      }
    }

    if (identical(rowpack_recovery, "retry_same")) {
      if (b$depth + 1L >= max_depth) {
        fail_rows(U, .terminal_reason(res_b), res_b, b$bid,
                  tokens_available = !tokens_credited); next
      }
      queue[[length(queue) + 1L]] <- list(rows = U, depth = b$depth + 1L,
                                          bid = paste0(b$bid, ".r"))
    } else if (identical(rowpack_recovery, "singletons") || head_only || ctxerr) {
      enqueue_chunks(1L)
    } else if (identical(rowpack_recovery, "halve_once")) {
      k2 <- max(1L, as.integer(ceiling(m / 2)))
      if (k2 >= m) k2 <- 1L
      # halve_once: split once; children at depth that prevents further recursion
      parts <- .batch_partition(length(U), k2); j <- 0L
      for (chunk in parts) {
        j <- j + 1L
        queue[[length(queue) + 1L]] <- list(
          rows = U[chunk], depth = max_depth - 1L,  # children won't recurse again
          bid  = paste0(b$bid, ".", j))
      }
    } else {  # "halve_recursive" (default)
      k2 <- max(1L, as.integer(ceiling(m / 2)))
      if (k2 >= m) k2 <- 1L
      enqueue_chunks(k2)
    }
  }

  # rows never touched (e.g. budget) -> terminal NA
  for (g in which(status == "unresolved")) {
    finish[g] <- "error:rowpack_unresolved"
    status[g] <- "failed"
    meta[[g]] <- .empty_meta()
  }

  grouped <- any(!is.na(bsz_col) & bsz_col > 1L)
  .batch_assemble_res(answers, status, meta, finish,
                      bid_col, bsz_col, bri_col, grouped = grouped)
}

#' Assemble llm_mutate() diagnostic columns for a batched result
#'
#' Mirrors the non-batched `added` tibble exactly (same suffixes and order) so
#' the only difference a caller sees is the three extra batch columns, appended
#' when the engine actually grouped rows.
#'
#' @param .data Original data frame.
#' @param res The engine result tibble (`call_llm_par` columns + batch columns).
#' @param out_sym Quosure/symbol of the output column.
#' @param .before,.after Relocation targets.
#' @param before_missing,after_missing Whether the caller passed them.
#' @return `.data` with the diagnostic (and batch) columns bound and relocated.
#' @keywords internal
#' @noRd
.assemble_mutate_columns <- function(.data, res, out_sym, .before, .after,
                                     before_missing, after_missing) {
  nm <- rlang::as_name(out_sym)
  base_text <- ifelse(res$success, res$response_text, NA_character_)
  added <- tibble::tibble(
    !!nm                      := base_text,
    !!paste0(nm, "_finish")   := res$finish_reason,
    !!paste0(nm, "_sent")     := res$sent_tokens,
    !!paste0(nm, "_rec")      := res$rec_tokens,
    !!paste0(nm, "_tot")      := res$total_tokens,
    !!paste0(nm, "_reason")   := res$reasoning_tokens,
    !!paste0(nm, "_cached")   := if ("cached_tokens" %in% names(res)) res$cached_tokens else rep(NA_integer_, nrow(res)),
    !!paste0(nm, "_ok")       := res$success,
    !!paste0(nm, "_err")      := res$error_message,
    !!paste0(nm, "_id")       := res$response_id,
    !!paste0(nm, "_status")   := res$status_code,
    !!paste0(nm, "_ecode")    := res$error_code,
    !!paste0(nm, "_param")    := res$bad_param,
    !!paste0(nm, "_t")        := res$duration
  )
  if (all(c("rowpack_id", "rows_per_prompt", "rowpack_row") %in% names(res))) {
    added[[paste0(nm, "_rowpack")]] <- res$rowpack_id
    added[[paste0(nm, "_rpn")]]    <- res$rows_per_prompt
    added[[paste0(nm, "_rpi")]]    <- res$rowpack_row
  }
  res_df <- dplyr::bind_cols(
    .llm_drop_clobbered(.data, names(added), context = "llm_mutate()"),
    added
  )
  if (is.null(.before) && is.null(.after)) return(res_df)
  if (!is.null(.before)) {
    dplyr::relocate(res_df, dplyr::all_of(names(added)), .before = {{ .before }})
  } else {
    dplyr::relocate(res_df, dplyr::all_of(names(added)), .after = {{ .after }})
  }
}

#' Batched implementation shared by llm_fn_structured()/llm_mutate_structured()
#'
#' Sends the wrapper-object instruction `{"results":[{"row":i, ...}]}` so the
#' reply stays valid for OpenAI-compatible `response_format = json_object`,
#' de-multiplexes by the integer `row` field, and parses each row's JSON with
#' the unchanged [llm_parse_structured_col()]. Emits a one-time warning that the
#' result depends on the model honoring the protocol; provider-side strict
#' single-object schema validation is not applied to the batched array (only
#' local parsing/validation runs).
#'
#' @param x Vector or data frame (vector path used by llm_fn_structured).
#' @param data_df,output Data-frame path (used by llm_mutate_structured).
#' @keywords internal
#' @noRd
.llm_structured_batched <- function(x = NULL, prompt, .config, .system_prompt,
                                    .schema, .fields, .validate_local = TRUE,
                                    .rows_per_prompt, .rowpack_payload, .rowpack_recovery,
                                    dots,
                                    data_df = NULL, output = NULL,
                                    .before = NULL, .after = NULL) {
  .assert_batch_not_embedding(.config)
  warning("Structured output with .rows_per_prompt > 1 packs rows into one JSON ",
          "object {\"results\":[{\"row\":i, ...}]} and relies on the model ",
          "honoring that protocol; per-element results are parsed and ",
          "validated locally. Use .rows_per_prompt = 1 for strict provider-side ",
          "schema enforcement.", call. = FALSE, immediate. = FALSE)

  # ensure JSON-object mode so the wrapper parses cleanly (OpenAI-compatible);
  # do NOT enable the strict single-object schema (it would reject the array).
  cfg <- .config
  mp <- cfg$model_params %||% list()
  if (is.null(mp$response_format) && is.null(mp$json_schema)) {
    mp$response_format <- list(type = "json_object")
    cfg$model_params <- mp
  }

  if (!is.null(data_df)) {
    per_row <- as.character(glue::glue_data(data_df, prompt, .na = ""))
  } else {
    per_row <- if (is.data.frame(x)) as.character(glue::glue_data(x, prompt, .na = ""))
               else as.character(glue::glue_data(list(x = x), prompt, .na = ""))
  }

  res <- .run_batched(
    config = cfg, per_row_texts = per_row, system_text = .system_prompt,
    mode = "structured", rows_per_prompt = .rows_per_prompt,
    rowpack_payload = .rowpack_payload, rowpack_recovery = .rowpack_recovery, dots = dots)

  fields_auto <- if (is.null(.fields) && !is.null(.schema))
    .auto_fields_from_schema(.schema) else .fields

  if (!is.null(data_df)) {
    out_sym <- if (is.null(output)) rlang::sym("llm_structured") else output
    res_df <- .assemble_mutate_columns(
      data_df, res, out_sym, .before, .after,
      before_missing = is.null(.before), after_missing = is.null(.after))
    out2 <- llm_parse_structured_col(res_df,
      structured_col = rlang::as_name(out_sym), fields = fields_auto)
  } else {
    out2 <- llm_parse_structured_col(res,
      structured_col = "response_text", fields = fields_auto)
  }
  if (!is.null(.schema) && isTRUE(.validate_local)) {
    out2 <- llm_validate_structured_col(out2, schema = .schema,
                                        structured_list_col = "structured_data")
  }
  out2
}

#' Batched implementation of llm_mutate_tags()
#'
#' Evaluates per-row user text (from `prompt` or `.messages`), runs the engine
#' in tag mode, attaches the output column plus diagnostic and batch columns,
#' then parses the `<row_i>`-wrapped field tags into typed columns via the
#' unchanged [llm_parse_tags_col()].
#'
#' @keywords internal
#' @noRd
.llm_mutate_tags_batched <- function(.data, output, prompt, .messages, .config,
                                     .system_prompt, .before, .after,
                                     tags, .fields,
                                     .rows_per_prompt, .rowpack_payload,
                                     .rowpack_recovery, dots) {
  n <- nrow(.data)
  # determine the output column name
  out_name <- if (is.null(output)) "llm_tags" else rlang::as_name(output)

  # evaluate per-row user text + shared system prompt
  if (!is.null(.messages)) {
    stopifnot(is.character(.messages), length(.messages) > 0)
    if (is.null(names(.messages))) names(.messages) <- rep("user", length(.messages))
    eval_msgs <- lapply(seq_len(n), function(i) {
      tpl <- .messages
      roles <- names(tpl); roles[is.na(roles) | roles == ""] <- "user"
      vals <- vapply(unname(tpl), function(s)
        as.character(glue::glue_data(.data[i, , drop = FALSE], s, .na = "")),
        character(1))
      names(vals) <- roles; vals
    })
    .assert_batchable(eval_msgs)
    m1 <- eval_msgs[[1]]
    sys_shared <- if (!is.null(names(m1)) && "system" %in% names(m1))
      unname(m1[names(m1) == "system"][[1]]) else .system_prompt
    per_row <- vapply(eval_msgs, function(m)
      paste(unname(m[names(m) == "user"]), collapse = "\n\n"), character(1))
  } else {
    if (is.null(prompt)) stop("Either 'prompt' or '.messages' must be provided.")
    per_row <- as.character(glue::glue_data(.data, prompt, .na = ""))
    sys_shared <- .system_prompt
  }

  res <- .run_batched(
    config = .config, per_row_texts = per_row, system_text = sys_shared,
    mode = "tags", tags = tags, rows_per_prompt = .rows_per_prompt,
    rowpack_payload = .rowpack_payload, rowpack_recovery = .rowpack_recovery, dots = dots)

  out_sym <- rlang::sym(out_name)
  res_df <- .assemble_mutate_columns(
    .data, res, out_sym, .before, .after,
    before_missing = is.null(.before), after_missing = is.null(.after))

  # parse the wrapped field tags into columns (unchanged parser)
  llm_parse_tags_col(res_df, tags = tags, tags_col = out_name, fields = .fields)
}

#' Empty per-row diagnostic record (all NA)
#' @keywords internal
#' @noRd
.empty_meta <- function() {
  list(raw_response_json = NA_character_, error_message = NA_character_,
       sent_tokens = NA_integer_, rec_tokens = NA_integer_,
       total_tokens = NA_integer_, reasoning_tokens = NA_integer_,
       cached_tokens = NA_integer_,
       response_id = NA_character_, duration = NA_real_,
       status_code = NA_integer_, error_code = NA_character_,
       bad_param = NA_character_)
}

#' Terminal finish_reason for an unresolved batch row
#' @keywords internal
#' @noRd
.terminal_reason <- function(res_b) {
  if (is.null(res_b)) return("error:rowpack_call")
  if (!isTRUE(res_b$success[1])) {
    return(res_b$finish_reason[1] %||% "error:rowpack_call")
  }
  if (identical(res_b$finish_reason[1], "length")) return("error:rowpack_truncated")
  "error:rowpack_missing_row"
}

#' Assemble the per-row result tibble in original order
#'
#' Produces the EXACT [call_llm_par()] column set (so downstream `.return`
#' shaping and tag/structured parsing are untouched), plus the three batch
#' columns when `grouped` is `TRUE`.
#'
#' @keywords internal
#' @noRd
.batch_assemble_res <- function(answers, status, meta, finish,
                                bid_col, bsz_col, bri_col, grouped) {
  n <- length(answers)
  getm <- function(field, default) {
    vapply(seq_len(n), function(i) {
      v <- if (is.null(meta[[i]])) NULL else meta[[i]][[field]]
      if (is.null(v) || length(v) != 1L) default else v
    }, FUN.VALUE = default)
  }
  success <- status %in% c("ok", "recovered")

  res <- tibble::tibble(
    response_text     = answers,
    raw_response_json = getm("raw_response_json", NA_character_),
    success           = success,
    error_message     = getm("error_message", NA_character_),
    finish_reason     = finish,
    sent_tokens       = getm("sent_tokens", NA_integer_),
    rec_tokens        = getm("rec_tokens", NA_integer_),
    total_tokens      = getm("total_tokens", NA_integer_),
    reasoning_tokens  = getm("reasoning_tokens", NA_integer_),
    cached_tokens     = getm("cached_tokens", NA_integer_),
    response_id       = getm("response_id", NA_character_),
    duration          = getm("duration", NA_real_),
    status_code       = getm("status_code", NA_integer_),
    error_code        = getm("error_code", NA_character_),
    bad_param         = getm("bad_param", NA_character_),
    response          = replicate(n, NULL, simplify = FALSE)
  )
  if (isTRUE(grouped)) {
    res$rowpack_id   <- bid_col
    res$rows_per_prompt <- bsz_col
    res$rowpack_row  <- bri_col
  }
  res
}
