# preview.R =================================================================
# No-API dry run for the tidy generative path. Renders exactly what llm_fn() /
# llm_mutate() would send (via the shared renderer in render_messages.R) and
# flags glue / role / file / batching / schema problems BEFORE any paid call.
# Performs no network and no base64/file encoding: a "file" role is shown as a
# path string and only checked for existence.

#' Render tidy messages without calling any API
#'
#' Returns the per-row message objects that [llm_fn()] / [llm_mutate()] would
#' build from `prompt` or `.messages`, using the same internal renderer they
#' use. No request is sent and no file is read or encoded; a `"file"` role
#' stays a (glued) path string. Use this to inspect templating, roles, and
#' multimodal wiring before spending anything.
#'
#' @param .data A data.frame/tibble whose columns feed the `glue` templates.
#' @param prompt A single `glue` template string (mutually exclusive with
#'   `.messages`).
#' @param .messages A character vector of `glue` templates, optionally named by
#'   role (`"system"`, `"user"`, `"assistant"`, `"file"`). Unnamed entries
#'   default to `"user"`.
#' @param .system_prompt Optional system string, prepended when a row has no
#'   `"system"` message.
#' @param rows Optional integer vector selecting which rows to render (default:
#'   all rows).
#'
#' @return A list of length `length(rows)` (default `nrow(.data)`). Each element
#'   is either a bare character scalar (prompt only, no system) or a role-named
#'   character vector, identical to what the call path would dispatch.
#'
#' @seealso [llm_preview()] for a row-level summary with issue flags and the
#'   batch plan; [llm_fn()], [llm_mutate()].
#'
#' @examples
#' df <- data.frame(text = c("good", "bad"), stringsAsFactors = FALSE)
#' llm_render_messages(df, prompt = "Sentiment of: {text}")
#' llm_render_messages(
#'   df,
#'   .messages = c(system = "Be terse.", user = "Rate: {text}")
#' )
#' @export
llm_render_messages <- function(.data,
                                prompt = NULL,
                                .messages = NULL,
                                .system_prompt = NULL,
                                rows = NULL) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data.frame or tibble; got: ",
         paste(class(.data), collapse = "/"), call. = FALSE)
  }
  if (is.null(prompt) && is.null(.messages)) {
    stop("Provide either `prompt` or `.messages`.", call. = FALSE)
  }
  if (!is.null(prompt) && !is.null(.messages)) {
    warning("Both '.messages' and 'prompt' supplied; 'prompt' will be ignored.")
    prompt <- NULL
  }

  msgs <- .llm_build_messages_df(.data, prompt, .messages, .system_prompt)

  if (!is.null(rows)) {
    rows <- as.integer(rows)
    rows <- rows[!is.na(rows) & rows >= 1L & rows <= length(msgs)]
    msgs <- msgs[rows]
  }
  msgs
}

#' Preview a tidy LLM call without spending anything
#'
#' Renders every row exactly as [llm_fn()] / [llm_mutate()] would (no API call,
#' no file I/O), then reports a tidy, row-level summary: the rendered text, the
#' roles, character counts, file presence and existence, the batch plan, and a
#' list-column of `issues`. Problems that would only surface mid-run (a missing
#' file, a `"file"` role combined with `.rows_per_prompt > 1`, an embedding config
#' with row batching, `.return = "object"` with batching, a schema supplied
#' without `.structured`, a template that references `NA` values or renders an
#' empty prompt, a file part with no accompanying user text, or a tag name that
#' collides with the batched `<row_N>` protocol) are collected per row so you
#' see all of them at once rather than hitting the first error.
#'
#' Batched data travels inside numbered `<row_i>...</row_i>` tags; the
#' `rowpack_id` / `rows_per_prompt` / `rowpack_row` columns show how rows would be
#' grouped into calls at the given `.rows_per_prompt`.
#'
#' @inheritParams llm_render_messages
#' @param .config Optional [llm_config()]. When supplied, preview checks the
#'   embedding-vs-row-batching conflict; otherwise config-dependent checks are
#'   skipped.
#' @param .structured Logical; if `TRUE`, the call would request structured
#'   JSON output. Used only to validate `.schema`/`.tags` combinations.
#' @param .schema Optional JSON schema (list). Flagged if supplied without
#'   `.structured = TRUE`.
#' @param .tags Optional character vector of tag names. Flagged if combined with
#'   `.structured = TRUE` (structured takes precedence).
#' @param .return One of `"columns"`, `"text"`, `"object"`. Only used to flag
#'   the unsupported `"object"` + batching combination.
#' @param .rows_per_prompt Rows per call. `1` (default) means one call per row;
#'   `> 1` or `Inf` packs rows into batched calls.
#' @param max_chars Truncate each row's rendered preview to this many characters
#'   (default 500). Set higher to see full prompts.
#'
#' @return A tibble of class `llmr_preview`, one row per previewed input row,
#'   with columns: `row`, `ok` (no issues), `roles`, `rendered_preview`,
#'   `chars`, `has_file`, `file_ok`, `rowpack_id`, `rows_per_prompt`, `rowpack_row`, and
#'   `issues` (a list-column of character vectors).
#'
#' @seealso [llm_render_messages()], [llm_usage()], [llm_failures()].
#'
#' @examples
#' df <- data.frame(text = c("a", "b", "c"), stringsAsFactors = FALSE)
#' llm_preview(df, prompt = "Classify: {text}", .rows_per_prompt = 2)
#' @export
llm_preview <- function(.data,
                        prompt = NULL,
                        .messages = NULL,
                        .system_prompt = NULL,
                        .config = NULL,
                        .structured = FALSE,
                        .schema = NULL,
                        .tags = NULL,
                        .return = c("columns", "text", "object"),
                        .rows_per_prompt = 1L,
                        rows = NULL,
                        max_chars = 500L) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data.frame or tibble; got: ",
         paste(class(.data), collapse = "/"), call. = FALSE)
  }
  .return <- match.arg(.return)
  if (!is.null(.config) && !inherits(.config, "llm_config")) {
    stop("`.config` must be an `llm_config` object (or NULL).", call. = FALSE)
  }

  # Same activation rule as the real path (Inf-safe; see .validate_rows_per_prompt).
  batch_active <- .validate_rows_per_prompt(.rows_per_prompt)

  msgs <- .llm_build_messages_df(.data, prompt, .messages, .system_prompt)
  n <- length(msgs)

  # Deterministic batch plan: identical partition the engine would use.
  parts <- .batch_partition(n, .rows_per_prompt)
  rowpack_id  <- rep(NA_integer_, n)
  rowpack_row <- rep(NA_integer_, n)
  batch_n   <- rep(NA_integer_, n)
  for (j in seq_along(parts)) {
    idx <- parts[[j]]
    rowpack_id[idx]  <- j
    rowpack_row[idx] <- seq_along(idx)
    batch_n[idx]   <- length(idx)
  }

  is_embedding <- !is.null(.config) && .is_embedding_config(.config)

  # Rows whose templates reference NA values (rendered as "" on the real path).
  na_rows <- .llm_na_rows(.data, prompt, .messages)

  # Tag names that would collide with the <row_N> batching protocol.
  rowlike_tags <- if (!is.null(.tags)) {
    .tags[grepl("^row[_-]?[0-9]+$", .tags, ignore.case = TRUE)]
  } else character(0)

  show_rows <- if (is.null(rows)) seq_len(n) else {
    r <- as.integer(rows)
    r[!is.na(r) & r >= 1L & r <= n]
  }

  one <- function(i) {
    m  <- msgs[[i]]
    nm <- names(m)
    if (is.null(nm)) nm <- rep("user", length(m))

    file_vals <- unname(m[nm == "file"])
    has_file  <- length(file_vals) > 0L
    file_ok   <- if (!has_file) NA else all(file.exists(path.expand(file_vals)))

    issues <- character(0)
    if (has_file && isTRUE(batch_active)) {
      issues <- c(issues,
                  "file/multimodal rows are not supported with .rows_per_prompt > 1 (use .rows_per_prompt = 1)")
    }
    if (is_embedding && isTRUE(batch_active)) {
      issues <- c(issues,
                  ".rows_per_prompt controls generative row batching and does not apply to embeddings; use get_batched_embeddings(batch_size = )")
    }
    # Plain and structured batched paths reject .return = "object"; the tag path
    # (when .tags is supplied) supports it by returning parsed tag data per row.
    if (identical(.return, "object") && isTRUE(batch_active) && is.null(.tags)) {
      issues <- c(issues,
                  ".return = \"object\" is not supported with .rows_per_prompt > 1 (except in tag mode)")
    }
    if (has_file && !isTRUE(file_ok)) {
      issues <- c(issues, "one or more file paths do not exist on disk")
    }
    if (!is.null(.schema) && !isTRUE(.structured)) {
      issues <- c(issues, ".schema is ignored unless .structured = TRUE")
    }
    if (isTRUE(.structured) && !is.null(.tags)) {
      issues <- c(issues, ".structured takes precedence over .tags")
    }
    if (isTRUE(na_rows[i])) {
      issues <- c(issues,
                  "template references NA values (rendered as empty strings; see .na_action)")
    }
    user_chars <- sum(nchar(unname(m[nm == "user"])))
    if (!has_file && user_chars == 0L) {
      issues <- c(issues,
                  "rendered user prompt is empty; this row would still be sent and billed")
    }
    if (has_file && !any(nm == "user" & nzchar(unname(m)))) {
      issues <- c(issues,
                  "a file part needs at least one non-empty user text in the same message")
    }
    if (length(rowlike_tags) && isTRUE(batch_active)) {
      issues <- c(issues,
                  paste0("tag name(s) ", paste(rowlike_tags, collapse = ", "),
                         " collide with the <row_N> markers used by batched mode"))
    }

    collapsed <- paste0(nm, ": ", unname(m), collapse = "\n---\n")
    if (nchar(collapsed) > max_chars) {
      collapsed <- paste0(substr(collapsed, 1L, max_chars), "...")
    }

    tibble::tibble(
      row              = i,
      ok               = length(issues) == 0L,
      roles            = paste(nm, collapse = ","),
      rendered_preview = collapsed,
      chars            = sum(nchar(unname(m))),
      has_file         = has_file,
      file_ok          = file_ok,
      rowpack_id         = rowpack_id[i],
      rows_per_prompt       = batch_n[i],
      rowpack_row        = rowpack_row[i],
      issues           = list(issues)
    )
  }

  out <- if (length(show_rows)) {
    dplyr::bind_rows(lapply(show_rows, one))
  } else {
    tibble::tibble(
      row = integer(0), ok = logical(0), roles = character(0),
      rendered_preview = character(0), chars = integer(0),
      has_file = logical(0), file_ok = logical(0),
      rowpack_id = integer(0), rows_per_prompt = integer(0),
      rowpack_row = integer(0), issues = list()
    )
  }
  class(out) <- unique(c("llmr_preview", class(out)))
  out
}

#' @exportS3Method print llmr_preview
print.llmr_preview <- function(x, ...) {
  n_rows  <- nrow(x)
  n_bad   <- sum(!x$ok)
  n_calls <- length(unique(stats::na.omit(x$rowpack_id)))
  cat(sprintf("# llmr_preview: %d row%s, %d call%s, %d row%s with issues\n",
              n_rows, if (n_rows == 1L) "" else "s",
              n_calls, if (n_calls == 1L) "" else "s",
              n_bad, if (n_bad == 1L) "" else "s"))
  if (n_bad > 0L) {
    bad <- which(!x$ok)
    for (i in bad) {
      cat(sprintf("  row %d: %s\n", x$row[i],
                  paste(x$issues[[i]], collapse = "; ")))
    }
  }
  # Drop our class so the tibble prints with its normal formatting, regardless
  # of dispatch context (NextMethod() needs a live generic frame; this does not).
  y <- x
  class(y) <- setdiff(class(y), "llmr_preview")
  print(y, ...)
  invisible(x)
}
