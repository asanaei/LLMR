# LLMR_tidy.R ---------------------------------------------------------------

#' @importFrom glue glue_data

# ---- shared helpers for the tidy layer -------------------------------------

# Which rows reference at least one NA value in their prompt template(s)?
# glue's `.na = NULL` propagates missingness, so re-rendering with it turns
# "any placeholder was NA" into an NA result without guessing column names.
# Templates glue cannot parse yield FALSE (no detection) rather than an error.
.llm_na_rows <- function(df, prompt = NULL, .messages = NULL) {
  n <- nrow(df)
  templates <- c(
    if (!is.null(prompt)) as.character(prompt),
    if (!is.null(.messages)) as.character(unname(.messages))
  )
  if (!length(templates)) return(rep(FALSE, n))
  out <- rep(FALSE, n)
  for (tpl in templates) {
    probe <- tryCatch(
      as.character(glue::glue_data(df, tpl, .na = NULL)),
      error = function(e) NULL
    )
    if (!is.null(probe) && length(probe) == n) out <- out | is.na(probe)
  }
  out
}

# Expand a result tibble computed on a subset of rows back to n rows, leaving
# all-NA rows (same column schema) where rows were skipped. Skipped rows are
# labeled in finish_reason so they are distinguishable from failures.
.llm_expand_skipped <- function(res, keep, n) {
  if (all(keep)) return(res)
  out <- res[rep(NA_integer_, n), , drop = FALSE]
  out[which(keep), ] <- res
  if ("finish_reason" %in% names(out)) {
    out$finish_reason[!keep] <- "skipped"
  }
  if ("success" %in% names(out)) out$success[!keep] <- NA
  out
}

# One-line outcome note after a run with failures or truncations, so problems
# are visible even when the user asked for plain text. Silenced by
# options(llmr.quiet = TRUE).
.llm_outcome_note <- function(res) {
  if (isTRUE(getOption("llmr.quiet"))) return(invisible(NULL))
  if (!is.data.frame(res) || !"success" %in% names(res)) return(invisible(NULL))
  skipped <- if ("finish_reason" %in% names(res)) res$finish_reason %in% "skipped" else rep(FALSE, nrow(res))
  n_fail  <- sum(!(res$success %in% TRUE) & !skipped)
  n_trunc <- if ("finish_reason" %in% names(res)) sum(res$finish_reason %in% "length") else 0L
  n_skip  <- sum(skipped)
  if (n_fail + n_trunc + n_skip == 0L) return(invisible(NULL))
  parts <- c(
    if (n_fail)  sprintf("%d call%s failed", n_fail, if (n_fail > 1L) "s" else ""),
    if (n_trunc) sprintf("%d response%s truncated (finish_reason = \"length\")",
                         n_trunc, if (n_trunc > 1L) "s" else ""),
    if (n_skip)  sprintf("%d row%s skipped (.na_action)", n_skip, if (n_skip > 1L) "s" else "")
  )
  message("LLMR: ", paste(parts, collapse = "; "),
          ". Inspect with llm_failures().")
  invisible(NULL)
}

# Overwrite-with-notice semantics for generated columns, mirroring
# dplyr::mutate(): an existing column of the same name is replaced (and a note
# is emitted) instead of letting bind_cols() mangle both names.
.llm_drop_clobbered <- function(.data, new_names, context = "llm_mutate()") {
  clobber <- intersect(new_names, names(.data))
  if (length(clobber)) {
    if (!isTRUE(getOption("llmr.quiet"))) {
      message(sprintf("%s is replacing existing column%s: %s",
                      context, if (length(clobber) > 1L) "s" else "",
                      paste(clobber, collapse = ", ")))
    }
    .data <- .data[, setdiff(names(.data), clobber), drop = FALSE]
  }
  .data
}


#' @title Apply an LLM prompt over vectors/data frames
#' @name llm_fn
#' @rdname llm_fn
#' @export
#' @param x A character vector *or* a data.frame/tibble.
#' @param prompt A glue template string. With a data-frame you may reference
#'   columns (`{col}`); with a vector the placeholder is `{x}`.
#' @param .config An [llm_config] object.
#' @param .system_prompt Optional system message (character scalar).
#' @param ... Passed unchanged to [call_llm_broadcast()] (e.g. `tries`,
#'   `progress`, `verbose`).
#' @param .tags Optional character vector of XML-like tag names to request and parse.
#'   When supplied, delegates to [llm_fn_tags()] for tag-based extraction.
#' @param .fields Optional field selector for tag extraction (see [llm_fn_tags()]).
#' @param .return One of \code{c("text","columns","object")}. `"columns"`
#'   returns a tibble of diagnostic columns; `"text"` returns a character
#'   vector; `"object"` returns a list of `llmr_response` (or `NA` on failure).
#' @param .na_action What to do with elements whose template references an `NA`
#'   value. `"send"` (default) renders `NA` as an empty string and sends the
#'   prompt anyway; `"skip"` does not call the API for those elements and
#'   returns `NA` for them (`finish_reason = "skipped"` in column mode);
#'   `"error"` stops before any call is made. `"skip"`/`"error"` are not
#'   available together with `.tags`.
#' @param .batch_size Integer scalar, or `Inf`. Number of input elements packed
#'   into a single generative request. The default, `1`, sends one request per
#'   element (the historical behaviour). When greater than `1`, elements are
#'   grouped and transmitted in one call wrapped in numbered
#'   `<row_1>...</row_1>` tags (see *Row batching* below); `Inf` sends all
#'   elements in a single call. Ignored for embedding configurations, which use
#'   [get_batched_embeddings()] instead.
#' @param .batch_payload One of `c("user","system")`. Channel to which the
#'   `<row_i>` data block is appended when batching. The imperative instruction
#'   is always placed in the system message; this argument controls only where
#'   the data goes. The default, `"user"`, keeps a static system prompt
#'   cacheable.
#' @param .batch_recovery How to handle rows that a batched call leaves
#'   unresolved (dropped, malformed, or truncated). One of:
#'   \describe{
#'     \item{`"halve_recursive"`}{(default) re-issue the unresolved rows at half
#'       the batch size, recursing down to single rows.}
#'     \item{`"halve_once"`}{re-issue at half the batch size exactly once, then
#'       give up on any still-unresolved rows.}
#'     \item{`"singletons"`}{re-issue each unresolved row on its own.}
#'     \item{`"retry_same"`}{re-issue the failed batch once at the same size.}
#'     \item{`"none"`}{do not recover; unresolved rows are returned as `NA`.}
#'   }
#'   Recovery is bounded by an internal call budget so it always terminates.
#'
#' @return For generative mode:
#' - `.return = "text"`: character vector
#' - `.return = "columns"`: tibble with diagnostics
#' - `.return = "object"`: list of `llmr_response` (or `NA` on failure;
#'   unavailable when `.batch_size > 1`)
#' For embedding mode, always a numeric matrix.
#'
#' @section Row batching:
#' With `.batch_size > 1`, several input elements travel in one generative
#' request: LLMR wraps each element's prompt in a numbered tag,
#' `<row_1>...</row_1>`, `<row_2>...</row_2>`, and so on, appends that block to
#' the message (see `.batch_payload`), and instructs the model to answer each
#' item inside a matching numbered tag. The reply is split back into the
#' original elements by those numbers. Batching trades a smaller number of
#' (larger) requests for some dependence on the model following the protocol; it
#' is most useful with capable models at `temperature = 0`, and it is a net loss
#' when the model ignores the wrapping. Results are deterministic given the
#' model's outputs: partitioning and parsing add no randomness. Rows the model
#' drops, reorders, duplicates, or truncates are detected and re-issued
#' according to `.batch_recovery`. Because a batch shares one underlying call,
#' token counts are reported once per batch (on its first resolved row, `NA`
#' elsewhere), as is the wall-clock duration, so that summing those columns is
#' correct. When a batch reply is entirely unusable and its rows succeed only
#' through recovery calls, the failed call's spend has no successful row to
#' land on, so sums can slightly undercount in heavy-recovery runs.
#'
#' @seealso [llm_mutate()], [llm_fn_structured()], [llm_fn_tags()],
#'   [llm_parse_batch_tags()], [setup_llm_parallel()], [call_llm_broadcast()],
#'   [get_batched_embeddings()]
#'
#' @examples
#' \dontrun{
#' words <- c("excellent", "awful")
#' cfg <- llm_config("openai", "gpt-4.1-nano", temperature = 0)
#' llm_fn(words, "Classify '{x}' as Positive/Negative.", cfg, .return = "text")
#'
#' df <- tibble::tibble(text = words, source = c("review", "review"))
#' llm_fn(df, "Classify '{text}' from {source}.", cfg, .return = "columns")
#' }
llm_fn <- function(x,
                   prompt,
                   .config,
                   .system_prompt = NULL,
                   ...,
                   .tags = NULL,
                   .fields = NULL,
                   .return = c("text","columns","object"),
                   .na_action = c("send", "skip", "error"),
                   .batch_size = 1L,
                   .batch_payload = c("user", "system"),
                   .batch_recovery = c("halve_recursive", "halve_once",
                                       "singletons", "retry_same", "none")) {

  stopifnot(inherits(.config, "llm_config"))
  .return <- match.arg(.return)
  .na_action <- match.arg(.na_action)
  .batched <- .validate_batch_size(.batch_size)
  .batch_payload  <- match.arg(.batch_payload)
  .batch_recovery <- match.arg(.batch_recovery)
  if (.batched) .assert_batch_not_embedding(.config)

  row_df0 <- if (is.data.frame(x)) x else data.frame(x = x, stringsAsFactors = FALSE)
  n_rows  <- nrow(row_df0)
  na_rows <- if (.na_action != "send") .llm_na_rows(row_df0, prompt = prompt) else rep(FALSE, n_rows)
  if (.na_action == "error" && any(na_rows)) {
    stop("Rows with NA in their prompt template: ",
         paste(utils::head(which(na_rows), 10L), collapse = ", "),
         if (sum(na_rows) > 10L) " ..." else "",
         ". Use .na_action = \"skip\" or clean the data first.", call. = FALSE)
  }
  keep <- !na_rows

  if (!is.null(.tags)) {
    if (.na_action != "send") {
      stop("`.na_action` values other than \"send\" are not supported together with `.tags`.",
           call. = FALSE)
    }
    return(llm_fn_tags(x, prompt,
                       .config = .config,
                       .system_prompt = .system_prompt,
                       ...,
                       .tags = .tags,
                       .fields = .fields,
                       .return = .return,
                       .batch_size = .batch_size,
                       .batch_payload = .batch_payload,
                       .batch_recovery = .batch_recovery))
  }

  user_txt <- as.character(glue::glue_data(row_df0, prompt, .na = ""))

  # Embeddings branch
  if (.is_embedding_config(.config)) {
    emb <- get_batched_embeddings(
      texts        = user_txt[keep],
      embed_config = .config,
      ...
    )
    if (all(keep)) return(emb)
    if (is.null(emb)) return(NULL)
    out <- matrix(NA_real_, nrow = n_rows, ncol = ncol(emb),
                  dimnames = list(NULL, colnames(emb)))
    out[which(keep), ] <- emb
    return(out)
  }

  if (.batched) {
    if (.return == "object")
      stop(".return = \"object\" is not supported with .batch_size > 1.",
           call. = FALSE)
    res <- .run_batched(
      config         = .config,
      per_row_texts  = user_txt[keep],
      system_text    = .system_prompt,
      mode           = "plain",
      batch_size     = .batch_size,
      batch_payload  = .batch_payload,
      batch_recovery = .batch_recovery,
      dots           = rlang::dots_list(...)
    )
    res <- .llm_expand_skipped(res, keep, n_rows)
    .llm_outcome_note(res)
    if (.return == "text")
      return(ifelse(res$success %in% TRUE, res$response_text, NA_character_))
    return(tibble::as_tibble(res[, intersect(c(
      "response_text","finish_reason",
      "sent_tokens","rec_tokens","total_tokens","reasoning_tokens","cached_tokens",
      "success","error_message","status_code","error_code","bad_param",
      "response_id","duration","raw_response_json",
      "batch_id","batch_size","batch_row"), names(res))]))
  }

  # Build messages through the shared renderer (R/render_messages.R) so llm_fn(),
  # llm_mutate(), and llm_preview() never diverge. For a bare-vector `x`, glue
  # against a one-column data.frame `x`; this is byte-identical to the previous
  # inline lapply() and is locked by golden tests (test-render-messages.R).
  msgs <- .llm_build_messages_df(row_df0[keep, , drop = FALSE], prompt = prompt,
                                 .system_prompt = .system_prompt)

  res <- call_llm_broadcast(
    config   = .config,
    messages = msgs,
    ...
  )
  res <- .llm_expand_skipped(res, keep, n_rows)
  .llm_outcome_note(res)

  if (.return == "text") {
    return(ifelse(res$success %in% TRUE, res$response_text, NA_character_))
  }

  if (.return == "object") {
    out <- vector("list", nrow(res))
    for (i in seq_len(nrow(res))) {
      out[[i]] <- if (isTRUE(res$success[i])) res$response[[i]] else NA
    }
    return(out)
  }

  tibble::as_tibble(res[, intersect(c(
    "response_text","finish_reason",
    "sent_tokens","rec_tokens","total_tokens","reasoning_tokens","cached_tokens",
    "success","error_message","status_code","error_code","bad_param",
    "response_id","duration","raw_response_json"
  ), names(res))])

}


#' Mutate a data frame with LLM output
#'
#' Adds one or more columns to `.data` that are produced by a Large-Language-Model.
#'
#' @param .data A data.frame / tibble.
#' @param output Unquoted name that becomes **the new column** (generative) *or*
#'   **the prefix** for embedding columns. In shorthand form, omit this argument
#'   and pass `newcol = "<glue prompt>"` or
#'   `newcol = c(system = "...", user = "...")` through `...`.
#' @param prompt Optional glue template string for a single user turn; reference
#'   any columns in `.data` (e.g. `"{id}. {question}\nContext: {context}"`).
#'   Ignored if `.messages` is supplied.
#' @param .messages Optional **named** character vector of glue templates to build
#'   a multi-turn message, using roles in `c("system","user","assistant","file")`.
#'   Values are glue templates evaluated per-row; all can reference multiple columns.
#'   For multimodal, use role `"file"` with a column containing a path template.
#' @param .config An [llm_config] object (generative or embedding).
#' @param .system_prompt Optional system message sent with every request when
#'   `.messages` does not include a `system` entry.
#' @param .before,.after Standard [dplyr::relocate] helpers controlling where the
#'   generated column(s) are placed.
#' @param .return One of \code{c("columns","text","object")}. For generative mode,
#'   controls how results are added. `"columns"` (default) adds text plus
#'   diagnostic columns; `"text"` adds a single text column; `"object"` adds a
#'   list-column of `llmr_response` objects named `<output>_obj`. Ignored (with
#'   a warning) when `.structured = TRUE` or `.tags` is supplied, which always
#'   return parsed columns.
#' @param .na_action What to do with rows whose template references an `NA`
#'   value. `"send"` (default) renders `NA` as an empty string and sends the
#'   prompt anyway; `"skip"` does not call the API for those rows (the output
#'   column is `NA` and `finish_reason` is `"skipped"`); `"error"` stops before
#'   any call is made. With `.structured` or `.tags`, only `"send"` and
#'   `"error"` are available.
#' @param .structured Logical. If `TRUE`, enables structured JSON output with automatic
#'   parsing. When enabled, this is equivalent to calling [llm_mutate_structured()].
#'   Default is `FALSE`.
#' @param .schema Optional JSON Schema (R list). When `.structured = TRUE`, this schema
#'   is sent to the provider for validation and used for local parsing. When `NULL`,
#'   only JSON mode is enabled (no strict schema validation).
#' @param .fields Optional character vector of fields to extract from parsed JSON
#'   or tag output. In JSON mode, supports nested paths (e.g., `"user.name"` or
#'   `"/data/items/0"`). When `NULL` and `.schema` is provided, auto-extracts all
#'   top-level schema properties. In tag mode, `NULL` extracts all `.tags`. Set
#'   to `FALSE` to skip field extraction entirely.
#' @param .tags Optional character vector of XML-like tag names to request and parse,
#'   such as `c("age", "job")`. When supplied, [llm_mutate()] delegates to
#'   [llm_mutate_tags()] and adds `tags_ok`, `tags_data`, and one column per tag
#'   unless `.fields = FALSE`.
#' @param .batch_size Integer scalar, or `Inf`. Number of rows packed into a
#'   single generative request. The default, `1`, sends one request per row (the
#'   historical behaviour). When greater than `1`, rows are grouped and sent in
#'   one call wrapped in numbered `<row_1>...</row_1>` tags (see *Row batching*
#'   below); `Inf` sends all rows at once. Works in generative, tag, and
#'   structured modes; not applicable to embedding configurations.
#' @param .batch_payload One of `c("user","system")`. Channel to which the
#'   `<row_i>` data block is appended when batching. The default `"user"` keeps a
#'   static system prompt cacheable; the imperative instruction is always placed
#'   in the system message.
#' @param .batch_recovery How to handle rows a batched call leaves unresolved.
#'   One of `"halve_recursive"` (default), `"halve_once"`, `"singletons"`,
#'   `"retry_same"`, or `"none"`; see [llm_fn()] for the precise meaning of each.
#' @param ... Passed to the underlying calls: [call_llm_broadcast()] in
#'   generative mode, [get_batched_embeddings()] in embedding mode.
#'
#' @details
#' - **Multi-column injection:** templating is NA-safe (`NA` -> empty string).
#' - **Multi-turn templating:** supply `.messages = c(system=..., user=..., file=...)`.
#'   Duplicate role names are allowed (e.g., two `user` turns).
#' - **Generative mode:** one request per row via [call_llm_broadcast()].
#' - **Parallelism:** calls [call_llm_broadcast()], which uses
#'   [call_llm_robust()] under the hood. If no future plan is active,
#'   workers are auto-configured; call [setup_llm_parallel()] to set worker
#'   count explicitly.
#' - **Embedding mode:** the per-row text is embedded via [get_batched_embeddings()].
#'   Result expands to numeric columns named `paste0(<output>, 1:N)`. If all rows
#'   fail to embed, a single `<output>1` column of `NA` is returned.
#' - Diagnostic columns use suffixes: `_finish`, `_sent`, `_rec`, `_tot`, `_reason`, `_ok`, `_err`, `_id`, `_status`, `_ecode`, `_param`, `_t`.
#' - **Row batching:** with `.batch_size > 1`, three further columns are added
#'   (`_batch`, `_bn`, `_bi`: the batch identifier, the size of the resolving
#'   call, and the within-call position). They appear only when batching
#'   actually groups rows, so the default schema is unchanged at `.batch_size = 1`.
#'
#' @return `.data` with the new column(s) appended.
#'
#' @section Row batching:
#' With `.batch_size > 1`, several rows travel in one generative request. LLMR
#' wraps each row's prompt in a numbered tag, `<row_1>...</row_1>`,
#' `<row_2>...</row_2>`, and so on, appends that block to the message (see
#' `.batch_payload`), and instructs the model to answer each item inside a
#' matching numbered tag; the reply is split back into rows by those numbers.
#' This also composes with `.tags` (each `<row_i>` then wraps the requested field
#' tags) and with `.structured = TRUE` (rows are returned as one JSON object
#' `{"results":[{"row":i, ...}]}`, de-multiplexed by the integer `row` field; a
#' one-time warning notes that this relies on the model honouring the protocol
#' and that strict provider-side schema validation is replaced by local parsing).
#' Batching is most useful with capable models at `temperature = 0` and is a net
#' loss when the model ignores the wrapping. Dropped, reordered, duplicated, or
#' truncated rows are detected and re-issued per `.batch_recovery`; token counts
#' are reported once per batch so that summing token columns stays correct.
#'
#' @section Shorthand:
#' You can supply the output column and prompt in one argument:
#'
#' \preformatted{
#' df |> llm_mutate(answer = "{question} (hint: {hint})", .config = cfg)
#' df |> llm_mutate(answer = c(system = "One word.", user = "{question}"), .config = cfg)
#' df |> llm_mutate(country = "Where is {city}? Answer with only the country.", .config = cfg)
#' }
#'
#' This is equivalent to:
#' \preformatted{
#' df |> llm_mutate(answer, prompt = "{question} (hint: {hint})", .config = cfg)
#' df |> llm_mutate(answer, .messages = c(system = "One word.", user = "{question}"), .config = cfg)
#' }
#'
#' @section Structured modes:
#' - `.structured = TRUE` delegates to [llm_mutate_structured()] for JSON.
#' - `.tags` delegates to [llm_mutate_tags()] for XML-like tags.
#' If both are supplied, `.structured` takes precedence.
#'
#' @seealso [llm_fn()], [llm_mutate_structured()], [llm_mutate_tags()],
#'   [llm_parse_structured_col()], [llm_parse_tags_col()],
#'   [llm_parse_batch_tags()], [call_llm_broadcast()], [setup_llm_parallel()]
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   id       = 1:2,
#'   question = c("Capital of France?", "Author of 1984?"),
#'   hint     = c("European city", "English novelist")
#' )
#'
#' cfg <- llm_config("openai", "gpt-4.1-nano",
#'                   temperature = 0)
#'
#' # Generative: single-turn with multi-column injection
#' df |>
#'   llm_mutate(
#'     answer,
#'     prompt = "{question} (hint: {hint})",
#'     .config = cfg,
#'     .system_prompt = "Respond in one word."
#'   )
#'
#' # Generative: multi-turn via .messages (system + user)
#' df |>
#'   llm_mutate(
#'     advice,
#'     .messages = c(
#'       system = "You are a helpful zoologist. Keep answers short.",
#'       user   = "What is a key fact about this? {question} (hint: {hint})"
#'     ),
#'     .config = cfg
#'   )
#'
#' # Multimodal: include an image path with role 'file'
#' pics <- tibble::tibble(
#'   img    = c("path/to/cat.png", "path/to/dog.jpg"),
#'   prompt = c("Describe the image.", "Describe the image.")
#' )
#' pics |>
#'   llm_mutate(
#'     vision_desc,
#'     .messages = c(user = "{prompt}", file = "{img}"),
#'     .config = llm_config("openai","gpt-4.1-mini")
#'   )
#'
#' # Embeddings: output name becomes the prefix of embedding columns
#' emb_cfg <- llm_config("voyage", "voyage-3.5-lite",
#'                       embedding = TRUE)
#' df |>
#'   llm_mutate(
#'     vec,
#'     prompt  = "{question}",
#'     .config = emb_cfg,
#'     .after  = id
#'   )
#'
#' # Structured output: using .structured = TRUE (equivalent to llm_mutate_structured)
#' schema <- list(
#'   type = "object",
#'   properties = list(
#'     answer = list(type = "string"),
#'     confidence = list(type = "number")
#'   ),
#'   required = list("answer", "confidence")
#' )
#'
#' df |>
#'   llm_mutate(
#'     result,
#'     prompt = "{question}",
#'     .config = cfg,
#'     .structured = TRUE,
#'     .schema = schema
#'   )
#'
#' # Structured with shorthand
#' df |>
#'   llm_mutate(
#'     result = "{question}",
#'     .config = cfg,
#'     .structured = TRUE,
#'     .schema = schema
#'   )
#'
#' # Soft structured output with XML-like tags
#' df |>
#'   llm_mutate(
#'     result = "Extract the person's age and job from: {question}",
#'     .config = cfg,
#'     .tags = c("age", "job")
#'   )
#'
#' cities <- tibble::tibble(city = c("Cairo", "Lima"))
#' cities |>
#'   llm_mutate(
#'     geo = "Where is {city}? Give country and continent in their own tags.",
#'     .config = cfg,
#'     .system_prompt = paste(
#'       "Use XML tags for different parts of the answer, but do not nest tags.",
#'       "Return <country>...</country> and <continent>...</continent>."
#'     ),
#'     .tags = c("country", "continent")
#'   )
#' }
#' @export
#' @importFrom glue glue_data
llm_mutate <- function(.data,
                       output,
                       prompt = NULL,
                       .messages = NULL,
                       .config,
                       .system_prompt = NULL,
                       .before = NULL,
                       .after  = NULL,
                       .return = c("columns","text","object"),
                       .na_action = c("send", "skip", "error"),
                       .structured = FALSE,
                       .schema = NULL,
                       .fields = NULL,
                       .tags = NULL,
                       .batch_size = 1L,
                       .batch_payload = c("user", "system"),
                       .batch_recovery = c("halve_recursive", "halve_once",
                                           "singletons", "retry_same", "none"),
                       ...) {

  stopifnot(inherits(.config, "llm_config"))
  roles_allowed <- c("system", "user", "assistant", "file")
  .na_action <- match.arg(.na_action)
  .ret_requested <- match.arg(.return)
  .batched <- .validate_batch_size(.batch_size)
  .batch_payload  <- match.arg(.batch_payload)
  .batch_recovery <- match.arg(.batch_recovery)
  if (.batched) .assert_batch_not_embedding(.config)

  out_missing <- missing(output)  # shorthand may supply it; final check happens later
  before_missing <- missing(.before)
  after_missing  <- missing(.after)

  dots <- rlang::dots_list(...)

  # Shorthand: llm_mutate(.data, newcol = "<prompt>") or newcol = c(system=..., user=...)
  if (out_missing && is.null(prompt) && is.null(.messages)) {
    nm <- names(dots)
    if (length(dots) && !is.null(nm)) {
      cand <- which(nzchar(nm) & vapply(dots, function(v) is.character(v) && length(v) >= 1L, logical(1)))
      if (length(cand) >= 1L) {
        j <- cand[1]
        output <- rlang::sym(nm[[j]])
        if (is.null(names(dots[[j]]))) {
          prompt <- as.character(dots[[j]])[1]
        } else {
          .messages <- dots[[j]]
          bad <- setdiff(unique(names(.messages)), roles_allowed)
          if (length(bad)) {
            stop("Unsupported roles in shorthand mapping: ", paste(bad, collapse = ", "))
          }
        }
        dots[[j]] <- NULL  # consume the mapping; remaining dots are pass-through
      }
    }
  }

  if (out_missing && missing(output)) {
    stop("llm_mutate() requires an output column name, e.g. llm_mutate(score, ...) or use the shorthand llm_mutate(score = '<prompt>', ...).")
  }

  if (!is.data.frame(.data)) stop("`.data` must be a data.frame or tibble; got: ", paste(class(.data), collapse = "/"))

  # NA policy: detect rows whose templates reference missing values before any
  # API call is made.
  na_rows <- if (.na_action != "send") .llm_na_rows(.data, prompt, .messages) else rep(FALSE, nrow(.data))
  if (.na_action == "error" && any(na_rows)) {
    stop("Rows with NA in their prompt template: ",
         paste(utils::head(which(na_rows), 10L), collapse = ", "),
         if (sum(na_rows) > 10L) " ..." else "",
         ". Use .na_action = \"skip\" or clean the data first.", call. = FALSE)
  }
  if (.na_action == "skip" && (isTRUE(.structured) || !is.null(.tags))) {
    stop("`.na_action = \"skip\"` is not supported together with `.structured` or `.tags`; ",
         "use \"error\" or filter the rows first.", call. = FALSE)
  }
  keep <- !na_rows

  # If .structured = TRUE, delegate to llm_mutate_structured
  if (isTRUE(.structured)) {
    if (!identical(.ret_requested, "columns")) {
      warning("`.return` is ignored when `.structured = TRUE`; parsed columns are returned.",
              call. = FALSE)
    }
    args <- list(
      .data = .data,
      prompt = prompt,
      .messages = .messages,
      .config = .config,
      .system_prompt = .system_prompt,
      .schema = .schema,
      .fields = .fields,
      .batch_size = .batch_size,
      .batch_payload = .batch_payload,
      .batch_recovery = .batch_recovery
    )
    if (!before_missing) args$.before <- .before
    if (!after_missing)  args$.after  <- .after

    if (out_missing) {
      # Shorthand set `output` to a symbol earlier
      args$output <- output
      return(do.call(llm_mutate_structured, c(args, dots)))
    } else {
      # User supplied `output`; capture as symbol
      args$output <- rlang::ensym(output)
      return(do.call(llm_mutate_structured, c(args, dots)))
    }
  }

  if (!is.null(.tags)) {
    if (!identical(.ret_requested, "columns")) {
      warning("`.return` is ignored when `.tags` is supplied; parsed columns are returned.",
              call. = FALSE)
    }
    args <- list(
      .data = .data,
      prompt = prompt,
      .messages = .messages,
      .config = .config,
      .system_prompt = .system_prompt,
      .tags = .tags,
      .fields = .fields,
      .batch_size = .batch_size,
      .batch_payload = .batch_payload,
      .batch_recovery = .batch_recovery
    )
    if (!before_missing) args$.before <- .before
    if (!after_missing)  args$.after  <- .after

    args$output <- if (out_missing) output else rlang::ensym(output)
    return(do.call(llm_mutate_tags, c(args, dots)))
  }

  # -- helpers (local) -------------------------------------------------------
  # The per-row rendering lives in .llm_build_messages_df()/.llm_eval_messages_one_row()
  # (R/render_messages.R) so that llm_fn(), llm_mutate(), and llm_preview() share
  # one source of truth and can never drift. These thin locals preserve the call
  # sites below (the embedding branch still calls eval_messages_one_row()).
  eval_messages_one_row <- .llm_eval_messages_one_row

  build_generative_messages <- function(df, prompt, .messages, .system_prompt) {
    .llm_build_messages_df(df, prompt, .messages, .system_prompt,
                           roles_allowed = roles_allowed)
  }

  pick_embed_text <- function(named_vec) {
    nm <- names(named_vec) %||% rep("user", length(named_vec))
    if (any(nm == "user")) {
      tail(named_vec[nm == "user"], 1)
    } else {
      paste(named_vec[nm != "file"], collapse = "\n\n")
    }
  }
  # -------------------------------------------------------------------------

  # ---- Embedding branch ----------------------------------------------------
  if (.is_embedding_config(.config)) {

    user_txt <- if (!is.null(.messages)) {
      stopifnot(is.character(.messages), length(.messages) > 0)
      if (is.null(names(.messages))) names(.messages) <- rep("user", length(.messages))
      bad <- setdiff(unique(names(.messages)), roles_allowed)
      if (length(bad)) {
        stop(sprintf("Unsupported roles in .messages: %s",
                     paste(bad, collapse = ", ")))
      }
      vapply(seq_len(nrow(.data)), function(i) {
        row_msg <- eval_messages_one_row(.data[i, , drop = FALSE], .messages)
        as.character(pick_embed_text(row_msg))
      }, FUN.VALUE = character(1))
    } else {
      if (is.null(prompt)) stop("For embeddings, provide 'prompt' or '.messages' that yields a user text.")
      as.character(glue::glue_data(.data, prompt, .na = ""))
    }

    emb_mat <- do.call(
      get_batched_embeddings,
      c(list(texts = user_txt[keep], embed_config = .config), dots)
    )
    if (!all(keep) && !is.null(emb_mat)) {
      full <- matrix(NA_real_, nrow = nrow(.data), ncol = ncol(emb_mat),
                     dimnames = list(NULL, colnames(emb_mat)))
      full[which(keep), ] <- emb_mat
      emb_mat <- full
    }

    # Determine prefix
    out_q  <- rlang::enquo(output)
    col_prefix <- rlang::as_name(out_q)

    # Robust fallback if all embeddings failed
    if (is.null(emb_mat)) {
      emb_mat <- matrix(NA_real_, nrow = nrow(.data), ncol = 1)
    }

    if (is.matrix(emb_mat) && ncol(emb_mat) > 0) {
      colnames(emb_mat) <- paste0(col_prefix, seq_len(ncol(emb_mat)))
    } else {
      # Ensure at least one column exists
      emb_mat <- matrix(NA_real_, nrow = nrow(.data), ncol = 1)
      colnames(emb_mat) <- paste0(col_prefix, 1L)
    }

    emb_df <- tibble::as_tibble(as.data.frame(emb_mat, stringsAsFactors = FALSE))

    res <- dplyr::bind_cols(
      .llm_drop_clobbered(.data, names(emb_df), context = "llm_mutate()"),
      emb_df
    )

    if (!is.null(.before) || !is.null(.after)) {
      res <- dplyr::relocate(
        res,
        dplyr::all_of(names(emb_df)),
        .before = {{ .before }},
        .after  = {{ .after }}
      )
    }
    return(res)
  }
  # -------------------------------------------------------------------------

  # ---- Generative branch ---------------------------------------------------
  if (!is.null(.messages) && !is.null(prompt)) {
    warning("Both '.messages' and 'prompt' supplied; 'prompt' will be ignored.")
  }

  msgs <- build_generative_messages(.data, prompt, .messages, .system_prompt)

  .ret <- .ret_requested
  out_sym <- rlang::enquo(output)
  n_rows <- nrow(.data)

  if (.batched) {
    if (.ret == "object")
      stop(".return = \"object\" is not supported with .batch_size > 1.",
           call. = FALSE)
    .assert_batchable(msgs)
    # extract one shared system prompt and one user text per row
    sys_shared <- {
      m1 <- msgs[[1]]
      if (!is.null(names(m1)) && "system" %in% names(m1))
        unname(m1[names(m1) == "system"][[1]]) else .system_prompt
    }
    per_row <- vapply(msgs, function(m) {
      if (is.null(names(m))) as.character(m)[1]
      else paste(unname(m[names(m) == "user"]), collapse = "\n\n")
    }, character(1))

    res <- .run_batched(
      config = .config, per_row_texts = per_row[keep], system_text = sys_shared,
      mode = "plain", batch_size = .batch_size, batch_payload = .batch_payload,
      batch_recovery = .batch_recovery, dots = dots)
    res <- .llm_expand_skipped(res, keep, n_rows)
    .llm_outcome_note(res)

    base_text <- ifelse(res$success %in% TRUE, res$response_text, NA_character_)
    if (.ret == "text") {
      return(.data |>
               dplyr::mutate(!!out_sym := base_text,
                             .before = {{ .before }}, .after = {{ .after }}))
    }
    return(.assemble_mutate_columns(.data, res, out_sym, .before, .after,
                                    before_missing, after_missing))
  }

  res <- do.call(
    call_llm_broadcast,
    c(list(config = .config, messages = msgs[keep]), dots)
  )
  res <- .llm_expand_skipped(res, keep, n_rows)
  .llm_outcome_note(res)

  base_text <- ifelse(res$success %in% TRUE, res$response_text, NA_character_)

  if (.ret == "text") {
    return(.data |>
             dplyr::mutate(!!out_sym := base_text,
                           .before = {{ .before }}, .after = {{ .after }}))
  }

  if (.ret == "object") {
    col_name <- paste0(rlang::as_name(out_sym), "_obj")
    objs <- vector("list", nrow(res))
    for (i in seq_len(nrow(res))) {
      objs[[i]] <- if (isTRUE(res$success[i])) res$response[[i]] else NA
    }
    return(.data |>
             dplyr::mutate(!!col_name := objs,
                           .before = {{ .before }}, .after = {{ .after }}))
  }


  added <- tibble::tibble(
    !!rlang::as_name(out_sym)              := base_text,
    !!paste0(rlang::as_name(out_sym), "_finish")  := res$finish_reason,
    !!paste0(rlang::as_name(out_sym), "_sent")    := res$sent_tokens,
    !!paste0(rlang::as_name(out_sym), "_rec")     := res$rec_tokens,
    !!paste0(rlang::as_name(out_sym), "_tot")     := res$total_tokens,
    !!paste0(rlang::as_name(out_sym), "_reason")  := res$reasoning_tokens,
    !!paste0(rlang::as_name(out_sym), "_cached")  := if ("cached_tokens" %in% names(res)) res$cached_tokens else rep(NA_integer_, nrow(res)),
    !!paste0(rlang::as_name(out_sym), "_ok")      := res$success,
    !!paste0(rlang::as_name(out_sym), "_err")     := res$error_message,
    !!paste0(rlang::as_name(out_sym), "_id")      := res$response_id,
    !!paste0(rlang::as_name(out_sym), "_status") := res$status_code,
    !!paste0(rlang::as_name(out_sym), "_ecode")  := res$error_code,
    !!paste0(rlang::as_name(out_sym), "_param")  := res$bad_param,
    !!paste0(rlang::as_name(out_sym), "_t")       := res$duration
  )

  # mutate semantics: an existing column with the output's name is replaced
  # (with a notice) instead of letting bind_cols() rename both copies.
  res_df <- dplyr::bind_cols(
    .llm_drop_clobbered(.data, names(added), context = "llm_mutate()"),
    added
  )

  res_df <- dplyr::relocate(
    res_df,
    dplyr::all_of(names(added)),
    .before = {{ .before }},
    .after  = {{ .after }}
  )

  return(res_df)
}
