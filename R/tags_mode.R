## tags_mode.R
## Soft structured output via XML-like tags
## --------------------------------------------------------------------------

.validate_tags <- function(tags) {
  tags <- as.character(tags)
  bad <- !length(tags) || anyNA(tags) || any(!nzchar(tags)) ||
    any(!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", tags))
  if (isTRUE(bad)) {
    stop("`.tags` must be a non-empty character vector of simple tag names.")
  }
  unique(tags)
}

# Batched mode wraps each row in <row_i>...</row_i>; a user tag that itself
# looks like a row marker would be read as a row boundary and scramble the
# demultiplexer, so reject it up front.
.assert_tags_not_rowlike <- function(tags) {
  bad <- grepl("^row[_-]?[0-9]+$", tags, ignore.case = TRUE)
  if (any(bad)) {
    stop("Tag name(s) ", paste(shQuote(tags[bad]), collapse = ", "),
         " collide with the <row_N> markers used by batched mode ",
         "(.rows_per_prompt > 1). Rename the tag or use .rows_per_prompt = 1.",
         call. = FALSE)
  }
  invisible(tags)
}

.escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
}

.tag_prompt <- function(tags) {
  tag_lines <- paste0("<", tags, ">...</", tags, ">")
  paste(
    c(
      "Return only XML-like tags with these tag names.",
      tag_lines,
      "Do not include prose, Markdown, or code fences outside the tags."
    ),
    collapse = "\n"
  )
}

.add_tag_prompt <- function(.messages, .system_prompt, tags) {
  instruction <- .tag_prompt(tags)

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

.decode_tag_entities <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

.coerce_tag_col <- function(vals, n) {
  nonnull <- vals[!vapply(vals, is.null, logical(1))]
  if (!length(nonnull)) return(rep(NA_character_, n))

  scalar <- vapply(nonnull, function(x) is.atomic(x) && length(x) == 1L, logical(1))
  if (!all(scalar)) return(vals)

  chr <- vapply(vals, function(x) {
    if (is.null(x)) NA_character_ else as.character(x)
  }, character(1))

  ok_num <- !is.na(chr) & nzchar(trimws(chr))
  if (any(ok_num) && all(!ok_num | !is.na(suppressWarnings(as.numeric(chr))))) {
    return(suppressWarnings(as.numeric(chr)))
  }

  low <- tolower(chr)
  ok_log <- !is.na(low) & low %in% c("true", "false")
  if (any(ok_log) && all(is.na(low) | ok_log)) {
    out <- rep(NA, n)
    out[ok_log] <- low[ok_log] == "true"
    return(out)
  }

  chr
}

#' Parse XML-like tags emitted by an LLM
#'
#' Extracts simple XML-like tags from a character scalar or [llmr_response], such
#' as `<age>21</age>` and `<job>student</job>`. This is intended for soft
#' structured output, not full XML validation.
#'
#' @param x Character scalar or [llmr_response].
#' @param tags Character vector of tag names to extract.
#' @return A named list of extracted tag values, or `NULL` when no requested tag
#'   is found.
#'
#' @examples
#' llm_parse_tags("<age>21</age><job>student</job>", tags = c("age", "job"))
#'
#' @seealso [llm_parse_tags_col()], [llm_mutate_tags()]
#' @export
llm_parse_tags <- function(x, tags) {
  tags <- .validate_tags(tags)
  if (inherits(x, "llmr_response")) x <- x$text %||% ""
  if (!is.character(x) || length(x) == 0) return(NULL)

  s <- as.character(x[[1]])
  if (is.na(s) || !nzchar(s)) return(NULL)

  s <- .strip_code_fences(s)
  out <- list()
  for (tag in tags) {
    tag_pat <- .escape_regex(tag)
    pat <- paste0("(?is)<\\s*", tag_pat, "(?:\\s+[^>]*)?>\\s*(.*?)\\s*</\\s*", tag_pat, "\\s*>")
    hit <- regmatches(s, regexec(pat, s, perl = TRUE))[[1]]
    if (length(hit) >= 2L) {
      out[[tag]] <- .decode_tag_entities(trimws(hit[[2]]))
    }
  }

  if (!length(out)) NULL else out
}

#' Parse XML-like tag fields from a column
#'
#' Appends `tags_ok`, `tags_data`, and one column per requested tag or field.
#'
#' @param .data data.frame/tibble.
#' @param tags Character vector of tag names to parse.
#' @param tags_col Column name to parse from. Default `"response_text"`.
#' @param fields `NULL` to extract all tags, a character vector of tags, a named
#'   vector such as `c(person_age = "age")`, or `FALSE` to skip field extraction.
#' @param prefix Optional prefix for extracted columns.
#' @return `.data` with tag diagnostics and extracted columns.
#'
#' @examples
#' df <- data.frame(response_text = "<age>21</age><job>student</job>")
#' llm_parse_tags_col(df, tags = c("age", "job"))
#' llm_parse_tags_col(df, tags = c("age", "job"), fields = c(person_age = "age"))
#'
#' @seealso [llm_parse_tags()], [llm_mutate_tags()], [llm_parse_structured_col()]
#' @export
llm_parse_tags_col <- function(.data, tags, tags_col = "response_text", fields = NULL, prefix = "") {
  tags <- .validate_tags(tags)
  if (!is.data.frame(.data)) {
    .data <- as.data.frame(.data, stringsAsFactors = FALSE)
  }
  n <- nrow(.data)
  out <- .data
  fields <- if (is.null(fields)) tags else fields

  # Snapshot the caller's columns: extracted tags must never silently
  # overwrite existing data.
  orig_names <- names(.data)
  .hoist_name <- function(dest) {
    nm <- paste0(prefix, dest)
    if (nm %in% orig_names) {
      nm2 <- make.unique(c(orig_names, nm), sep = "_")[length(orig_names) + 1L]
      warning(sprintf(
        "Column '%s' already exists; writing the extracted tag to '%s' instead. Use `prefix=` to control naming.",
        nm, nm2))
      nm2
    } else nm
  }

  if (!tags_col %in% names(.data)) {
    out$tags_ok <- rep(FALSE, n)
    out$tags_data <- replicate(n, NULL, simplify = FALSE)
    if (!identical(fields, FALSE) && length(fields)) {
      dest <- if (is.null(names(fields))) fields else names(fields)
      for (f in dest) out[[.hoist_name(f)]] <- rep(NA_character_, n)
    }
    return(tibble::as_tibble(out))
  }

  src <- .data[[tags_col]]
  parsed <- vector("list", n)
  ok <- logical(n)
  for (i in seq_len(n)) {
    p <- llm_parse_tags(src[[i]], tags = tags)
    parsed[i] <- list(p)
    ok[[i]] <- !is.null(p) && all(tags %in% names(p))
  }
  out$tags_ok <- ok
  out$tags_data <- parsed

  if (!identical(fields, FALSE) && length(fields)) {
    src_tags <- unname(if (is.null(names(fields))) fields else fields)
    dest_names <- if (is.null(names(fields))) fields else names(fields)
    for (k in seq_along(src_tags)) {
      vals <- lapply(parsed, function(x) {
        if (is.null(x)) NULL else x[[src_tags[[k]]]]
      })
      out[[.hoist_name(dest_names[[k]])]] <- .coerce_tag_col(vals, n)
    }
  }

  tibble::as_tibble(out)
}

#' Data-frame mutate with XML-like tag output
#'
#' Soft structured variant of [llm_mutate()]. It asks the model to return simple
#' XML-like tags, then parses those tags into columns.
#'
#' @inheritParams llm_mutate
#' @param .tags Character vector of tag names to request and parse.
#' @param .fields `NULL` to extract all tags, a character vector of tags, a named
#'   vector such as `c(person_age = "age")`, or `FALSE` to keep only `tags_data`.
#'
#' @details
#' Returns the mutated data frame plus:
#' \describe{
#'   \item{`tags_ok`}{`TRUE` when all requested tags were found.}
#'   \item{`tags_data`}{A list-column of parsed tag lists.}
#'   \item{tag columns}{One column per requested tag or field. Scalar columns are
#'   coerced to numeric or logical when all non-missing values allow it.}
#' }
#'
#' @section Shorthand syntax:
#' \preformatted{
#' df |> llm_mutate_tags(result = "{text}", .tags = c("age", "job"), .config = cfg)
#' }
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(city = c("Cairo", "Lima"))
#' cfg <- llm_config("openai", "gpt-4.1-nano", temperature = 0)
#'
#' df |>
#'   llm_mutate_tags(
#'     geo = "Where is {city}? Give country and continent in their own tags.",
#'     .config = cfg,
#'     .system_prompt = paste(
#'       "Use XML tags for different parts of the answer, but do not nest tags.",
#'       "Return <country>...</country> and <continent>...</continent>."
#'     ),
#'     .tags = c("country", "continent")
#'   )
#' }
#'
#' @seealso [llm_mutate()], [llm_parse_tags()], [llm_parse_tags_col()],
#'   [llm_mutate_structured()], [llm_parse_structured_col()]
#' @export
llm_mutate_tags <- function(.data,
                            output,
                            prompt = NULL,
                            .messages = NULL,
                            .config,
                            .system_prompt = NULL,
                            .before = NULL,
                            .after = NULL,
                            .tags,
                            .fields = NULL,
                            .rows_per_prompt = 1L,
                            .rowpack_payload = c("user", "system"),
                            .rowpack_recovery = c("halve_recursive", "halve_once",
                                                "singletons", "retry_same", "none"),
                            ...) {
  tags <- .validate_tags(.tags)
  output_missing <- missing(output)
  before_missing <- missing(.before)
  after_missing <- missing(.after)
  dots <- rlang::dots_list(...)
  .batched <- .validate_rows_per_prompt(.rows_per_prompt)
  .rowpack_payload  <- match.arg(.rowpack_payload)
  .rowpack_recovery <- match.arg(.rowpack_recovery)

  if (.batched) {
    .assert_batch_not_embedding(.config)
    .assert_tags_not_rowlike(tags)
    return(.llm_mutate_tags_batched(
      .data = .data,
      output = if (output_missing) NULL else rlang::ensym(output),
      prompt = prompt, .messages = .messages, .config = .config,
      .system_prompt = .system_prompt,
      .before = if (before_missing) NULL else .before,
      .after  = if (after_missing) NULL else .after,
      tags = tags, .fields = .fields,
      .rows_per_prompt = .rows_per_prompt, .rowpack_payload = .rowpack_payload,
      .rowpack_recovery = .rowpack_recovery, dots = dots))
  }

  prompted <- .add_tag_prompt(.messages, .system_prompt, tags)

  args <- list(
    .data = .data,
    prompt = prompt,
    .messages = prompted$.messages,
    .config = .config,
    .system_prompt = prompted$.system_prompt,
    .return = "columns"
  )
  if (!before_missing) args$.before <- .before
  if (!after_missing) args$.after <- .after

  if (output_missing) {
    out <- do.call(llm_mutate, c(args, dots))
    new_cols <- setdiff(names(out), names(.data))
    if (!length(new_cols)) {
      stop("Could not determine output column name from shorthand syntax")
    }
    output_name <- new_cols[[1]]
  } else {
    output_sym <- rlang::ensym(output)
    args$output <- output_sym
    out <- do.call(llm_mutate, c(args, dots))
    output_name <- rlang::as_name(output_sym)
  }

  llm_parse_tags_col(out, tags = tags, tags_col = output_name, fields = .fields)
}

#' Vectorized LLM with tag extraction
#'
#' Tags-first variant of [llm_fn()]. Injects tag instructions, calls the model
#' via [call_llm_broadcast()], then parses XML-like tags from each response.
#'
#' @inheritParams llm_fn
#' @param .tags Character vector of tag names to request and parse.
#' @param .fields `NULL` to extract all tags, a character vector of tags, a named
#'   vector such as `c(person_age = "age")`, or `FALSE` to skip field extraction.
#' @param .return One of `c("columns","text","object")`. `"columns"` returns a
#'   tibble with the parsed tag columns and diagnostics; `"text"` returns the
#'   raw response text. Unlike [llm_fn()], `"object"` here returns the parsed tag
#'   data (a list, one element per row), not `llmr_response` objects; this form
#'   is also supported together with `.rows_per_prompt > 1`.
#' @seealso [llm_fn()], [llm_mutate_tags()], [llm_parse_tags_col()],
#'   [call_llm_par_tags()]
#' @export
llm_fn_tags <- function(x,
                        prompt,
                        .config,
                        .system_prompt = NULL,
                        ...,
                        .tags,
                        .fields = NULL,
                        .return = c("columns", "text", "object"),
                        .rows_per_prompt = 1L,
                        .rowpack_payload = c("user", "system"),
                        .rowpack_recovery = c("halve_recursive", "halve_once",
                                            "singletons", "retry_same", "none")) {

  tags <- .validate_tags(.tags)
  .return <- match.arg(.return)
  .batched <- .validate_rows_per_prompt(.rows_per_prompt)
  .rowpack_payload  <- match.arg(.rowpack_payload)
  .rowpack_recovery <- match.arg(.rowpack_recovery)

  if (.batched) {
    .assert_batch_not_embedding(.config)
    .assert_tags_not_rowlike(tags)
    user_txt <- if (is.data.frame(x)) glue::glue_data(x, prompt, .na = "") else
      glue::glue_data(list(x = x), prompt, .na = "")
    res <- .run_batched(
      config = .config, per_row_texts = as.character(user_txt),
      system_text = .system_prompt, mode = "tags", tags = tags,
      rows_per_prompt = .rows_per_prompt, rowpack_payload = .rowpack_payload,
      rowpack_recovery = .rowpack_recovery, dots = rlang::dots_list(...))
    out2 <- llm_parse_tags_col(res, tags = tags, tags_col = "response_text",
                               fields = .fields)
    if (.return == "text")
      return(ifelse(out2$tags_ok, out2$response_text, NA_character_))
    if (.return == "object") return(out2$tags_data)
    return(out2)
  }

  prompted <- .add_tag_prompt(NULL, .system_prompt, tags)

  out <- llm_fn(x, prompt,
                .config = .config,
                .system_prompt = prompted$.system_prompt,
                ...,
                .return = "columns")

  out2 <- llm_parse_tags_col(out, tags = tags,
                             tags_col = "response_text",
                             fields = .fields)

  if (.return == "text") {
    return(ifelse(out2$tags_ok,
                  out2$response_text,
                  NA_character_))
  }
  if (.return == "object") return(out2$tags_data)
  out2
}

#' Parallel experiments with tag parsing
#'
#' Injects tag instructions into each experiment row, runs [call_llm_par()],
#' then parses XML-like tags from each response via [llm_parse_tags_col()].
#'
#' @param experiments Tibble with `config` and `messages` list-columns.
#' @param .tags Character vector of tag names to request and parse.
#' @param .fields `NULL` to extract all tags, a character vector of tags, a named
#'   vector such as `c(person_age = "age")`, or `FALSE` to skip field extraction.
#' @param ... Passed to [call_llm_par()].
#' @seealso [call_llm_par()], [llm_parse_tags_col()], [llm_fn_tags()],
#'   [llm_mutate_tags()]
#' @export
call_llm_par_tags <- function(experiments, .tags, .fields = NULL, ...) {
  stopifnot(is.data.frame(experiments),
            all(c("config", "messages") %in% names(experiments)))
  tags <- .validate_tags(.tags)

  ex <- experiments
  # Character messages are glue-templated against the experiment's metadata
  # columns; strings glue cannot parse (e.g. literal braces) pass through
  # verbatim instead of aborting the run.
  ex$messages <- lapply(seq_len(nrow(ex)), function(i) {
    msg <- ex$messages[[i]]
    rowdf <- ex[i, setdiff(names(ex), c("config", "messages")), drop = FALSE]
    if (is.character(msg)) {
      nm <- names(msg)
      out <- vapply(msg, function(s) {
        tryCatch(as.character(glue::glue_data(rowdf, s, .na = "")),
                 error = function(e) s)
      }, character(1), USE.NAMES = FALSE)
      if (!is.null(nm)) names(out) <- nm
      msg <- out
    }
    prompted <- .add_tag_prompt(msg, NULL, tags)
    prompted$.messages %||% msg
  })

  res <- call_llm_par(ex, ...)
  out <- llm_parse_tags_col(res, tags = tags,
                            tags_col = "response_text",
                            fields = .fields)

  class(out) <- unique(c("llmr_experiment", class(out)))
  out
}

