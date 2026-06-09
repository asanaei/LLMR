# usage.R ===================================================================
# Post-run summaries over the diagnostic columns LLMR already emits. These are
# pure readers: they never call an API and never invent dollar figures. Token
# counts are reported as-is (sent / received / total / reasoning); cost in money
# is deliberately NOT computed here because provider prices change and a baked-in
# table would mislead. Multiply tokens by your own current per-token prices.

# Both result shapes are supported:
#   * call_llm_par() / call_llm_broadcast() / llm_fn(.return="columns"): direct
#     names (success, finish_reason, sent_tokens, ...).
#   * llm_mutate(out, ...): prefixed names (out_ok, out_finish, out_sent, ...).

#' Map logical fields to the actual diagnostic column names present
#'
#' Returns a named character vector keyed by canonical field
#' (`ok`, `finish`, `sent`, ...) whose values are the column names to read, or
#' `NA` where that column is absent. Detects the call_llm_par direct schema
#' first; otherwise infers the `llm_mutate` prefix (or uses the supplied one).
#'
#' @keywords internal
#' @noRd
.llm_diag_map <- function(x, prefix = NULL) {
  nm <- names(x)

  direct <- c(
    text      = "response_text",
    ok        = "success",
    err       = "error_message",
    finish    = "finish_reason",
    sent      = "sent_tokens",
    rec       = "rec_tokens",
    total     = "total_tokens",
    reasoning = "reasoning_tokens",
    id        = "response_id",
    status    = "status_code",
    ecode     = "error_code",
    param     = "bad_param",
    duration  = "duration",
    batch     = "batch_id",
    batch_n   = "batch_size",
    batch_i   = "batch_row"
  )

  # Collision-renamed call_llm_par() result: when the input frame already had a
  # column named e.g. "success", call_llm_par() writes its OWN output to
  # "success.1" (next free suffix), leaving the user's column as "success". The
  # presence of BOTH "success.N" and "finish_reason.N" is the unambiguous signal
  # that this renaming happened (call_llm_par creates those suffixes only on
  # collision). When it has, the suffixed columns are ours, so map each
  # diagnostic field to its highest-suffix variant where one exists.
  collided <- is.null(prefix) &&
    any(grepl("^success\\.[0-9]+$", nm)) &&
    any(grepl("^finish_reason\\.[0-9]+$", nm))
  if (collided) {
    pick <- function(base) {
      hits <- grep(paste0("^", base, "\\.[0-9]+$"), nm, value = TRUE)
      if (length(hits)) {
        hits[which.max(as.integer(sub(paste0("^", base, "\\."), "", hits)))]
      } else if (base %in% nm) {
        base
      } else {
        NA_character_
      }
    }
    direct2 <- vapply(unname(direct), pick, character(1))
    names(direct2) <- names(direct)
    return(direct2)
  }

  use_direct <- is.null(prefix) && all(c("success", "finish_reason") %in% nm)
  if (use_direct) {
    direct[!(direct %in% nm)] <- NA_character_
    return(direct)
  }

  if (is.null(prefix)) {
    # Infer from a unique "<prefix>_ok" column.
    ok_cols <- nm[grepl("_ok$", nm)]
    cand <- sub("_ok$", "", ok_cols)
    if (length(cand) == 1L) {
      prefix <- cand[[1]]
    } else if (length(cand) == 0L) {
      stop("Could not find diagnostic columns. Supply `prefix =` (the ",
           "llm_mutate() output column name), or pass a call_llm_par() result.",
           call. = FALSE)
    } else {
      stop("Found multiple diagnostic blocks (", paste(cand, collapse = ", "),
           "). Supply `prefix =` to choose one.", call. = FALSE)
    }
  }

  pref <- c(
    text      = prefix,
    ok        = paste0(prefix, "_ok"),
    err       = paste0(prefix, "_err"),
    finish    = paste0(prefix, "_finish"),
    sent      = paste0(prefix, "_sent"),
    rec       = paste0(prefix, "_rec"),
    total     = paste0(prefix, "_tot"),
    reasoning = paste0(prefix, "_reason"),
    id        = paste0(prefix, "_id"),
    status    = paste0(prefix, "_status"),
    ecode     = paste0(prefix, "_ecode"),
    param     = paste0(prefix, "_param"),
    duration  = paste0(prefix, "_t"),
    batch     = paste0(prefix, "_batch"),
    batch_n   = paste0(prefix, "_bn"),
    batch_i   = paste0(prefix, "_bi")
  )
  pref[!(pref %in% nm)] <- NA_character_
  pref
}

#' Read a mapped column, or a default-filled vector when absent
#' @keywords internal
#' @noRd
.llm_get_col <- function(x, colname, default) {
  if (is.na(colname) || !colname %in% names(x)) {
    return(rep(default, nrow(x)))
  }
  x[[colname]]
}

#' Summarize token usage and outcomes of an LLM run
#'
#' Reads the diagnostic columns produced by [call_llm_par()] (and
#' [call_llm_broadcast()] / [llm_fn()] with `.return = "columns"`) or by
#' [llm_mutate()], and returns a one-row tibble of counts and token totals. It
#' reports **tokens, not money**: sent, received, total, and reasoning tokens
#' are summed with `na.rm = TRUE` (correct under row batching, which attributes a
#' batch's tokens to its first row and leaves the rest `NA`). To estimate cost,
#' multiply these by your provider's current per-token prices yourself.
#'
#' @param x A data frame from [call_llm_par()] or [llm_mutate()].
#' @param prefix For an [llm_mutate()] result, the output column name whose
#'   diagnostics to summarize (e.g. `"answer"`). Inferred automatically when a
#'   single diagnostic block is present; required when several are.
#'
#' @return A one-row tibble: `n`, `n_ok`, `n_failed`, `ok_rate`, `n_truncated`
#'   (finish `"length"`), `n_filtered` (finish `"filter"`), `sent_tokens`,
#'   `rec_tokens`, `total_tokens`, `reasoning_tokens`, `duration_s`, and (when a
#'   batch id column is present) `batch_calls` and `rows_per_batch_call`.
#'
#' @seealso [llm_failures()], [llm_preview()], [llm_par_resume()].
#'
#' @examples
#' res <- tibble::tibble(
#'   success = c(TRUE, TRUE, FALSE),
#'   finish_reason = c("stop", "length", "error:rate_limit"),
#'   sent_tokens = c(10L, 12L, NA_integer_),
#'   rec_tokens = c(5L, 7L, NA_integer_),
#'   total_tokens = c(15L, 19L, NA_integer_),
#'   reasoning_tokens = c(NA_integer_, NA_integer_, NA_integer_),
#'   duration = c(0.4, 0.5, 0.1)
#' )
#' llm_usage(res)
#' @export
llm_usage <- function(x, prefix = NULL) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame (a call_llm_par() or llm_mutate() result).",
         call. = FALSE)
  }
  m <- .llm_diag_map(x, prefix)
  n <- nrow(x)

  ok        <- .llm_get_col(x, m[["ok"]],        NA)
  finish    <- .llm_get_col(x, m[["finish"]],    NA_character_)
  sent      <- .llm_get_col(x, m[["sent"]],      NA_integer_)
  rec       <- .llm_get_col(x, m[["rec"]],       NA_integer_)
  total     <- .llm_get_col(x, m[["total"]],     NA_integer_)
  reasoning <- .llm_get_col(x, m[["reasoning"]], NA_integer_)
  duration  <- .llm_get_col(x, m[["duration"]],  NA_real_)
  batch     <- .llm_get_col(x, m[["batch"]],     NA)

  has_batch <- !all(is.na(batch))
  uniq_batches <- if (has_batch) length(unique(stats::na.omit(batch))) else NA_integer_

  tibble::tibble(
    n                = n,
    n_ok             = sum(ok %in% TRUE),
    n_failed         = sum(!(ok %in% TRUE)),
    ok_rate          = if (n) mean(ok %in% TRUE) else NA_real_,
    n_truncated      = sum(finish %in% "length"),
    n_filtered       = sum(finish %in% "filter"),
    sent_tokens      = sum(sent,      na.rm = TRUE),
    rec_tokens       = sum(rec,       na.rm = TRUE),
    total_tokens     = sum(total,     na.rm = TRUE),
    reasoning_tokens = sum(reasoning, na.rm = TRUE),
    duration_s       = sum(duration,  na.rm = TRUE),
    batch_calls         = if (has_batch) uniq_batches else NA_integer_,
    rows_per_batch_call = if (has_batch) n / uniq_batches else NA_real_
  )
}

#' List the rows of an LLM run that failed or were truncated
#'
#' Returns one row per problem: a hard failure (`success` not `TRUE`) or a
#' truncated / content-filtered completion (`finish_reason` `"length"` or
#' `"filter"`), with the diagnostic detail needed to act. Works on both
#' [call_llm_par()] and [llm_mutate()] results. For a [call_llm_par()] result
#' (which still carries `config` and `messages`), pass the original frame to
#' [llm_par_resume()] to re-run only these rows.
#'
#' @inheritParams llm_usage
#' @param include One of `"failed"` (hard failures only), `"truncated"`
#'   (length/filter only), or `"all"` (default; both).
#'
#' @return A tibble (zero rows if nothing matched) with: `row` (index into `x`),
#'   `success`, `finish_reason`, `status_code`, `error_code`, `bad_param`,
#'   `error_message`, `response_id`. Columns absent from `x` are filled with
#'   `NA`.
#'
#' @seealso [llm_par_resume()] to re-run failed rows, [llm_usage()].
#'
#' @examples
#' res <- tibble::tibble(
#'   success = c(TRUE, FALSE, TRUE),
#'   finish_reason = c("stop", "error:server", "length"),
#'   error_message = c(NA, "HTTP 503", NA)
#' )
#' llm_failures(res)
#' @export
llm_failures <- function(x, prefix = NULL, include = c("all", "failed", "truncated")) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame (a call_llm_par() or llm_mutate() result).",
         call. = FALSE)
  }
  include <- match.arg(include)
  m <- .llm_diag_map(x, prefix)

  ok     <- .llm_get_col(x, m[["ok"]],     NA)
  finish <- .llm_get_col(x, m[["finish"]], NA_character_)

  is_failed    <- !(ok %in% TRUE)
  is_truncated <- finish %in% c("length", "filter")

  bad <- switch(include,
                failed    = which(is_failed),
                truncated = which(is_truncated),
                all       = which(is_failed | is_truncated))

  empty <- tibble::tibble(
    row = integer(0), success = logical(0), finish_reason = character(0),
    status_code = integer(0), error_code = character(0),
    bad_param = character(0), error_message = character(0),
    response_id = character(0)
  )
  if (!length(bad)) return(empty)

  tibble::tibble(
    row           = bad,
    success       = ok[bad],
    finish_reason = finish[bad],
    status_code   = .llm_get_col(x, m[["status"]], NA_integer_)[bad],
    error_code    = .llm_get_col(x, m[["ecode"]],  NA_character_)[bad],
    bad_param     = .llm_get_col(x, m[["param"]],   NA_character_)[bad],
    error_message = .llm_get_col(x, m[["err"]],     NA_character_)[bad],
    response_id   = .llm_get_col(x, m[["id"]],      NA_character_)[bad]
  )
}
