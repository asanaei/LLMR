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
    cached    = "cached_tokens",
    id        = "response_id",
    status    = "status_code",
    ecode     = "error_code",
    param     = "bad_param",
    duration  = "duration",
    batch     = "rowpack_id",
    batch_n   = "batch_size",
    batch_i   = "rowpack_row"
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
    cached    = paste0(prefix, "_cached"),
    id        = paste0(prefix, "_id"),
    status    = paste0(prefix, "_status"),
    ecode     = paste0(prefix, "_ecode"),
    param     = paste0(prefix, "_param"),
    duration  = paste0(prefix, "_t"),
    batch     = paste0(prefix, "_rowpack"),
    batch_n   = paste0(prefix, "_rpn"),
    batch_i   = paste0(prefix, "_rpi")
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
#' @param price_table Optional data frame you supply with your provider's
#'   current prices, holding columns `model`, `input`, and `output` (US dollars
#'   per **million** tokens), and optionally `cached` (price per million cached
#'   prompt tokens). When given, a `cost_estimate` column is added: cached
#'   tokens are billed at the `cached` rate (or the `input` rate if no `cached`
#'   column), the remaining sent tokens at `input`, and received tokens at
#'   `output`. LLMR ships no price list on purpose; prices change, and a stale
#'   bundled table would mislead silently.
#'
#' @return A one-row tibble: `n`, `n_ok`, `n_failed`, `ok_rate`, `n_truncated`
#'   (finish `"length"`), `n_filtered` (finish `"filter"`), `sent_tokens`,
#'   `rec_tokens`, `total_tokens`, `reasoning_tokens`, `cached_tokens`
#'   (prompt tokens served from the provider's cache, when reported),
#'   `n_unknown_tokens`
#'   (successful rows for which the provider reported no token usage, so the
#'   token sums above understate the truth), `duration_s`, (when a batch id
#'   column is present) `rowpack_calls` and `rows_per_rowpack`, and (when
#'   `price_table` is supplied) `cost_estimate` in the table's currency.
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
llm_usage <- function(x, prefix = NULL, price_table = NULL) {
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
  cached    <- .llm_get_col(x, m[["cached"]],    NA_integer_)
  duration  <- .llm_get_col(x, m[["duration"]],  NA_real_)
  batch     <- .llm_get_col(x, m[["batch"]],     NA)

  has_batch <- !all(is.na(batch))
  uniq_batches <- if (has_batch) length(unique(stats::na.omit(batch))) else NA_integer_
  batch_i <- .llm_get_col(x, m[["batch_i"]], NA_integer_)

  # Rows with genuinely unknown usage: succeeded but no total-token count, and
  # not a batch follow-on position (where NA tokens are by design, attributed to
  # the batch's first row). This disambiguates "0 tokens" from "tokens unknown",
  # since the token sums below use na.rm = TRUE (correct for batching).
  batch_follow_on <- !is.na(batch_i) & batch_i > 1L
  n_unknown_tokens <- sum((ok %in% TRUE) & is.na(total) & !batch_follow_on)

  out <- tibble::tibble(
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
    cached_tokens    = sum(cached,    na.rm = TRUE),
    n_unknown_tokens = n_unknown_tokens,
    duration_s       = sum(duration,  na.rm = TRUE),
    rowpack_calls         = if (has_batch) uniq_batches else NA_integer_,
    rows_per_rowpack = if (has_batch) n / uniq_batches else NA_real_
  )
  if (!is.null(price_table)) {
    out$cost_estimate <- .llm_cost_estimate(x, m, price_table)
  }
  out
}

# Internal: cost estimate from a user-supplied price table (per million tokens).
# Cached prompt tokens are billed at the `cached` rate when supplied, otherwise
# at the input rate (i.e., caching is ignored). Rows whose model has no price
# are excluded with a warning.
.llm_cost_estimate <- function(x, m, price_table) {
  if (!is.data.frame(price_table) ||
      !all(c("model", "input", "output") %in% names(price_table))) {
    stop("`price_table` must be a data frame with columns `model`, `input`, ",
         "`output` (and optionally `cached`), in currency per million tokens.",
         call. = FALSE)
  }
  sent   <- .llm_get_col(x, m[["sent"]],   NA_integer_)
  rec    <- .llm_get_col(x, m[["rec"]],    NA_integer_)
  cached <- .llm_get_col(x, m[["cached"]], NA_integer_)

  if ("model" %in% names(x)) {
    model <- as.character(x$model)
  } else if (nrow(price_table) == 1L) {
    model <- rep(price_table$model[[1]], nrow(x))
  } else {
    stop("`x` has no `model` column; supply a one-row `price_table` ",
         "(or use a call_llm_par() result, which carries `model`).", call. = FALSE)
  }

  idx <- match(model, price_table$model)
  if (anyNA(idx)) {
    warning("No price found for model(s): ",
            paste(unique(model[is.na(idx)]), collapse = ", "),
            ". Those rows are excluded from the estimate.", call. = FALSE)
  }
  p_in    <- price_table$input[idx]
  p_out   <- price_table$output[idx]
  p_cache <- if ("cached" %in% names(price_table)) price_table$cached[idx] else p_in

  cached0 <- ifelse(is.na(cached), 0, as.numeric(cached))
  sent0   <- ifelse(is.na(sent),   0, as.numeric(sent))
  rec0    <- ifelse(is.na(rec),    0, as.numeric(rec))
  fresh   <- pmax(sent0 - cached0, 0)

  cost <- fresh * p_in + cached0 * p_cache + rec0 * p_out
  sum(cost, na.rm = TRUE) / 1e6
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

#' Draft a methods-section paragraph from an LLM run
#'
#' Turns the diagnostic columns of a finished run into a first draft of the
#' transparency paragraph that journals and methodological guidelines now ask
#' for: which model(s) and provider(s), how many calls, the inference settings
#' that were recorded, token totals, and the failure/truncation counts. Edit
#' the draft; it states only what the result frame actually contains and marks
#' anything unknown as such.
#'
#' @inheritParams llm_usage
#' @param task Optional one-clause description of what the model was asked to
#'   do (e.g., `"to code open-ended survey responses into topics"`); it is
#'   spliced into the first sentence.
#' @return A character scalar (one paragraph). Print it with `cat()`.
#' @seealso [llm_usage()], [llm_log_enable()] for the per-call audit trail.
#' @examples
#' res <- tibble::tibble(
#'   model = "openai/gpt-oss-20b", provider = "groq",
#'   success = c(TRUE, TRUE), finish_reason = c("stop", "stop"),
#'   sent_tokens = c(10L, 12L), rec_tokens = c(5L, 7L),
#'   total_tokens = c(15L, 19L), reasoning_tokens = NA_integer_,
#'   duration = c(0.4, 0.5)
#' )
#' cat(llm_methods_text(res, task = "to classify sample sentences"))
#' @export
llm_methods_text <- function(x, prefix = NULL, task = NULL) {
  u <- llm_usage(x, prefix = prefix)

  models <- if ("model" %in% names(x)) {
    sort(unique(stats::na.omit(as.character(x$model))))
  } else character(0)
  providers <- if ("provider" %in% names(x)) {
    sort(unique(stats::na.omit(as.character(x$provider))))
  } else character(0)

  model_part <- if (length(models)) {
    paste0(if (length(models) > 1L) "the large language models " else "the large language model ",
           paste(sQuote(models, q = FALSE), collapse = ", "))
  } else {
    "a large language model (model identifier not recorded in this result frame)"
  }
  provider_part <- if (length(providers)) {
    paste0(" accessed through the ", paste(providers, collapse = ", "),
           if (length(providers) > 1L) " APIs" else " API")
  } else ""

  task_part <- if (!is.null(task) && nzchar(task)) paste0(" ", task) else ""

  settings <- character(0)
  for (p in c("temperature", "top_p", "max_tokens", "seed")) {
    if (p %in% names(x)) {
      vals <- unique(stats::na.omit(x[[p]]))
      if (length(vals) == 1L) settings <- c(settings, paste0(p, " = ", vals))
      else if (length(vals) > 1L) settings <- c(settings, paste0(p, " varied (", paste(utils::head(vals, 4L), collapse = ", "),
                                                                 if (length(vals) > 4L) ", ..." else "", ")"))
    }
  }
  settings_part <- if (length(settings)) {
    paste0(" Inference settings: ", paste(settings, collapse = "; "), ".")
  } else {
    " Inference settings beyond provider defaults were not recorded in this result frame."
  }

  fail_part <- if (u$n_failed > 0 || u$n_truncated > 0) {
    paste0(" ", u$n_failed, " call(s) failed and ", u$n_truncated,
           " response(s) were truncated; these are reported by llm_failures().")
  } else {
    " All calls completed without failures or truncation."
  }

  paste0(
    "Text was processed with ", model_part, provider_part, task_part,
    " via the LLMR package (version ",
    as.character(utils::packageVersion("LLMR")), ") for R, on ",
    format(Sys.Date(), "%Y-%m-%d"), ". ",
    "The run comprised ", u$n, " call(s), of which ", u$n_ok,
    " succeeded (", sprintf("%.1f%%", 100 * u$ok_rate), "). ",
    "Token usage as reported by the provider(s): ",
    format(u$sent_tokens, big.mark = ","), " sent and ",
    format(u$rec_tokens, big.mark = ","), " received",
    if (!is.na(u$cached_tokens) && u$cached_tokens > 0)
      paste0(" (of the sent tokens, ", format(u$cached_tokens, big.mark = ","),
             " were served from the provider's prompt cache)") else "",
    ".", settings_part, fail_part,
    " Proprietary models can change behind a fixed name; the response-level ",
    "model identifiers and request ids needed for exact attribution are ",
    "retained when logging is enabled via llm_log_enable()."
  )
}
