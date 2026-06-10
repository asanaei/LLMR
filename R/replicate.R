# replicate.R -----------------------------------------------------------------
# Replication and inter-replicate agreement for LLM annotation.
#
# A single model run is one draw from a stochastic process. Methodological
# guidance for LLM-assisted research asks for repeated runs and an agreement
# statistic, exactly as one would report intercoder reliability for human
# coders. llm_replicate() collects the draws; llm_agreement() computes the
# reliability.

#' Run the same prompt several times per row
#'
#' Calls the model `.times` times for every row of `.data` (all replicates run
#' through the parallel engine in one pass) and appends one column per
#' replicate: `<output>_1`, `<output>_2`, .... Feed the result to
#' [llm_agreement()] for per-row majority labels and overall reliability.
#'
#' Replication only measures sampling variability if the model can vary: with
#' `temperature = 0` (or a fixed `seed`) most providers return nearly identical
#' draws, which inflates agreement. Conversely, for measurement purposes you
#' may want exactly that check: high disagreement at low temperature signals a
#' prompt the model finds genuinely ambiguous.
#'
#' @param .data A data.frame / tibble.
#' @param output Unquoted base name for the replicate columns.
#' @param prompt A glue template string evaluated against the columns of
#'   `.data` (as in [llm_mutate()]).
#' @param .config An [llm_config] object (generative).
#' @param .times Number of replicates (default 3).
#' @param .system_prompt Optional system message.
#' @param ... Passed to [call_llm_par()] (e.g. `tries`, `progress`).
#' @return `.data` with `.times` new character columns,
#'   `<output>_1 ... <output>_<.times>` (`NA` where a call failed).
#' @examples
#' \dontrun{
#' cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 1)
#' df <- tibble::tibble(text = c("I loved it", "Meh", "Terrible service"))
#' reps <- df |>
#'   llm_replicate(sentiment,
#'                 prompt = "Sentiment of '{text}'. One word: positive, negative, or neutral.",
#'                 .config = cfg, .times = 5)
#' llm_agreement(reps, prefix = "sentiment")
#' }
#' @seealso [llm_agreement()], [llm_mutate()], [call_llm_par()]
#' @export
llm_replicate <- function(.data, output, prompt, .config,
                          .times = 3L, .system_prompt = NULL, ...) {
  stopifnot(is.data.frame(.data), inherits(.config, "llm_config"))
  if (.is_embedding_config(.config)) {
    stop("llm_replicate() is for generative configs.", call. = FALSE)
  }
  .times <- as.integer(.times)
  stopifnot(length(.times) == 1L, !is.na(.times), .times >= 2L)
  out_name <- rlang::as_name(rlang::ensym(output))
  n <- nrow(.data)
  if (n == 0L) return(.data)

  rendered <- .llm_build_messages_df(.data, prompt = prompt,
                                     .system_prompt = .system_prompt)

  experiments <- tibble::tibble(
    .rep     = rep(seq_len(.times), each = n),
    .row     = rep(seq_len(n), times = .times),
    config   = rep(list(.config), n * .times),
    messages = rep(rendered, times = .times)
  )

  res <- call_llm_par(experiments, ...)
  txt <- ifelse(res$success %in% TRUE, res$response_text, NA_character_)

  out <- .data
  for (r in seq_len(.times)) {
    sel <- which(res$.rep == r)
    sel <- sel[order(res$.row[sel])]
    out[[paste0(out_name, "_", r)]] <- txt[sel]
  }
  out
}

#' Agreement across replicated LLM annotations
#'
#' Computes per-row majority labels and overall reliability for replicate
#' columns produced by [llm_replicate()] (or any set of columns holding
#' repeated codings of the same units, including codings by different models
#' or by humans). Reliability is reported as average pairwise percent
#' agreement and Krippendorff's alpha for nominal data, the statistic
#' reviewers most often ask for; alpha handles missing values (failed calls)
#' gracefully.
#'
#' @param .data A data frame holding the replicate columns.
#' @param cols Character vector naming the replicate columns. Alternatively
#'   supply `prefix`.
#' @param prefix Base name: columns matching `<prefix>_1`, `<prefix>_2`, ...
#'   are used.
#' @param normalize If `TRUE` (default), values are compared after trimming
#'   whitespace and lowercasing, so "Positive" and " positive" agree. Set to
#'   `FALSE` for exact string comparison.
#' @return An object of class `llmr_agreement`: a list with
#'   \describe{
#'     \item{`by_row`}{a tibble with one row per unit: `majority` (modal
#'       label, `NA` on ties), `share` (modal share of non-missing
#'       replicates), `n_distinct`, `unanimous`, `tie`, `n_missing`.}
#'     \item{`summary`}{a one-row tibble: `n_units`, `n_replicates`,
#'       `mean_pairwise_agreement`, `krippendorff_alpha`, `n_unanimous`,
#'       `n_ties`.}
#'   }
#'   Printing shows the summary.
#' @references Krippendorff, K. (2019). Content Analysis: An Introduction to
#'   Its Methodology (4th ed.), chapter 12. The alpha implemented here is the
#'   nominal-data form with missing values allowed.
#' @seealso [llm_replicate()]
#' @export
llm_agreement <- function(.data, cols = NULL, prefix = NULL, normalize = TRUE) {
  stopifnot(is.data.frame(.data))
  if (is.null(cols)) {
    if (is.null(prefix)) stop("Supply `cols` or `prefix`.", call. = FALSE)
    cols <- grep(paste0("^", prefix, "_[0-9]+$"), names(.data), value = TRUE)
  }
  if (length(cols) < 2L) {
    stop("Need at least two replicate columns; found: ",
         paste(cols, collapse = ", "), call. = FALSE)
  }
  miss <- setdiff(cols, names(.data))
  if (length(miss)) stop("Columns not found: ", paste(miss, collapse = ", "), call. = FALSE)

  m <- as.matrix(.data[, cols, drop = FALSE])
  storage.mode(m) <- "character"
  if (isTRUE(normalize)) {
    m[] <- tolower(trimws(m))
    m[m == ""] <- NA_character_
  }

  n_units <- nrow(m)
  k <- ncol(m)

  by_row <- lapply(seq_len(n_units), function(u) {
    v <- m[u, ]
    v <- v[!is.na(v)]
    if (!length(v)) {
      return(tibble::tibble(majority = NA_character_, share = NA_real_,
                            n_distinct = 0L, unanimous = NA, tie = NA,
                            n_missing = k))
    }
    tab <- sort(table(v), decreasing = TRUE)
    tie <- length(tab) > 1L && tab[[1]] == tab[[2]]
    tibble::tibble(
      majority   = if (tie) NA_character_ else names(tab)[[1]],
      share      = as.numeric(tab[[1]]) / length(v),
      n_distinct = length(tab),
      unanimous  = length(tab) == 1L && length(v) >= 2L,
      tie        = tie,
      n_missing  = k - length(v)
    )
  })
  by_row <- dplyr::bind_rows(by_row)

  # mean pairwise agreement over units with at least two non-missing codes
  pair_agree <- vapply(seq_len(n_units), function(u) {
    v <- m[u, ]; v <- v[!is.na(v)]
    nu <- length(v)
    if (nu < 2L) return(NA_real_)
    tab <- table(v)
    sum(tab * (tab - 1)) / (nu * (nu - 1))
  }, numeric(1))

  summary <- tibble::tibble(
    n_units = n_units,
    n_replicates = k,
    mean_pairwise_agreement = mean(pair_agree, na.rm = TRUE),
    krippendorff_alpha = .krippendorff_alpha_nominal(m),
    n_unanimous = sum(by_row$unanimous %in% TRUE),
    n_ties = sum(by_row$tie %in% TRUE)
  )

  structure(list(by_row = by_row, summary = summary), class = "llmr_agreement")
}

#' @export
print.llmr_agreement <- function(x, ...) {
  s <- x$summary
  cat("-- LLMR replicate agreement --\n")
  cat(sprintf("Units: %d | Replicates: %d\n", s$n_units, s$n_replicates))
  cat(sprintf("Mean pairwise agreement: %.3f\n", s$mean_pairwise_agreement))
  cat(sprintf("Krippendorff's alpha (nominal): %.3f\n", s$krippendorff_alpha))
  cat(sprintf("Unanimous units: %d | Tied units: %d\n", s$n_unanimous, s$n_ties))
  cat("Per-unit detail in $by_row\n")
  invisible(x)
}

# Krippendorff's alpha for nominal data with missing values.
# m: units x coders character matrix. Units with fewer than two non-missing
# codes are excluded (they carry no agreement information).
#' @keywords internal
#' @noRd
.krippendorff_alpha_nominal <- function(m) {
  keep <- rowSums(!is.na(m)) >= 2L
  m <- m[keep, , drop = FALSE]
  if (!nrow(m)) return(NA_real_)

  vals <- sort(unique(stats::na.omit(as.vector(m))))
  if (length(vals) < 2L) {
    # a single category everywhere: no disagreement is observable; by
    # convention alpha is 1 (perfect, though uninformative, agreement)
    return(1)
  }

  # n_uk: counts of category k within unit u; n_u: codes per unit
  n_uk <- vapply(vals, function(v) rowSums(m == v, na.rm = TRUE),
                 numeric(nrow(m)))
  if (is.null(dim(n_uk))) n_uk <- matrix(n_uk, nrow = 1L)
  n_u <- rowSums(n_uk)
  n_total <- sum(n_u)

  # observed disagreement
  Do <- sum(vapply(seq_len(nrow(n_uk)), function(u) {
    sum(n_uk[u, ] * (n_u[u] - n_uk[u, ])) / (n_u[u] - 1)
  }, numeric(1))) / n_total

  # expected disagreement
  n_k <- colSums(n_uk)
  De <- sum(n_k * (n_total - n_k)) / (n_total * (n_total - 1))

  if (De == 0) return(1)
  1 - Do / De
}
