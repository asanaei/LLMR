# personas.R ----------------------------------------------------------------
# A small, non-GUI contract for "persona" data frames: one respondent per row,
# columns that are demographics or survey/attitude answers, optionally carrying a
# `dictionary` attribute (handle -> question wording -> domain -> source variable)
# and a `demographic_fields` attribute. The bundled `anes_2024_personas` follows
# this contract. These helpers let any package read such a frame the same way:
# split a row into labeled demographics and labeled answers, summarize the frame
# for display, and validate the contract. Rendering a persona into a prompt is
# left to the consumer (it differs by task), but the field extraction here is the
# shared part.

# Common demographic column names, used only as a fallback when a frame carries
# no `demographic_fields` attribute.
.llmr_persona_common_demographics <- c(
  "age", "sex", "gender", "education", "race", "race/ethnicity",
  "marital status", "household income", "income", "religion",
  "census region", "region", "community type", "employment status",
  "home ownership", "children in household", "union household",
  "military service", "attention to politics",
  paste0("demo_", c("age", "sex", "gender", "education", "race", "income")))

#' The question dictionary attached to a persona data frame
#'
#' Persona frames may carry a `dictionary` attribute: a data frame mapping each
#' tidy column handle to the human-readable question wording (and, when present,
#' its domain and source-survey variable). [llm_persona_split()] and
#' [llm_persona_overview()] use it to show questions rather than column handles.
#'
#' @param x A persona data frame (see [anes_2024_personas]).
#' @return The dictionary data frame, or `NULL` if the frame carries none.
#' @seealso [llm_persona_split()], [anes_2024_personas].
#' @export
llm_persona_dictionary <- function(x) {
  d <- attr(x, "dictionary")
  if (is.null(d) || !is.data.frame(d)) return(NULL)
  d
}

#' Which columns of a persona frame are demographics
#'
#' Returns the demographic column names. A persona frame should mark these in a
#' `demographic_fields` attribute; when it does not, a small set of common
#' demographic names found in the frame is used as a fallback.
#'
#' @param x A persona data frame.
#' @return A character vector of column names present in `x`.
#' @seealso [llm_persona_split()].
#' @export
llm_persona_demographic_fields <- function(x) {
  f <- attr(x, "demographic_fields")
  if (is.null(f)) f <- intersect(.llmr_persona_common_demographics, names(x))
  intersect(f, names(x))
}

# Map column handles to their question wording via the dictionary; fall back to
# the handle itself when no entry exists.
.llmr_persona_relabel <- function(x, cols) {
  d <- llm_persona_dictionary(x)
  if (is.null(d) || !all(c("handle", "question") %in% names(d))) return(cols)
  q <- d$question[match(cols, d$handle)]
  ifelse(is.na(q), cols, q)
}

#' Split one persona into labeled demographics and labeled answers
#'
#' Extracts row `i` of a persona frame into two named character vectors: the
#' demographic fields and the survey/attitude answers. Names are the question
#' wording when the frame carries a `dictionary`, otherwise the column handles.
#' Missing values are dropped, and any score/index column (e.g. `ideology_score`)
#' is treated as metadata, not an answer. This is the shared field-extraction
#' step; how the result becomes prompt text is up to the caller.
#'
#' @param x A persona data frame (see [anes_2024_personas]).
#' @param i Integer row index.
#' @param drop Character vector of column names to exclude from both parts
#'   (defaults to `"ideology_score"`).
#' @return A list with two named character vectors, `demographics` and
#'   `responses`.
#' @seealso [llm_persona_demographic_fields()], [anes_2024_personas].
#' @examples
#' data(anes_2024_personas, package = "LLMR")
#' parts <- llm_persona_split(anes_2024_personas, 1)
#' names(parts$demographics)[1:3]
#' @export
llm_persona_split <- function(x, i, drop = "ideology_score") {
  stopifnot(is.data.frame(x), length(i) == 1L, i >= 1L, i <= nrow(x))
  demo_cols <- llm_persona_demographic_fields(x)
  ans_cols <- setdiff(names(x), c(demo_cols, drop))

  pull <- function(cols) {
    if (!length(cols)) return(stats::setNames(character(0), character(0)))
    vals <- vapply(cols, function(cl) {
      v <- x[[cl]][i]
      if (length(v) != 1L || is.na(v) || !nzchar(trimws(as.character(v))))
        NA_character_ else as.character(v)
    }, character(1))
    keep <- !is.na(vals)
    stats::setNames(unname(vals[keep]), .llmr_persona_relabel(x, cols[keep]))
  }

  list(demographics = pull(demo_cols), responses = pull(ans_cols))
}

#' A compact overview of a persona frame for display
#'
#' Builds a small data frame summarizing the personas, suitable for a browse or
#' selection table. By default it surfaces a few widely useful fields (an ideology
#' score when present, plus party, age, race, region, and religion when the
#' dictionary names them); pass `columns` to choose your own.
#'
#' @param x A persona data frame.
#' @param columns Optional character vector of either column handles or question
#'   wordings to include. When `NULL`, a default set is used.
#' @return A data frame with one row per persona.
#' @seealso [anes_2024_personas].
#' @export
llm_persona_overview <- function(x, columns = NULL) {
  d <- llm_persona_dictionary(x)
  by_question <- function(q) {
    if (is.null(d)) return(NULL)
    h <- d$handle[match(q, d$question)]
    if (length(h) && !is.na(h) && h %in% names(x)) x[[h]] else NULL
  }
  if (is.null(columns)) {
    out <- data.frame(
      ideology = if ("ideology_score" %in% names(x)) x$ideology_score else NULL,
      party    = by_question("Party identification"),
      age      = by_question("age"),
      race     = by_question("race/ethnicity"),
      region   = by_question("census region"),
      religion = by_question("religion"),
      stringsAsFactors = FALSE, check.names = FALSE)
    if (!ncol(out)) out <- as.data.frame(x[, seq_len(min(6L, ncol(x)))],
                                         stringsAsFactors = FALSE)
    return(out)
  }
  # caller-specified columns: accept handles or question wordings
  d2 <- llm_persona_dictionary(x)
  resolve <- function(c) {
    if (c %in% names(x)) return(c)
    if (!is.null(d2)) { h <- d2$handle[match(c, d2$question)]; if (!is.na(h)) return(h) }
    NA_character_
  }
  cols <- vapply(columns, resolve, "")
  cols <- cols[!is.na(cols) & cols %in% names(x)]
  out <- as.data.frame(x[, cols, drop = FALSE], stringsAsFactors = FALSE,
                       check.names = FALSE)
  names(out) <- .llmr_persona_relabel(x, cols)
  out
}

#' Check that a data frame follows the persona contract
#'
#' Verifies the minimum a persona frame needs: it is a data frame with at least
#' one row. It also reports, via a message when `verbose`, whether the optional
#' `dictionary` and `demographic_fields` attributes are present. It is tolerant:
#' a frame without those attributes is still usable (handles stand in for
#' questions, and demographics are guessed from common names).
#'
#' @param x A candidate persona data frame.
#' @param verbose Logical; if `TRUE`, report which optional attributes are present.
#' @return `TRUE` invisibly if `x` is a usable persona frame; otherwise it raises
#'   an error.
#' @seealso [llm_persona_split()], [anes_2024_personas].
#' @export
llm_validate_persona_frame <- function(x, verbose = FALSE) {
  if (!is.data.frame(x)) rlang::abort("A persona frame must be a data frame.")
  if (!nrow(x)) rlang::abort("A persona frame must have at least one row.")
  if (isTRUE(verbose)) {
    has_dict <- !is.null(llm_persona_dictionary(x))
    has_demo <- !is.null(attr(x, "demographic_fields"))
    message(sprintf("persona frame: %d rows, %d cols; dictionary %s; demographic_fields %s.",
                    nrow(x), ncol(x),
                    if (has_dict) "present" else "absent (handles used as questions)",
                    if (has_demo) "present" else "absent (guessed from common names)"))
  }
  invisible(TRUE)
}
