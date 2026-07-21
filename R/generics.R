# generics.R ----------------------------------------------------------------
# Two shared S3 generics for LLMR and its method packages (LLMRcontent,
# LLMRpanel). LLMR implements report() for its experiment results; the method
# packages register methods for their own result classes.

#' Machine-readable diagnostics for an LLMR-family result object
#'
#' A shared generic across the LLMR method packages. It returns the small,
#' machine-readable set of health numbers behind a result object (the part you
#' would assert in a test or drop into a table), as distinct from [report()],
#' which drafts methods-section prose.
#'
#' LLMR defines the generic and an erroring default only. The method packages
#' (LLMRcontent, LLMRpanel) provide the implementations,
#' each returning a tibble of the key numbers for its own result classes.
#'
#' @param x An LLMR experiment or an object returned by an LLMR method package.
#' @param ... Passed to methods.
#' @return A method-defined object, by convention a tibble of diagnostic values.
#' @seealso [report()]
#' @examples
#' \dontrun{
#' # LLMRcontent, for instance, returns one row of stability and fragility numbers:
#' diagnostics(audit)
#' }
#' @export
diagnostics <- function(x, ...) {
  UseMethod("diagnostics")
}

#' @export
diagnostics.default <- function(x, ...) {
  cls <- class(x)
  if (!length(cls)) cls <- typeof(x)
  .llmr_error(
    message = sprintf(
      "No diagnostics() method for objects of class <%s>; this generic is implemented by the LLMR method packages (LLMRcontent, LLMRpanel).",
      paste(cls, collapse = ", ")
    ),
    category = "param"
  )
}

#' Draft a methods-section report from an LLMR-family result object
#'
#' A shared generic across the LLMR method packages. It returns the
#' methods-section prose and tables for a result object (what a paper's
#' appendix would print), as distinct from [diagnostics()], which returns the
#' machine-readable numbers.
#'
#' LLMR implements this generic for `llmr_experiment` objects returned by
#' [call_llm_par()] and its wrappers. The method packages (LLMRcontent,
#' LLMRpanel) provide methods for their own result classes.
#'
#' @param x An LLMR experiment or an object returned by an LLMR method package.
#' @param ... Passed to methods (some methods require extra arguments, e.g.
#'   some LLMRcontent report methods require the gold set and protocol).
#' @param prefix For an `llmr_experiment`, the output-column prefix when the
#'   object uses [llm_mutate()] diagnostic names. It is inferred when possible.
#' @param task For an `llmr_experiment`, an optional clause describing the task,
#'   such as `"to classify open-ended responses"`.
#' @return A method-defined report object, by convention a character vector
#'   with a print method. The `llmr_experiment` method returns a character
#'   scalar containing a draft methods paragraph.
#' @seealso [diagnostics()], [llm_usage()], [llm_log_enable()]
#' @examples
#' \dontrun{
#' results <- call_llm_broadcast(cfg, c("First prompt", "Second prompt"))
#' cat(report(results, task = "to classify short texts"))
#' }
#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' @export
report.default <- function(x, ...) {
  cls <- class(x)
  if (!length(cls)) cls <- typeof(x)
  .llmr_error(
    message = sprintf(
      "No report() method for objects of class <%s>; methods are available for LLMR experiments and result objects from the LLMR method packages.",
      paste(cls, collapse = ", ")
    ),
    category = "param"
  )
}
