# generics.R ----------------------------------------------------------------
# Two shared S3 generics for the LLMR method packages (LLMRcontent,
# LLMRpanel). LLMR defines the generics and an erroring default
# only; the method packages register class-specific methods. Additive: no
# existing LLMR behavior changes.

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
#' @param x An object returned by an LLMR method package.
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
#' LLMR defines the generic and an erroring default only. The method packages
#' (LLMRcontent, LLMRpanel) provide the implementations.
#'
#' @param x An object returned by an LLMR method package.
#' @param ... Passed to methods (some methods require extra arguments, e.g.
#'   some LLMRcontent report methods require the gold set and protocol).
#' @return A method-defined report object, by convention a character vector
#'   with a print method.
#' @seealso [diagnostics()]
#' @examples
#' \dontrun{
#' # LLMRcontent, for instance, drafts the robustness appendix:
#' report(audit)
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
      "No report() method for objects of class <%s>; this generic is implemented by the LLMR method packages (LLMRcontent, LLMRpanel).",
      paste(cls, collapse = ", ")
    ),
    category = "param"
  )
}
