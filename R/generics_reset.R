# generics_reset.R --------------------------------------------------------------
# A shared S3 generic for resetting a stateful LLMR-family object to its initial
# position (for example, an archive replayer whose queue has advanced). LLMR
# defines the generic and an erroring default only; method packages register
# class-specific methods. Additive: no existing LLMR behavior changes.

#' Reset a stateful object to its initial position
#'
#' A shared generic across the LLMR method packages, for objects that carry
#' consumable state. LLMR defines the generic and an erroring default only; a
#' method package (for example, an archive replayer) registers the concrete
#' method that restores its state.
#'
#' @param x An object with resettable state.
#' @param ... Passed to methods.
#' @return `x`, invisibly, with its state restored.
#' @examples
#' # A method restores the object's state, here a replay queue's position:
#' reset.toy_queue <- function(x, ...) {
#'   x$position <- 1L
#'   invisible(x)
#' }
#' q <- structure(list(items = c("a", "b", "c"), position = 3L), class = "toy_queue")
#' reset(q)$position
#' @export
reset <- function(x, ...) {
  UseMethod("reset")
}

#' @export
reset.default <- function(x, ...) {
  cls <- class(x)
  if (!length(cls)) cls <- typeof(x)
  .llmr_error(
    message = sprintf(
      "No reset() method for objects of class <%s>; this generic is implemented by the LLMR method packages.",
      paste(cls, collapse = ", ")
    ),
    category = "param"
  )
}
