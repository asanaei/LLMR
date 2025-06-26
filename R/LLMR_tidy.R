# LLMR_tidy.R ---------------------------------------------------------------
#' Vectorised LLM transformer
#'
#' @importFrom tidyr expand_grid
#' @importFrom rlang `:=`
#'
#' \code{llm_fn()} turns every element (or row) of \code{x} into an LLM prompt
#' (via glue templating) and returns the model’s reply.
#'
#' *Stateless* – no memory across calls, so it is **not** an agent.
#'
#' @param x  A character vector **or** a data.frame/tibble.
#' @param prompt A glue template string.
#'   *If* \code{x} is a data frame, use \code{{col}} placeholders;
#'   *if* \code{x} is a vector, refer to the element as \code{{x}}.
#' @param .config An \link{llm_config} object.
#' @param .system_prompt Optional system message (character scalar).
#' @param ... Passed unchanged to \link{call_llm_broadcast} (e.g.\ \code{tries},
#'   \code{progress}, \code{verbose}).
#'
#' @return A character vector the same length as \code{x}.
#'   Failed calls yield \code{NA}.
#' @details
#' Runs each prompt through `call_llm_broadcast()`, which forwards the
#' requests to `call_llm_par()`.  That core engine executes them **in
#' parallel** according to the current *future* plan.  For instant
#' multi-core use, call `setup_llm_parallel(workers = 4)` (or whatever
#' number you prefer) once per session; revert with `reset_llm_parallel()`.
#'
#' @seealso setup_llm_parallel, reset_llm_parallel, call_llm_par
#' @export
#'
#' @examples
#' ## --- Vector input ------------------------------------------------------
#' \dontrun{
#' cfg <- llm_config(
#'   provider = "openai",
#'   model    = "gpt-4.1-nano",
#'   api_key  =  Sys.getenv("OPENAI_API_KEY"),
#'   temperature = 0
#' )
#'
#' words <- c("excellent", "awful", "average")
#'
#' llm_fn(
#'   words,
#'   prompt   = "Classify sentiment of '{x}' as Positive, Negative, or Neutral.",
#'   .config  = cfg,
#'   .system_prompt = "Respond with ONE word only."
#' )
#'
#' ## --- Data-frame input inside a tidyverse pipeline ----------------------
#' library(dplyr)
#'
#' reviews <- tibble::tibble(
#'   id     = 1:3,
#'   review = c("Great toaster!", "Burns bread.", "It's okay.")
#' )
#'
#' reviews |>
#'   llm_mutate(
#'     sentiment,
#'     prompt  = "Classify the sentiment of this review: {review}",
#'     .config = cfg,
#'     .system_prompt = "Respond with Positive, Negative, or Neutral."
#'   )
#' }
llm_fn <- function(x,
                   prompt,
                   .config,
                   .system_prompt = NULL,
                   ...) {

  stopifnot(inherits(.config, "llm_config"))

  user_txt <- if (is.data.frame(x)) {
    glue::glue_data(x, prompt)
  } else {
    glue::glue_data(list(x = x), prompt)
  }

  msgs <- lapply(user_txt, function(txt) {
    if (is.null(.system_prompt)) {
      list(list(role = "user", content = txt))
    } else {
      list(
        list(role = "system", content = .system_prompt),
        list(role = "user",   content = txt)
      )
    }
  })

  res <- call_llm_broadcast(
    config        = .config,
    messages_list = msgs,
    ...
  )

  ifelse(res$success, res$response_text, NA_character_)
}

#' Mutate a data frame with LLM output
#'
#' A convenience wrapper around \link{llm_fn} that inserts the result as a new
#' column via \link[dplyr]{mutate}.
#'
#' @inheritParams llm_fn
#' @param .data  A data frame / tibble.
#' @param output Unquoted name of the new column you want to add.
#' @param .before,.after Standard \link[dplyr]{mutate} column-placement helpers.
#' @details
#' Internally calls `llm_fn()`, so the API requests inherit the same
#' parallel behaviour.  Activate parallelism with
#' `setup_llm_parallel()` and shut it off with `reset_llm_parallel()`.
#'
#' @seealso llm_fn, setup_llm_parallel, reset_llm_parallel
#' @export
#'
#' @examples
#' ## See examples under \link{llm_fn}.
llm_mutate <- function(.data,
                       output,
                       prompt,
                       .config,
                       .system_prompt = NULL,
                       .before = NULL,
                       .after  = NULL,
                       ...) {

  out <- rlang::enquo(output)
  new_vals <- llm_fn(.data,
                     prompt         = prompt,
                     .config        = .config,
                     .system_prompt = .system_prompt,
                     ...)
  .data |>
    dplyr::mutate(
      !!out := new_vals,
      .before = {{ .before }},
      .after  = {{ .after }}
    )
}

utils::globalVariables(
  c("config", "messages", "config_label",
    "message_label", "repetition",
    ".param_name_sweep", ".param_value_sweep")
)
