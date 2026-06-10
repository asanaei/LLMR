# render_messages.R =========================================================
# Single source of truth for turning a prompt template or a `.messages`
# template vector into the per-row message objects that the generative call
# path consumes. Both llm_fn() and llm_mutate() delegate here, and llm_preview()
# reuses the very same code so a dry-run can never drift from what is actually
# sent. Behaviour is locked by golden tests in test-render-messages.R.

# Roles accepted in a `.messages` template. Kept identical to the value used at
# the llm_fn()/llm_mutate() call sites.
.llmr_roles_allowed <- c("system", "user", "assistant", "file")

#' Render one row of a `.messages` template into a role-named character vector
#'
#' Mirrors the historical local helper `eval_messages_one_row()` exactly: roles
#' come from `names(tpl_vec)`, unnamed/empty/NA names default to `"user"`, and
#' each template element is glued once against the row. `USE.NAMES = FALSE` (via
#' the explicit `names(out) <- roles`) is what keeps a bare template's text from
#' being used as its own role (the 0.7.2 fix).
#'
#' @keywords internal
#' @noRd
.llm_eval_messages_one_row <- function(row_df, tpl_vec) {
  roles <- names(tpl_vec)
  if (is.null(roles)) roles <- rep("user", length(tpl_vec))
  roles[is.na(roles) | roles == ""] <- "user"

  out <- vapply(
    unname(tpl_vec),
    function(s) as.character(glue::glue_data(row_df, s, .na = "")),
    FUN.VALUE = character(1)
  )
  names(out) <- roles
  out
}

#' Build per-row generative messages from `prompt` or `.messages`
#'
#' Returns a length-`nrow(df)` list. Each element is either a bare unnamed
#' character scalar (prompt-only, no system) or a role-named character vector,
#' exactly matching what llm_fn()/llm_mutate() produced before this helper
#' existed. This function performs NO network or file I/O: a `"file"` role stays
#' a glued path string, just as it did when handed to call_llm_broadcast().
#'
#' @param df A data.frame/tibble. (llm_fn's scalar-vector input is wrapped as a
#'   one-column data.frame by the caller before reaching here.)
#' @param prompt A single glue template, or NULL.
#' @param .messages A (optionally role-named) character template vector, or NULL.
#' @param .system_prompt A system string to prepend if absent, or NULL.
#' @param roles_allowed Character vector of accepted role names.
#' @keywords internal
#' @noRd
.llm_build_messages_df <- function(df, prompt = NULL, .messages = NULL,
                                   .system_prompt = NULL,
                                   roles_allowed = .llmr_roles_allowed) {
  n <- nrow(df)
  if (is.null(n) || is.na(n)) {
    stop("Internal: `.data` has no rows; ensure you pass a data.frame/tibble.")
  }

  msgs <- vector("list", n)

  if (!is.null(.messages)) {
    stopifnot(is.character(.messages), length(.messages) > 0)
    roles <- names(.messages)
    if (is.null(roles)) roles <- rep("user", length(.messages))
    roles[is.na(roles) | roles == ""] <- "user"
    names(.messages) <- roles
    bad <- setdiff(unique(names(.messages)), roles_allowed)
    if (length(bad)) {
      stop(sprintf("Unsupported roles in .messages: %s",
                   paste(bad, collapse = ", ")))
    }
    for (i in seq_len(n)) {
      row_msg <- .llm_eval_messages_one_row(df[i, , drop = FALSE], .messages)
      if (!is.null(.system_prompt) && !"system" %in% names(row_msg)) {
        row_msg <- c(system = .system_prompt, row_msg)
      }
      msgs[[i]] <- row_msg
    }
  } else {
    if (is.null(prompt)) stop("Either 'prompt' or '.messages' must be provided.")
    user_txt <- glue::glue_data(df, prompt, .na = "")
    for (i in seq_len(n)) {
      if (is.null(.system_prompt)) {
        msgs[[i]] <- as.character(user_txt[i])
      } else {
        msgs[[i]] <- c(system = .system_prompt, user = as.character(user_txt[i]))
      }
    }
  }

  msgs
}
