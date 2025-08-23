##globalVariables.R
## to prevent cran note
utils::globalVariables(
  c("config", "messages", "config_label",
    "message_label", "repetition",
    ".param_name_sweep", ".param_value_sweep",
    "user_prompt", "system_prompt",
    "user_prompt_label", "system_prompt_label",
    ":=",
    "structured_ok", "structured_data",
    "structured_valid", "structured_error")
)
#' @importFrom utils head tail modifyList
#' @importFrom vctrs vec_ptype_common vec_cast vec_c
#' @importFrom rlang `:=`
#' @importFrom stats setNames
#' @noRd
NULL


