#' Declare an API key sourced from an environment variable
#'
#' Use this when creating an LLM config to avoid placing secrets
#' inside R objects. Store your key in your shell or in `~/.Renviron`
#' and reference it here by name.
#'
#' @param var Name of the environment variable (e.g., "OPENAI_API_KEY").
#' @param required If TRUE, missing variables cause an authentication error at call time.
#' @param default Optional default if the environment variable is not set.
#' @return An internal secret handle to be used as `api_key = llm_api_key_env("VARNAME")`.
#' @keywords internal
#' @noRd
llm_api_key_env <- function(var, required = TRUE, default = NULL) {
  structure(
    list(
      kind     = "env",
      ref      = as.character(var),
      required = isTRUE(required),
      default  = if (is.null(default)) NULL else as.character(default)[1]
    ),
    class = c("llmr_secret", "llmr_secret_env")
  )
}

# Internal: default environment variables a provider's key may live in.
# Formulaic: <PROVIDER>_API_KEY then <PROVIDER>_KEY, upper-cased. Both forms
# are tried in order by .resolve_api_key(). (Gemini-on-Vertex is handled
# upstream in llm_config(), which selects VERTEX_ACCESS_TOKEN before this.)
#' @keywords internal
#' @noRd
.default_api_key_env <- function(provider) {
  stem <- toupper(provider)
  c(paste0(stem, "_API_KEY"), paste0(stem, "_KEY"))
}

# Internal: resolve a secret handle or an env-spec string into a concrete key string
#' @keywords internal
#' @noRd
.resolve_api_key <- function(secret, provider = NULL, model = NULL) {
  # Back-compat: allow plain strings that name the variable, optionally with "env:" prefix
  if (is.character(secret) && length(secret) == 1L) {
    var <- sub("^env:", "", secret)
    val <- Sys.getenv(var, unset = "")
    if (nzchar(val)) return(val)
    .llmr_error(
      message = sprintf(
        "Missing API key. Set environment variable '%s' for provider '%s'%s.",
        var, provider %||% "?", if (!is.null(model)) paste0(" (model '", model, "')") else ""
      ),
      category = "auth"
    )
  }

  if (inherits(secret, "llmr_secret_literal")) return(secret$value)

  if (inherits(secret, "llmr_secret_env")) {
    for (v in secret$ref) {
      val <- Sys.getenv(v, unset = "")
      if (nzchar(val)) return(val)
    }
    if (!is.null(secret$default) && nzchar(secret$default)) return(secret$default)
    .llmr_error(
      message = sprintf(
        "Missing API key. Set environment variable '%s' for provider '%s'%s.",
        paste(secret$ref, collapse = " or "), provider %||% "?", if (!is.null(model)) paste0(" (model '", model, "')") else ""
      ),
      category = "auth"
    )
  }

  if (is.null(secret) && !is.null(provider)) {
    var <- .default_api_key_env(provider)
    for (v in var) {
      val <- Sys.getenv(v, unset = "")
      if (nzchar(val)) return(val)
    }
    .llmr_error(
      message = sprintf(
        "Missing API key. Set environment variable '%s' for provider '%s'%s.",
        paste(var, collapse=" or "), provider %||% "?", if (!is.null(model)) paste0(" (model '", model, "')") else ""
      ),
      category = "auth"
    )
  }

  .llmr_error("No API key configured.", category = "auth")
}

# Internal: produce a friendly masked representation for prints
#' @keywords internal
#' @noRd
.mask_api_key <- function(secret) {
  if (inherits(secret, "llmr_secret_literal")) return("<llmr_secret: literal>")
  if (inherits(secret, "llmr_secret_env")) return(paste0("<llmr_secret: env:", paste(secret$ref, collapse="|"), ">"))
  if (is.character(secret) && length(secret) == 1L) {
    if (grepl("^env:", secret)) return(paste0("<llmr_secret: ", secret, ">"))
    return("<llmr_secret: string>")
  }
  "<llmr_secret>"
}

# Internal: mask only the api_key field for troubleshooting prints
#' @keywords internal
#' @noRd
.mask_config_for_print <- function(config) {
  cfg <- config
  cfg$api_key <- .mask_api_key(cfg$api_key)
  cfg
}


