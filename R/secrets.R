#' Declare an API key sourced from an environment variable
#'
#' Reference an API key by the name of the environment variable that holds it,
#' so the secret never appears in your R code or saved objects. Store the key in
#' your shell profile or in `~/.Renviron` (e.g. `OPENAI_API_KEY=sk-...`).
#'
#' Best practice is to not pass a key explicitly at all: [llm_config()] already
#' looks up the standard variable for each provider (`<PROVIDER>_API_KEY`, then
#' `<PROVIDER>_KEY`). Use `llm_api_key_env()` only when your variable has a
#' non-standard name.
#'
#' @param var Name of the environment variable (e.g., "OPENAI_API_KEY"). A
#'   character vector is also accepted; the variables are tried in order and the
#'   first one that is set wins, which is convenient when a key may live under
#'   more than one name (e.g., `c("GROQ_API_KEY", "GROQ_KEY")`).
#' @param required If TRUE, a missing variable raises an authentication error at
#'   call time. If FALSE, a missing variable resolves to an empty key, which is
#'   appropriate for providers that do not require authentication (e.g., a local
#'   Ollama server).
#' @param default Optional default used if the environment variable is not set.
#' @return A secret handle to pass as `api_key = llm_api_key_env("VARNAME")` in
#'   [llm_config()].
#' @seealso [llm_config()]
#' @examples
#' cfg <- llm_config(
#'   "openai", "gpt-4o-mini",
#'   api_key = llm_api_key_env("MY_OPENAI_KEY")
#' )
#' @export
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
  # Back-compat: configs saved by older LLMR versions stored a plain string here,
  # either the name of an environment variable (optionally with an "env:" prefix)
  # or the key itself.
  if (is.character(secret) && length(secret) == 1L) {
    var <- sub("^env:", "", secret)
    looks_like_env <- grepl("^env:", secret) || grepl("^[A-Z][A-Z0-9_]*$", var)
    if (looks_like_env) {
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
    # Not env-shaped: this is a literal key from a legacy config. Use it, but
    # never echo it in any message.
    if (is.na(secret) || !nzchar(secret)) {
      .llmr_error(
        message = sprintf(
          "Empty API key for provider '%s'. Set '%s' in your environment, or pass api_key = llm_api_key_env(\"VARNAME\").",
          provider %||% "?",
          paste(.default_api_key_env(provider %||% "PROVIDER"), collapse = "' or '")
        ),
        category = "auth"
      )
    }
    return(secret)
  }

  if (inherits(secret, "llmr_secret_literal")) {
    val <- secret$value
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
      .llmr_error(
        message = sprintf(
          "Empty or invalid literal API key for provider '%s'. Set '%s' in your environment, or pass api_key = llm_api_key_env(\"VARNAME\").",
          provider %||% "?",
          paste(.default_api_key_env(provider %||% "PROVIDER"), collapse = "' or '")
        ),
        category = "auth"
      )
    }
    return(val)
  }

  if (inherits(secret, "llmr_secret_env")) {
    for (v in secret$ref) {
      val <- Sys.getenv(v, unset = "")
      if (nzchar(val)) return(val)
    }
    if (!is.null(secret$default) && nzchar(secret$default)) return(secret$default)
    # required = FALSE: a missing variable is acceptable (e.g. a provider that
    # does not need a key, or optional auth). Return an empty string instead of
    # raising an authentication error.
    if (isFALSE(secret$required)) return("")
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


