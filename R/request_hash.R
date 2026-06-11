# request_hash.R --------------------------------------------------------------
# Request identity. Two calls are the same question only if everything that can
# change the answer is the same: provider, model, the messages, and the
# generation parameters (temperature, seed, max_tokens, ...), plus any tool or
# schema signature. The record/dedup/replay layers (LLMRarchive) key on this.
# A request hash is NOT a content hash of the response; it identifies the call
# that produced one.

# Internal: the model_params worth hashing. Drop the closures and transport
# knobs that change how a call is issued without changing the answer
# (req_builder, response_modifier, timeout), and never let an api_url that
# merely points at a mirror split the identity of an otherwise identical call.
# Keep generation parameters: temperature, top_p, seed, max_tokens, stop,
# presence/frequency penalties, logprobs, and anything else a provider honors.
.llmr_hashable_params <- function(model_params) {
  if (!is.list(model_params) || !length(model_params)) return(list())
  drop <- c("req_builder", "response_modifier", "timeout", "api_url",
            "max_tries", "verbose")
  keep <- model_params[setdiff(names(model_params), drop)]
  # Functions remaining in params (rare) hash by deparsed source via llm_hash;
  # nothing here needs special handling.
  keep
}

#' Stable request hash for an LLM call
#'
#' A request hash identifies the question asked, so two calls collide only when
#' everything that can change the answer is identical: provider, model, the
#' messages, the generation parameters, and any tool or response-format
#' signature. It is the key the archive layer uses for deduplication and replay,
#' and the value to record when reproducibility of a specific call matters.
#'
#' The hash is built over a canonical object and passed to [llm_hash()], so it
#' inherits that convention: construction order and S3 class do not matter, and
#' the value does not depend on R's serialization format. API keys and volatile
#' fields (timestamps, request ids) are never part of the input. Transport-only
#' knobs in `model_params` -- `req_builder`, `response_modifier`, `timeout`,
#' `api_url`, `max_tries`, `verbose` -- are excluded, since they change how a
#' call is issued, not what is asked.
#'
#' @param config An [llm_config()] (its `provider`, `model`, and the
#'   answer-relevant entries of `model_params` are used), or `NULL` to supply
#'   `provider`/`model`/`params` directly through `extra`.
#' @param messages The messages or prompt sent. A character scalar, a named
#'   character vector of roles, or a list of `list(role=, content=)` turns --
#'   whatever was passed to [call_llm()]. Canonicalized before hashing.
#' @param tools Optional tool definitions (as passed to [call_llm()] /
#'   [bind_tools()]). Their signature is included when present.
#' @param response_format Optional response-format directive (e.g. a JSON-mode
#'   flag or object) that constrains the output.
#' @param schema Optional JSON Schema used for structured output.
#' @param extra Optional named list folded into the hashed object for callers
#'   that hold parameters outside a config (e.g. `list(provider=, model=,
#'   params=, seed=)`). Use it to add identity-relevant fields the standard
#'   arguments do not cover.
#' @return A 64-character lowercase SHA-256 hex string (see [llm_hash()]).
#' @seealso [llm_hash()], [llm_response_record()]
#' @examples
#' cfg <- llm_config("openai", "gpt-4.1-mini", temperature = 0)
#' h1 <- llm_request_hash(cfg, "Hello")
#' # message order/content matters; temperature matters:
#' cfg2 <- llm_config("openai", "gpt-4.1-mini", temperature = 1)
#' identical(h1, llm_request_hash(cfg2, "Hello"))
#' @export
llm_request_hash <- function(config = NULL, messages = NULL, tools = NULL,
                             response_format = NULL, schema = NULL,
                             extra = NULL) {
  provider <- NULL
  model <- NULL
  params <- list()
  if (!is.null(config)) {
    provider <- config$provider
    model <- config$model
    params <- .llmr_hashable_params(config$model_params)
  }

  obj <- list(
    provider        = tolower(as.character(provider %||% "")),
    model           = as.character(model %||% ""),
    messages        = messages,
    params          = params,
    tools           = tools,
    response_format = response_format,
    schema          = schema,
    extra           = extra
  )
  # Drop empty branches so an absent tool set or schema does not change the
  # hash relative to a call that never mentioned them.
  obj <- obj[vapply(obj, function(v) !is.null(v) &&
                      !(is.list(v) && length(v) == 0L), logical(1))]
  llm_hash(obj)
}
