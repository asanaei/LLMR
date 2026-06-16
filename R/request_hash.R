# request_hash.R --------------------------------------------------------------
# Request identity. Two calls are the same question only if everything that can
# change the answer is the same: provider, model, the messages, and the
# generation parameters (temperature, seed, max_tokens, ...), plus any tool or
# schema signature. The record/dedup/replay layers (LLMRcontent) key on this.
# A request hash is NOT a content hash of the response; it identifies the call
# that produced one.

# The generation params worth hashing are everything except the transport knobs;
# see .llmr_drop_transport() in log_messages.R, which both this (config) side and
# the log side route through so the same call hashes identically.

#' Stable request hash for an LLM call
#'
#' A request hash identifies the question asked. It keys on provider, model, the
#' canonical role/content turns, and the generation parameters
#' (`model_params` minus transport and internal knobs), plus any explicit tool,
#' response-format, or schema signature. It is the key the archive layer uses for
#' deduplication and replay, and the value to record when reproducibility of a
#' specific call matters.
#'
#' The hash is built over a canonical object and passed to [llm_hash()], so it
#' inherits that convention: construction order and S3 class do not matter, and
#' the value does not depend on R's serialization format. API keys and volatile
#' fields (timestamps, request ids) are never part of the input. Transport and
#' internal knobs in `model_params` (see the source for the full list -- e.g.
#' `req_builder`, `response_modifier`, `timeout`, `api_url`, `max_tries`,
#' `request_modifier`, `cache`, `use_responses_api`, `vertex`) are excluded,
#' since they change how a call is issued, not what is asked.
#'
#' @section Scope:
#' The hash keys on the canonical turns and generation parameters. It is designed
#' so a call described by an [llm_config()] and the same call read from a logged
#' provider request body agree for the common chat/completions path. It does
#' *not* attempt to reconstruct every provider's exact translated body: calls
#' that differ only in features outside this key -- the OpenAI Responses API
#' (`input`/`instructions`), full tool-call transcripts, or provider-injected
#' defaults -- may hash the same across the config and log sides. The archive
#' layer's collision detection is the backstop for those cases: it flags when one
#' replay key gathers records whose stored request hashes disagree.
#'
#' @param config An [llm_config()] (its `provider`, `model`, and the
#'   answer-relevant entries of `model_params` are used), or `NULL` to supply
#'   `provider`/`model` directly and the generation parameters through
#'   `extra$params`.
#' @param messages The messages or prompt sent. A character scalar, a named
#'   character vector of roles, or a list of `list(role=, content=)` turns --
#'   whatever was passed to [call_llm()]. Canonicalized to a provider-neutral
#'   list of role/content turns before hashing, so the message *shape* (a bare
#'   string vs a named vector vs a turn list) does not change the hash; only the
#'   roles and text do.
#' @param provider,model Provider and model ids. Taken from `config` when it is
#'   given; supply them directly (with `extra$params`) when hashing a call read
#'   from an audit log, where there is no `config`.
#' @param tools Optional tool definitions (as passed to [call_llm()] /
#'   [bind_tools()]). Their signature is included when present.
#' @param response_format Optional response-format directive (e.g. a JSON-mode
#'   flag or object) that constrains the output.
#' @param schema Optional JSON Schema used for structured output.
#' @param extra Optional named list folded into the hashed object. The log side
#'   passes the call's generation parameters here as `extra$params` (a named
#'   list), which are consumed into the param key exactly as a config's
#'   `model_params` would be; a config, when given, takes precedence over
#'   `extra$params`. Any other entries are hashed as-is, to add identity-relevant
#'   fields the standard arguments do not cover.
#' @return A 64-character lowercase SHA-256 hex string (see [llm_hash()]).
#' @seealso [llm_hash()], [llm_response_record()]
#' @examples
#' cfg <- llm_config("openai", "gpt-4.1-mini", temperature = 0)
#' h1 <- llm_request_hash(cfg, "Hello")
#' # message order/content matters; temperature matters:
#' cfg2 <- llm_config("openai", "gpt-4.1-mini", temperature = 1)
#' identical(h1, llm_request_hash(cfg2, "Hello"))
#' @export
llm_request_hash <- function(config = NULL, messages = NULL, provider = NULL,
                             model = NULL, tools = NULL,
                             response_format = NULL, schema = NULL,
                             extra = NULL) {
  # provider/model come from the config when given, else from the direct args.
  if (!is.null(config)) {
    provider <- config$provider
    model <- config$model
  }

  # Generation params: from the config's model_params (calling side), or from
  # extra$params (log side, which has no config). Both route through
  # .llmr_drop_transport so the same call hashes identically. `params` is always
  # consumed out of `extra` so it can never be counted twice -- once in
  # obj$params and again under obj$extra. When a config is given it takes
  # precedence over any extra$params.
  if (!is.null(config)) {
    params <- .llmr_drop_transport(config$model_params)
  } else if (is.list(extra) && !is.null(extra$params)) {
    params <- .llmr_drop_transport(extra$params)
  } else {
    params <- list()
  }
  if (is.list(extra) && !is.null(extra$params)) {
    extra$params <- NULL
    if (!length(extra)) extra <- NULL
  }

  # Canonicalize messages to provider-neutral turns so the shape does not matter.
  turns <- if (is.null(messages)) NULL else .llmr_turns(messages = messages)

  obj <- list(
    provider        = tolower(as.character(provider %||% "")),
    model           = as.character(model %||% ""),
    messages        = turns,
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
