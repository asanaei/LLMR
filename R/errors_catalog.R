# errors_catalog.R
# -------------------------------------------------------------------
# Catalog of documented provider error codes and the classifier that
# perform_request() uses to type an HTTP failure before raising it.
#
# Categories (each becomes a condition class llmr_api_<category>_error):
#   param       request-side: malformed request, bad parameter, unknown
#               model, oversized payload. Not retryable.
#   auth        key, permission, or region problem. Not retryable.
#   billing     empty balance, exhausted credit, or a spend cap. Not
#               retryable: no repeat of the same request can succeed until
#               the account changes.
#   rate_limit  throttling. Retryable with backoff.
#   server      provider-side failure or overload. Retryable.
#   unknown     anything the catalog cannot place.
#
# The same code string can mean different things at different providers,
# which is why the catalog is keyed by provider. Two documented examples:
# `insufficient_quota` at OpenAI means the account is out of credit
# (billing), while at DashScope (Alibaba) it means requests-per-minute
# throttling (rate_limit); a 403 at xAI is a permission problem (auth),
# while at Together it signals a prompt that exceeds the model's context
# window (param).
#
# Sources, all read 2026-07-31:
#   openai     https://developers.openai.com/api/docs/guides/error-codes
#   anthropic  https://platform.claude.com/docs/en/api/errors
#   gemini     https://ai.google.dev/gemini-api/docs/troubleshooting
#   groq       https://console.groq.com/docs/errors
#   deepseek   https://api-docs.deepseek.com/quick_start/error_codes
#   moonshot   https://platform.kimi.ai/docs/guide/troubleshooting
#   alibaba    https://www.alibabacloud.com/help/en/model-studio/error-code
#   zhipu      https://docs.bigmodel.cn/cn/faq/api-code
#   together   https://docs.together.ai/docs/error-codes
#   openrouter https://openrouter.ai/docs/api-reference/errors
#   voyage     https://docs.voyageai.com/docs/error-codes
#   ollama     https://github.com/ollama/ollama/blob/main/docs/api.md
#               (error bodies are {"error": "<text>"}, observed behavior;
#               the shape is not formally documented)
#   xai        https://docs.x.ai/developers/debugging
# Xiaomi publishes no public error reference; its endpoint is
# OpenAI-compatible and the status mapping below covers it.

# Status-code mapping shared by every provider. Provider-specific entries
# below override it where a provider's documentation departs from it.
.llmr_status_category <- function(status) {
  s <- suppressWarnings(as.integer(status[1]))
  if (is.na(s)) return("unknown")
  if (s >= 500L) return("server")
  switch(as.character(s),
    "400" = "param",
    "401" = "auth",
    "402" = "billing",     # deepseek, together, openrouter: payment required
    "403" = "auth",
    "404" = "param",       # almost always a wrong model id or endpoint
    "408" = "server",      # timeout: transient
    "409" = "server",      # conflict: anthropic documents it as retryable
    "413" = "param",       # request too large
    "422" = "param",       # validation failure (deepseek, groq)
    "429" = "rate_limit",
    "498" = "server",      # groq: flex-tier capacity exceeded, transient
    "unknown"
  )
}

# Documented body-level code/type/status strings, by provider.
.llmr_error_catalog <- list(
  openai = c(
    insufficient_quota      = "billing",   # out of credit; arrives as a 429
    invalid_api_key         = "auth",
    model_not_found         = "param",
    context_length_exceeded = "param"
  ),
  anthropic = c(               # the `type` string inside `error`
    invalid_request_error = "param",
    authentication_error  = "auth",
    permission_error      = "auth",
    billing_error         = "billing",
    not_found_error       = "param",
    request_too_large     = "param",
    rate_limit_error      = "rate_limit",
    api_error             = "server",
    overloaded_error      = "server",
    timeout_error         = "server",
    conflict_error        = "server"
  ),
  gemini = c(                  # the `status` string of google.rpc.Status
    INVALID_ARGUMENT    = "param",
    FAILED_PRECONDITION = "billing",       # billing not enabled for the key
    PERMISSION_DENIED   = "auth",
    UNAUTHENTICATED     = "auth",
    NOT_FOUND           = "param",
    RESOURCE_EXHAUSTED  = "rate_limit",
    INTERNAL            = "server",
    UNAVAILABLE         = "server",
    DEADLINE_EXCEEDED   = "server"
  ),
  moonshot = c(                # three distinct causes share HTTP 429
    exceeded_current_quota_error = "billing",
    rate_limit_reached_error     = "rate_limit",
    engine_overloaded_error      = "server",
    invalid_authentication_error = "auth",
    permission_denied_error      = "auth",
    invalid_request_error        = "param"
  ),
  alibaba = c(                 # DashScope compatible mode
    Arrearage                   = "billing",
    "AccessDenied.Unpurchased"  = "billing",
    Throttling                  = "rate_limit",
    "Throttling.RateQuota"      = "rate_limit",
    "Throttling.AllocationQuota" = "rate_limit",
    insufficient_quota          = "rate_limit",  # TPM throttling, not credit
    InvalidApiKey               = "auth",
    invalid_api_key             = "auth",
    InvalidParameter            = "param",
    invalid_request_error       = "param",
    model_not_found             = "param"
  ),
  zhipu = c(                   # numeric business codes of the v4 API
    "1000" = "auth", "1001" = "auth", "1002" = "auth", "1003" = "auth",
    "1004" = "auth", "1112" = "auth",
    "1113" = "billing",        # insufficient balance or no resource package
    "1211" = "param",          # unknown model
    "1302" = "rate_limit"
  )
)

# Providers whose documented use of a status departs from the shared map.
.llmr_status_overrides <- list(
  together   = c("403" = "param"),   # prompt exceeds the context window
  openrouter = c("403" = "param")    # input flagged by moderation
)

# Pull reason, code string, and flagged parameter out of a parsed error
# body, tolerating every shape the supported providers send: an `error`
# object (OpenAI and compatibles, Anthropic, Gemini, Zhipu), top-level
# code/message pairs (DashScope), a top-level `detail` string (Voyage),
# and a bare string under `error` (Ollama).
.llmr_error_fields <- function(err, raw_tail = NULL) {
  fallback <- raw_tail %||% "No message supplied"
  if (is.null(err) || inherits(err, "try-error") || !is.list(err)) {
    return(list(reason = fallback, code = NA_character_, param = NA_character_))
  }
  e <- err$error
  if (is.character(e) && length(e) == 1L) {
    return(list(reason = e, code = NA_character_, param = NA_character_))
  }
  reason <- err$error$message %||% err$message %||% err$detail %||%
    err$error$type %||% err$error$code %||% fallback
  # Prefer a named code string over a numeric echo of the HTTP status
  # (Gemini bodies carry both: code = 429, status = "RESOURCE_EXHAUSTED").
  cands <- list(err$error$code, err$error$type, err$error$status,
                err$code, err$type)
  cands <- Filter(function(x) length(x) == 1L && !is.na(x) && nzchar(as.character(x)), cands)
  code <- NA_character_
  for (x in cands) {
    xs <- as.character(x)
    if (!grepl("^[0-9]+$", xs)) { code <- xs; break }
  }
  if (is.na(code) && length(cands)) code <- as.character(cands[[1]])
  param <- err$error$param %||% err$param %||% NA_character_
  list(reason  = as.character(reason)[1],
       code    = code,
       param   = as.character(param)[1])
}

# Classify one failed response. Precedence: shared status map, then the
# provider's documented status overrides, then its documented code strings,
# then a wording check that catches spend caps delivered under generic
# statuses (Anthropic sends its usage-limit refusal as a 400
# invalid_request_error). The wording check never touches a 429: at several
# providers quota wording on a 429 describes recoverable throttling
# (Gemini's per-minute quotas), which retrying handles.
.llmr_classify_error <- function(provider, status, code = NULL, message = "") {
  category <- .llmr_status_category(status)
  p <- if (length(provider) == 1L && !is.na(provider)) tolower(provider) else ""
  ov <- .llmr_status_overrides[[p]]
  if (!is.null(ov)) {
    hit <- ov[as.character(as.integer(status[1]))]
    if (length(hit) == 1L && !is.na(hit)) category <- unname(hit)
  }
  map <- .llmr_error_catalog[[p]]
  cd <- if (length(code) == 1L && !is.na(code) && nzchar(code)) code else NULL
  if (!is.null(map) && !is.null(cd) && cd %in% names(map)) {
    category <- unname(map[[cd]])
  }
  s <- suppressWarnings(as.integer(status[1]))
  if (category %in% c("param", "auth", "unknown") &&
      !identical(s, 429L) &&
      grepl("usage limit|quota|billing|credit|insufficient balance|arrear|out of balance|top up",
            message, ignore.case = TRUE)) {
    category <- "billing"
  }
  category
}
