# Create an LLM configuration (provider-agnostic)

`llm_config()` builds a provider-agnostic configuration object that
[`call_llm()`](https://asanaei.github.io/LLMR/reference/call_llm.md)
(and friends) understand. You can pass provider-specific parameters via
`...`; LLMR forwards them as-is, with a few safe conveniences.

## Usage

``` r
llm_config(
  provider,
  model,
  api_key = NULL,
  troubleshooting = FALSE,
  base_url = NULL,
  embedding = NULL,
  no_change = FALSE,
  ...
)
```

## Arguments

- provider:

  Character scalar. One of: `"openai"`, `"anthropic"`, `"gemini"`,
  `"groq"`, `"together"`, `"voyage"` (embeddings only), `"deepseek"`,
  `"xai"`, `"ollama"`.

- model:

  Character scalar. Model name understood by the chosen provider. (e.g.,
  `"gpt-4o-mini"`, `"o4-mini"`, `"claude-3.7"`, `"gemini-2.0-flash"`,
  etc.)

- api_key:

  Character scalar. Provider API key.

- troubleshooting:

  Logical. If `TRUE`, prints the full request payloads (including your
  API key!) for debugging. **Use with extreme caution.**

- base_url:

  Optional character. Back-compat alias; if supplied it is stored as
  `api_url` in `model_params` and overrides the default endpoint.

- embedding:

  `NULL` (default), `TRUE`, or `FALSE`. If `TRUE`, the call is routed to
  the provider's embeddings API; if `FALSE`, to the chat API. If `NULL`,
  LLMR infers embeddings when `model` contains `"embedding"`.

- no_change:

  Logical. If `TRUE`, LLMR **never** auto-renames/adjusts provider
  parameters. If `FALSE` (default), well-known compatibility shims may
  apply (e.g., renaming OpenAI's `max_tokens` → `max_completion_tokens`
  after a server hint; see
  [`call_llm()`](https://asanaei.github.io/LLMR/reference/call_llm.md)
  notes).

- ...:

  Additional provider-specific parameters (e.g., `temperature`, `top_p`,
  `max_tokens`, `top_k`, `repetition_penalty`, `reasoning_effort`,
  `api_url`, etc.). Values are forwarded verbatim unless documented
  shims apply. For Anthropic extended thinking, supply `thinking_budget`
  (canonical; mapped to `thinking.budget_tokens`) together with
  `include_thoughts = TRUE` to request the thinking block in the
  response.

## Value

An object of class `c("llm_config", provider)`. Fields: `provider`,
`model`, `api_key`, `troubleshooting`, `embedding`, `no_change`, and
`model_params` (a named list of extras).

## Temperature range clamping

Anthropic temperatures must be in `[0, 1]`; others in `[0, 2]`.
Out-of-range values are clamped with a warning.

## Endpoint overrides

You can pass `api_url` (or `base_url=` alias) in `...` to point to
gateways or compatible proxies.

## See also

[`call_llm`](https://asanaei.github.io/LLMR/reference/call_llm.md),
[`call_llm_robust`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md),
[`llm_chat_session`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md),
[`call_llm_par`](https://asanaei.github.io/LLMR/reference/call_llm_par.md),
[`get_batched_embeddings`](https://asanaei.github.io/LLMR/reference/get_batched_embeddings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic OpenAI config
cfg <- llm_config("openai", "gpt-4o-mini",
temperature = 0.7, max_tokens = 300)

# Generative call returns an llmr_response object
r <- call_llm(cfg, "Say hello in Greek.")
print(r)
as.character(r)

# Embeddings (inferred from the model name)
e_cfg <- llm_config("gemini", "text-embedding-004")

# Force embeddings even if model name does not contain "embedding"
e_cfg2 <- llm_config("voyage", "voyage-large-2", embedding = TRUE)
} # }
```
