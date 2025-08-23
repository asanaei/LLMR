# LLMR 0.6.0

## Breaking changes

* `build_factorial_experiments()` API revised:

  * `messages` → `user_prompts` and optional `system_prompts`.
  * New labels: `user_prompt_labels`, `system_prompt_labels`. Output columns now include these labels.
  * `.compose_msg()` now ignores missing/`NA` system prompts; it returns user-only messages. Safer defaults, different behavior.
* `call_llm()` family expanded and standardized. Requests and returns are now provider-normalized; errors are typed. Some edge behaviors differ from older ad-hoc calls.
* Anthropic calls require `max_tokens`. If absent, the code supplies `2048` with a warning. This is stricter than before.

## New

* Structured JSON mode (provider-agnostic):

  * `enforce_json_mode()` sets JSON-strict output across OpenAI-compatible, Anthropic, and Gemini.
  * `llm_parse_json()` robustly extracts the largest balanced JSON object when extra text appears.
  * `llm_parse_json_col()` parses a JSON column, adds `json_ok` and `json_data`, and hoists typed fields.
  * JSON-first helpers: `llm_fn_json()`, `llm_mutate_json()`, `call_llm_par_json()`.
* Response object:

  * `llmr_response` S3 class with `print()`, `as.character()`, `finish_reason()`, `tokens()`, `is_truncated()`.
* Robust calling and caching:

  * `.llmr_error()` typed errors: `param`, `auth`, `rate_limit`, `server`, `unknown`.
  * `call_llm_robust()` with exponential backoff on rate limits; optional memoization via `cache_llm_call()`.
  * `log_llm_error()` for timestamped diagnostics.
* Provider coverage and multimodal:

  * Generative: `openai`, `anthropic`, `gemini`, `groq`, `together`, `deepseek`, `xai`.
  * Embeddings: `openai`, `gemini`, `voyage`, `together`.
  * Multimodal shortcut: named `"file"` parts inline as base64 images (OpenAI/Anthropic) or `inlineData` (Gemini).
* Embedding utilities:

  * `parse_embeddings()` → numeric matrix.
  * `get_batched_embeddings()` with batching, retries, stable row count, and `NA` fill for failures.
* Parallel and experiments:

  * `call_llm_par()` core engine returns rich diagnostics: `finish_reason`, `{sent,rec,total,reasoning}_tokens`, `response_id`, `duration`, `status_code`, `error_code`, `bad_param`, plus `response` objects.
  * `setup_llm_parallel()` and `reset_llm_parallel()` convenience functions.
  * `call_llm_sweep()`, `call_llm_broadcast()`, `call_llm_compare()` updated to the new engine.
* Parameter translation:

  * `_translate_params()` maps canonical names to provider-native keys and drops unsupported ones (e.g., for Gemini and Anthropic).
* Chat session improvements:

  * `chat_session()` prints a concise status line with model, finish reason, and tokens. Token accounting is provider-agnostic and includes Gemini `usageMetadata`.
* Misc:

  * `globalVariables()` now includes `":="` and new label symbols. Imports add `rlang::\`:=\``and`vctrs\` helpers.

## Enhancements

* OpenAI-compatible calls:

  * If server flags `max_tokens` as invalid, the code retries once using `max_completion_tokens` unless `no_change=TRUE`.
  * Structured outputs via `response_format` including JSON Schema with `strict`.
* Anthropic:

  * Structured outputs via single-tool schema injection with `tool_choice`.
  * Optional thinking controls: `thinking_budget`, `include_thoughts` (beta header wired).
* Gemini:

  * Uses `generationConfig` with camelCase fields. Supports `responseMimeType` and optional `responseSchema` in JSON mode.
* Message normalization:

  * A single path normalizes character, named-vector, and list messages, including multimodal blocks. Fewer surprises.
* `llm_mutate()`:

  * Returns typed, per-column diagnostics: `<name>_finish`, `_sent`, `_rec`, `_tot`, `_reason`, `_ok`, `_err`, `_id`, `_status`, `_ecode`, `_param`, `_t`.
  * Embedding mode preserves row order and fills with dimension-consistent `NA`.


## Fixes

* Safer handling of empty/`NA` system prompts in experiment builders.
* More tolerant token-usage extraction across providers.
* HTTP error bodies are captured before throwing, enabling informative errors.

### Deprecations

* The old `json_output` parameter in parallel helpers is ignored; raw JSON is always in `raw_response_json`.
* Legacy paths that inferred messages from `messages=` in factorial designs are replaced by explicit user/system prompt inputs.


### Notes

* When expecting structured output, prefer:

  * `cfg <- enforce_json_mode(cfg, schema = <JSON Schema>)`
  * Use `llm_fn_json()` / `llm_mutate_json()` and hoist fields via `.fields`.
* If you passed `max_tokens` to OpenAI-compatible APIs and saw 400s, keep using `max_tokens`; the shim retries with `max_completion_tokens` automatically unless `no_change=TRUE`.
* For Anthropic, always set `max_tokens` explicitly to avoid the defaulted 2048 and warning.
* For multimodal content, use named vectors or message lists with `"file"` parts; text must precede files within a user block.
* Downstream code that assumed plain character returns should accept `llmr_response` as `json=TRUE` paths are now common. Use `as.character()` when needed.

