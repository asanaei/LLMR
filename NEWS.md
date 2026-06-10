# LLMR 0.8.3

## New features

- **Provider batch APIs.** `llm_batch_submit()`, `llm_batch_status()`,
  `llm_batch_fetch()`, and `llm_batch_cancel()` drive the asynchronous batch
  endpoints of OpenAI, Groq, Anthropic, and Gemini, which price requests at
  roughly half the live rate. Jobs are plain R objects without secrets (keys
  stay environment references), so a job can be saved with `state_path=`,
  the session closed, and results fetched later or from another machine.
- **`llm_hash()`: one content-hash convention for the ecosystem.**
  Canonical form (classes stripped, named lists sorted, functions deparsed),
  canonical JSON, SHA-256 over the UTF-8 bytes -- independent of R's
  serialization format and of construction order. Downstream packages
  (locked coding protocols, replication archives) use these hashes as
  identifiers of record.
- **Audit log for reproducible research.** Each record carries a
  `schema_version` field (currently `"1.0"`) so downstream tools can parse
  the log against a stable contract. `llm_log_enable(path)` appends one
  JSON record per API call (timestamp, provider, model, the served
  `model_version`, full request parameters and messages, reply text, token
  usage including cached tokens, request id, status, timing) to a JSONL file;
  `llm_log_disable()` and `llm_log_status()` manage it. Failed calls are
  logged too. `include_messages = FALSE` keeps a metadata-only trail for
  confidential prompts.
- **A draft methods paragraph.** `llm_methods_text()` turns a result frame
  into the transparency paragraph journals increasingly require: models,
  providers, call counts, recorded inference settings, token totals, and
  failure counts, stated only as far as the data supports.
- **Replication and reliability.** `llm_replicate()` runs every row `.times`
  times through the parallel engine; `llm_agreement()` reports per-unit
  majority labels and overall reliability (mean pairwise agreement and
  Krippendorff's alpha for nominal data, missing-safe).
- **Native tool calling with an execution loop.** `llm_tool()` wraps an R
  function with a JSON-Schema argument spec; `call_llm_tools()` sends the
  definitions, executes the calls the model makes, feeds results back, and
  returns the final response with a `tool_history` attribute.
  `tool_calls()` extracts requested calls from any response. Supported on
  OpenAI-compatible providers and Anthropic.
- **Streaming.** `call_llm_stream()` delivers the reply chunk by chunk
  (callback) for OpenAI-compatible providers, Anthropic, and Gemini, and
  returns a normal `llmr_response` at the end, with usage when the provider
  reports it in the stream.
- **Token log-probabilities.** Pass `logprobs = TRUE` (and `top_logprobs = k`)
  in `llm_config()`; `llm_logprobs()` returns them tidily (OpenAI-compatible
  providers and Gemini) for confidence scoring and calibration.
- **Reproducibility knobs.** A canonical `seed` parameter is forwarded where
  supported; every response now records `model_version` (the identifier the
  server reports having served) and, when returned separately, the model's
  reasoning text in a `thinking` field.
- **Prompt caching.** `cache = TRUE` marks the system prompt and tools as
  cacheable for Anthropic; cached prompt tokens are now extracted from all
  providers that report them (`tokens(x)$cached`, `cached_tokens` /
  `*_cached` diagnostic columns) and counted by `llm_usage()`.
- **Cost estimates on your own prices.** `llm_usage(price_table = ...)`
  accepts a user-supplied table (per-million-token prices, optional cached
  rate) and adds a `cost_estimate`; LLMR still ships no price list, on
  purpose.
- **NA policy for templates.** `llm_fn()`/`llm_mutate()` gain
  `.na_action = c("send", "skip", "error")`; `llm_preview()` flags rows whose
  templates reference `NA` or render empty prompts.
- **Quality-of-life.** `chat_session()` gains `quiet=` and multimodal sends
  (named-vector shortcut); a one-line summary message after runs with
  failures or truncations points to `llm_failures()`; `print(llm_config)` is
  masked and informative; `options(llmr.quiet = TRUE)` silences advisory
  notes; every request carries a total timeout (`timeout=` per config or
  `options(llmr.timeout=)`, default 600 s).

## Provider-path overhaul

- The nine OpenAI-compatible providers (groq, together, deepseek, xiaomi,
  alibaba, zhipu, moonshot, xai, ollama) now share one request builder with
  the full feature set: multimodal file parts (previously serialized raw,
  with the local path leaked to the provider), the complete canonical
  parameter set, structured output, tools, verbatim extras passthrough, and
  the modifiability hooks. Parameters a provider rejects are dropped with a
  console note instead of silently.
- The `req_builder`, `response_modifier`, and `request_modifier` hooks now
  apply on every provider path (previously only OpenAI) and are documented
  in `llm_config()`.
- Gemini: `thinking_budget` / `include_thoughts` are finally sent
  (`generationConfig.thinkingConfig`); presence/frequency penalties, `seed`,
  and logprobs are translated rather than dropped; `enable_structured_output()`
  sends the schema by default via `responseJsonSchema` (set
  `gemini_enable_response_schema = FALSE` for old models); embeddings use
  `batchEmbedContents` (one HTTP call per up to 100 texts instead of one per
  text) and go through the standard error handling; Gemini thought parts are
  kept out of the answer text and surfaced as `thinking`.
- Anthropic: `top_k` is supported again (it was wrongly dropped as
  "unsupported"); `no_change` is honored; PDFs are sent as `document` blocks
  (previously mislabeled as images); an invalid `thinking_budget >=
  max_tokens` combination warns before the call; unknown typed content
  blocks pass through, enabling the tool loop; the false claim that LLMR
  "sets the beta header automatically" is gone from the docs
  (`anthropic_beta = ...` sends one).
- OpenAI: `max_completion_tokens` is honored directly (reasoning models no
  longer pay a guaranteed failed round trip); the Responses-API autodetect
  recognizes `gpt-5-pro` and the deep-research models and no longer matches
  nonexistent ones; the Responses path drops the parameters that API rejects
  (penalties, logprobs, seed) with a note, maps `reasoning_effort` to its
  nested shape, and no longer duplicates system text when all messages are
  system.
- Embedding routing now follows the documented inference (`embedding = NULL`
  + "embedding" in the model name) for every provider, and embedding
  endpoints no longer receive chat-only parameters from a reused config.
  `get_batched_embeddings()` gains retry controls and its `verbose` default
  is documented correctly.

## Reliability fixes

- The retry helper no longer sleeps after the final failed attempt
  (previously up to ~104 minutes wasted on the default schedule); waits
  honor `Retry-After` when a 429 provides one, and add jitter so parallel
  workers do not retry in lockstep.
- Retry classification is exact for typed errors: rate limits and server
  errors (now including 403->auth, 408->retryable) retry; parameter,
  authentication, and quota errors fail fast. The over-broad `"exceeded"`
  message pattern (which retried `context_length_exceeded` for the full
  schedule) is gone.
- `call_llm_robust()` defaults are humane (`wait_seconds = 2`,
  `backoff_factor = 3`) and consistent with `call_llm_par()`;
  `start_jitter` in the parallel engine defaults to 0 (it silently added up
  to 5 s per row).
- Failed rows in `call_llm_par()` results now carry the provider's raw error
  body in `raw_response_json`.

## Bug fixes

- **Gemini multi-turn roles**: assistant turns are now sent as Gemini's
  `"model"` role. Previously every turn was sent as `"user"`, so the model
  saw its own prior replies as user messages, corrupting chat sessions and
  any multi-turn agent memory on Gemini.
- **`extract_text()` (Anthropic)**: responses with several text blocks
  (typical around tool use) now concatenate all of them; previously only the
  last block was returned and earlier content was silently dropped.
- **`llm_fn_structured()` / `llm_mutate_structured()`**: with `.schema =
  NULL` these now request JSON-object mode as documented; previously the
  config was sent unchanged and the model could return arbitrary prose.
- **`call_llm_tools()`**: the return value now carries
  `attr(x, "tool_loop")` with `model_calls`, aggregate `sent`/`rec` token
  totals across every internal round, and `tool_calls` -- `tokens(x)` alone
  covers only the final call, which undercounted multi-round loops. A new
  `max_tool_calls` argument caps tool executions across the loop, raising a
  typed `llmr_tool_limit` condition instead of continuing to spend.
- **`call_llm.openai()`**: `top_k` and `repetition_penalty` are now dropped
  (with the usual one-time note) instead of being forwarded to an endpoint
  that rejects them; the streaming and batch paths already did this.
- **Anthropic `stop_sequence`** now maps to finish reason `"stop"` rather
  than `"other"`.
- **Templated `.messages` with partially named vectors** (e.g.
  `c(system = ..., "{x}")`) now default unnamed elements to the user role,
  as documented, instead of erroring.
- **`call_llm_broadcast()` with zero messages** returns the full diagnostic
  column schema (finish reasons, token columns, `response`), matching
  non-empty results.
- **`llm_batch_submit()`**: a named character vector like
  `c(system = "...", user = "...")` is treated as a single multi-role request
  rather than being split into separate batch requests; unnamed character
  vectors still expand one element per request.
- **`llm_batch_fetch()` / `call_llm_stream()`**: total token count is `NA`
  when both sent and received counts are unknown, consistent with the
  `call_llm()` path, instead of a false 0.
- **`chat_session()`**: rejects embedding configs inferred from model names
  (e.g. `"gemini-embedding-001"`) in addition to those with `embedding = TRUE`
  set explicitly.
- **`expand_llm_config()`**: sweeping `provider` now updates the S3 class, so
  the swept config dispatches to the right API (previously every swept
  provider was called through the original provider's path).
- **`call_llm_par_structured()` / `call_llm_par_tags()`**: prompts containing
  literal braces (typical for structured-output instructions) no longer
  abort the run; strings glue cannot parse pass through verbatim.
- **Key handling**: an empty-string `api_key` (what `Sys.getenv()` returns
  for an unset variable) falls back to the provider's default variables with
  a warning instead of sending an empty Bearer header; `NA` keys are
  rejected; a vector `api_key` works as ordered fallbacks; legacy configs
  holding a literal key string resolve correctly and error messages never
  echo a key.
- **Chat sessions**: the NA-token fix now also covers `send_structured()` and
  `send_tags()` (one usage-less response could previously poison the running
  totals); multi-part sends are no longer truncated to their first element.
- **Column safety**: `llm_mutate()` replaces an existing output column with a
  notice (mutate semantics) instead of letting `bind_cols()` mangle both
  names; hoisted structured/tag fields never silently overwrite existing
  columns (suffix + warning); the parallel engine's collision warning fires
  regardless of `verbose`; `summary.llmr_experiment()` follows
  collision-renamed columns.
- **Row batching**: user tags shaped like `row_2` are rejected in batched tag
  mode (they would scramble the demultiplexer); assistant turns error
  instead of being silently dropped; `duration` is attributed once per batch
  so `llm_usage()` stops overcounting wall time; fully-failed batch calls
  attribute their token spend to the first failed row; the protocol
  instructions state the actual item count.
- **Structured output**: JSON recovery is string-aware (braces inside string
  values no longer corrupt extraction); `disable_structured_output()`
  removes a custom-named schema tool; `llm_mutate_structured()` validates
  locally like `llm_fn_structured()` (new `.validate_local`).
  `enable_structured_output()` now knows which providers reject a server-side
  `json_schema` (DeepSeek, Alibaba, Zhipu, Moonshot, Xiaomi) and requests
  JSON-object mode with local validation there, instead of a guaranteed
  HTTP 400. In strict mode the schema sent to the provider is hardened the
  way the OpenAI protocol formally requires (`additionalProperties: false`
  and all properties required, filled in only where unspecified), so plain
  schemas work on OpenAI and Groq without boilerplate.
- **`llm_par_resume()`** works on `call_llm_sweep()` / `call_llm_broadcast()`
  / `call_llm_compare()` results (they now keep the `config` list-column) and
  refreshes `structured_ok` / `structured_data` for re-run rows.
- **Responses**: OpenAI-style refusals surface their text and map to
  `finish_reason = "filter"` (as do Gemini safety verdicts such as
  RECITATION); `llm_judge()` gains `.output=` and refuses to clobber a
  `.target` column.
- Assorted: `call_llm()` troubleshooting output prints real newlines; dead
  code removed (`parse_embeddings()` no-op branch, unreachable per-provider
  default models); embedding example code no longer uses the discouraged
  `Sys.getenv()` key pattern.

## Documentation

- New vignette "Interactive calls: tools, streaming, and logprobs": the tool
  loop end to end (definitions, history, aggregate spend, `max_tool_calls`),
  streaming with custom callbacks, and log-probabilities as graded
  measurements, with honest notes on provider support.
- `llm_config()` now documents the full canonical parameter set (including
  `seed`, `logprobs`, `thinking_budget`, `timeout`, `cache`) and the three
  request hooks. The Anthropic thinking example is valid
  (`max_tokens > thinking_budget`). `build_factorial_experiments()` documents
  that system prompts are crossed, not recycled. Vignettes updated
  accordingly, plus a new article on reproducibility and cost.
- Live tests and examples run on inexpensive open-weight models (Groq
  `openai/gpt-oss-20b`, DeepSeek, Moonshot, Qwen, Gemini Flash-Lite).

---

# LLMR 0.8.0

## New features

- **Preview a call before spending anything.** `llm_preview()` renders exactly
  what `llm_fn()` / `llm_mutate()` would send (using the same internal renderer,
  so it can never drift from the real path) without making any API call or
  reading/encoding files. It returns a row-level tibble with the rendered
  messages, roles, character counts, file presence/existence, the batch plan
  (`batch_id` / `batch_size` / `batch_row`), and an `issues` list-column that
  flags problems up front: missing files, `"file"` content combined with
  `.batch_size > 1`, an embedding config with row batching, `.return = "object"`
  with batching, or a schema supplied without `.structured`. `llm_render_messages()`
  exposes just the rendered message objects.
- **Summarize a finished run.** `llm_usage()` reports outcome counts and token
  totals (sent / received / total / reasoning) plus truncation and filter
  counts, reading the diagnostic columns that `call_llm_par()` and `llm_mutate()`
  already produce. It works on both result shapes and sums tokens with
  `na.rm = TRUE`, which is correct under row batching. It reports **tokens, not
  money**: no dollar figures and no built-in price table (which would go stale).
- **Find and re-run failures.** `llm_failures()` lists exactly which rows failed
  or were truncated/filtered, with `status_code`, `error_code`, `bad_param`,
  and `error_message`. For a `call_llm_par()` result, pass the original frame to
  the existing `llm_par_resume()` to re-run only those rows.

## Internal

- The per-row message rendering used by `llm_fn()`, `llm_mutate()`, and
  `llm_preview()` is now a single shared internal helper, locked by golden tests
  so its output stays byte-identical to previous releases.

## Bug fixes

- **`.fields = FALSE`** now correctly skips field extraction in structured/JSON
  mode (keeping only the `structured_data` list-column), matching tag mode.
  Previously the logical `FALSE` was treated as a one-element field name.
- **Missing token usage is reported as `NA`, not `0`.** When a provider returns
  no usage metadata, chat sessions and responses no longer record the call as
  having used zero tokens; running chat totals add `NA` as `0` so one unknown
  response cannot poison the cumulative count.
- **`llm_usage()` / `llm_failures()` / `llm_par_resume()`** handle the case where
  `call_llm_par()` collision-renamed its output columns (because the input frame
  already had a column named `success`, etc.): the summaries follow the renamed
  columns, and `llm_par_resume()` raises a clear, actionable error.
- **Bare environment-variable API keys.** `api_key = "OPENAI_API_KEY"` is now
  always treated as an environment-variable reference, even when that variable is
  not yet set (it then fails with a clear "missing env var" message at call time
  instead of silently sending the literal name as the key), matching the
  documented behavior.
- **`llm_api_key_env(required = FALSE)`** is now honored: a missing variable
  yields an empty key instead of an authentication error.
- **`llm_parse_structured_col()`** now returns a tibble on every path, including
  when the structured column is absent.
- **`llm_usage()`** gains an `n_unknown_tokens` count so an all-`NA` token column
  (a provider that reports no usage) is no longer indistinguishable from a true
  zero. The token sums still use `na.rm = TRUE`, which is correct for batching.

## Documentation

- `llm_api_key_env()` is now exported and documented. The help also notes that
  the simplest approach is to set the standard `<PROVIDER>_API_KEY` variable and
  pass no key at all.
- New **"LLMR in 5 minutes"** quickstart vignette: install, set a key, a first
  `call_llm()`, a generative `llm_fn()` over a vector, a data-frame `llm_mutate()`,
  and tagged + batched extraction, all on the open-weight `gpt-oss-20b` so the
  examples are runnable for everyone. The structured-output articles now lead
  with a concrete example before the provider-by-provider details.
- The troubleshooting help no longer claims the API key is printed (it is
  masked). The embeddings vignette can now be enabled with
  `LLMR_RUN_VIGNETTES=true` (its run flag was previously hard-coded off), and its
  stale prebuilt HTML was removed.
- Fixed a vignette that referenced a non-existent function and assorted stale
  version labels; removed non-ASCII look-alike punctuation from R sources.

---

# LLMR 0.7.2

## Bug fixes

- **Retry/error classification.** API error messages containing curly braces
  (common when providers echo JSON fragments) no longer break error
  construction: the typed condition class, `status_code`, and provider message
  are preserved, so retryable rate-limit/server errors are retried correctly
  instead of being misclassified. The retry helper also re-raises the original
  typed error after exhausting attempts, and its wait-time message no longer
  errors on fractional backoff values.
- **`llm_par_resume()`** now re-runs only the failed/`NA` rows instead of every
  row (it previously collapsed the per-row success vector with `isTRUE()`).
- **Parallel tag/structured helpers.** Unnamed (bare-prompt) messages passed to
  `call_llm_par_tags()` / `call_llm_par_structured()` no longer have their
  template text used as the message role.
- **JSON recovery.** A top-level JSON array embedded in prose is now extracted in
  full (a regex character-class bug previously truncated it to the first object).
- **Embeddings.** `get_batched_embeddings()` no longer locks in a wrong vector
  dimension when the first batch returns empty, preserving the one-row-per-input
  contract.
- **Gemini token counts.** Usage is preserved when `candidatesTokenCount` is
  absent (e.g. reasoning models with no visible output).
- **Printing.** `print()` on an `llmr_response` with a non-standard finish reason
  no longer drops the status line.

---

# LLMR 0.7.1

## New features

- **Row batching for generative calls.** `llm_fn()` and `llm_mutate()` (and the
  `_tags` / `_structured` variants) gain `.batch_size`, `.batch_payload`, and
  `.batch_recovery`. With `.batch_size > 1`, several rows are packed into one
  request wrapped in numbered `<row_1>...</row_1>` tags and de-multiplexed back
  into rows, with fault-tolerant recovery (split-and-retry down to single rows)
  for dropped, reordered, or truncated rows. Composes with `.tags` (one level of
  required nesting) and with structured/JSON output (`{"results":[{"row":i,
  ...}]}`). The default `.batch_size = 1` reproduces the previous one-call-per-row
  behaviour exactly. New exported helper `llm_parse_batch_tags()`.

---

# LLMR 0.7.0

## New features

- **Soft structured output via XML-like tags.** `llm_mutate()` gains `.tags`,
  backed by `llm_mutate_tags()`, `llm_fn_tags()`, `llm_parse_tags()`,
  `llm_parse_tags_col()`, and `call_llm_par_tags()`.
- **Four new providers:** Xiaomi MiMo, Alibaba (Qwen), Zhipu (GLM), and Moonshot
  (Kimi). All use OpenAI-compatible structured output.
- **Gemini Vertex AI** supported via `llm_config("gemini", ..., vertex = TRUE)`.
- **Multi-variable API key fallback.** Providers can declare multiple
  environment variable names (e.g., `XIAOMI_KEY` or `XIAOMI_API_KEY`); the first
  one found is used.

## Bug fixes

- Fixed API key resolution for providers with multiple fallback env vars.
- Removed dead `requireNamespace("LLMR")` guard inside `llm_mutate()`.

---

# LLMR 0.6.3

- Added Ollama provider (local generative and embedding models).
- Stable column names (`v1`...`vN`) in `get_batched_embeddings()`.

# LLMR 0.6.2

- `llm_mutate()` shorthand: `llm_mutate(answer = "{question}", .config = cfg)`.
- `.structured = TRUE` flag in `llm_mutate()` for inline JSON parsing.
- `setup_llm_parallel()` accepts a positional numeric `workers` argument.

# LLMR 0.6.1

- Fixed a bug that affected Anthropic calls.

# LLMR 0.6.0

- `call_llm()` now returns an `llmr_response` object. Use `as.character(x)` for
  plain text. Legacy `json=` arguments are removed.
- Secure API key handling: literal keys are moved to temporary env vars.
- Structured JSON output and schema validation.
- Multi-column injection in `llm_mutate()`.
