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

## Documentation

- `llm_api_key_env()` is now exported and documented. The help also notes that
  the simplest approach is to set the standard `<PROVIDER>_API_KEY` variable and
  pass no key at all.
- New **"LLMR in 5 minutes"** quickstart vignette: install, set a key, first
  call, first `llm_mutate()`. The structured-output articles now lead with a
  concrete example before the provider-by-provider details.
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
