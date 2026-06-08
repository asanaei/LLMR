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
