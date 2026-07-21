# LLMR 0.8.11

* Fast-follow corrections cover attachment-aware request hashes, embedding
  error propagation, Anthropic thinking budgets, ordered agreement labels,
  audit-log destinations, tool-loop provenance, and experiment reporting.
  The inert structured-output `method` argument is gone, and ten non-core helpers
  are now internal.
* New provider `"openrouter"`: chat, streaming, and structured output through
  OpenRouter's OpenAI-compatible aggregator (model ids like
  `"openai/gpt-4o-mini"`; key from `OPENROUTER_API_KEY`). OpenRouter has no
  embeddings or batch API; its optional attribution headers can be set through
  the `req_builder` hook.

# LLMR 0.8.10

A bug-fix release; all changes are backward compatible.

* `llm_hash()` no longer depends on the collation locale, so the same object
  hashes identically on every machine; hashes recorded under the C locale are
  unchanged.
* Constant prompts (no `{column}` reference) now fill every row; previously
  rows 2..n were sent with `NA` content and rejected by providers.
* `llm_mutate()` appends its generated columns again (instead of moving them to
  the front), and `.before`/`.after` now work on the embedding, structured,
  tag, and row-batched paths.
* `llm_usage()` and `llm_failures()` work on `llm_mutate_structured()` and
  `llm_mutate_tags()` results; config-side and log-side request hashes agree
  after provider parameter renames.
* Streaming no longer errors for providers outside the built-in table and now
  applies the `req_builder` hook and the request timeout. Assorted smaller
  fixes; see the commit log.

# LLMR 0.8.9

* Additive helpers for the agent layer: `call_llm_par(.request_hash =)`,
  `llm_add_request_hash()`, `llm_log_active()`, `llm_tool_signature()`, and
  `llm_uuid()`.
* `llm_agreement()` gains ordinal and interval Krippendorff's alpha
  (`metric =`); the nominal default is unchanged.

# LLMR 0.8.8

* Bundled example data `anes_2024_personas`: 100 participant profiles derived
  from the ANES 2024 public release, the shared persona dataset of the LLMR
  family.
* Persona-frame contract helpers: `llm_persona_split()`,
  `llm_persona_overview()`, `llm_persona_dictionary()`,
  `llm_persona_demographic_fields()`, `llm_validate_persona_frame()`.

# LLMR 0.8.7

* `transcript_as_messages()` and `ensure_alternating_messages()`: build a
  provider-safe, role-flipped message array from a multi-speaker transcript
  (own turns become `assistant`, others become labeled `user` turns).

# LLMR 0.8.6

* Row-packing controls renamed (`.batch_size` -> `.rows_per_prompt`, and kin);
  the word "batch" is now reserved for the asynchronous provider Batch API.
* `llm_log_read()`: parse a JSONL audit log into records plus a per-record
  manifest with record and request hashes.
* `llm_request_hash()` canonicalizes message shape and keys on all generation
  parameters, so a logged call and its config hash identically.
* Added the shared `reset()` generic.

# LLMR 0.8.5

* Provenance helpers: `llm_response_record()` (one-row response contract; a
  failed call is a row, never a dropped call) and `llm_request_hash()` (stable
  identity for a call).

# LLMR 0.8.4

* Added the shared S3 generics `diagnostics()` and `report()`, implemented by
  the LLMR method packages.

# LLMR 0.8.3

* Provider Batch APIs at roughly half price: `llm_batch_submit()`,
  `llm_batch_status()`, `llm_batch_fetch()`, `llm_batch_cancel()` (OpenAI,
  Groq, Anthropic, Gemini).
* Audit log for reproducible research (`llm_log_enable()`), a draft methods
  paragraph (`llm_methods_text()`), and replication with agreement statistics
  (`llm_replicate()`, `llm_agreement()`).
* Native tool calling (`llm_tool()`, `call_llm_tools()`) and streaming
  (`call_llm_stream()`).
* Token log-probabilities (`llm_logprobs()`), canonical `seed`,
  `model_version` and `thinking` on every response, prompt caching, and cost
  estimates from a user-supplied `price_table`.
* One shared request builder for the nine OpenAI-compatible providers, with
  full multimodal, structured-output, tools, and hooks parity.

# LLMR 0.8.0

* `llm_preview()`: render exactly what a call would send, and flag problems,
  without any API call.
* `llm_usage()` and `llm_failures()`: outcome counts, token totals, and
  per-row failure listings for any result frame.
* Missing token usage is reported as `NA`, not `0`; a bare environment-variable
  name passed as `api_key` is always treated as a reference.
* New "LLMR in 5 minutes" quickstart vignette, runnable on an open-weight
  model.

# LLMR 0.7.2

* Retry and error-classification fixes (provider errors containing braces,
  typed re-raise after exhausted retries).
* `llm_par_resume()` re-runs only the failed rows; JSON array recovery and
  embedding-dimension fixes.

# LLMR 0.7.1

* Row batching for generative calls: pack several rows into one request, with
  fault-tolerant recovery for dropped, reordered, or truncated rows.

# LLMR 0.7.0

* Soft structured output via XML-like tags: `.tags` on `llm_mutate()`, plus
  `llm_mutate_tags()`, `llm_fn_tags()`, and the tag parsers.
* Four new providers (Xiaomi, Alibaba/Qwen, Zhipu, Moonshot) and Gemini on
  Vertex AI.

# LLMR 0.6.3

* Ollama provider (local generative and embedding models); stable embedding
  column names.

# LLMR 0.6.2

* `llm_mutate()` shorthand (`answer = "{question}"`) and `.structured = TRUE`.

# LLMR 0.6.1

* Fixed a bug affecting Anthropic calls.

# LLMR 0.6.0

* `call_llm()` returns an `llmr_response` object; `as.character(x)` extracts
  the text.
* Secure API-key handling, structured JSON output, and multi-column injection
  in `llm_mutate()`.
