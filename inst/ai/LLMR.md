---
name: llmr
description: Unified LLM provider layer for R - one config across OpenAI/Anthropic/Gemini/Groq/DeepSeek/Together/Ollama and more; structured output, tool calling, streaming, logprobs, embeddings, tidy data-frame verbs, parallel and batch execution, audit logging, replication and agreement, cost accounting.
---

# LLMR — usage capsule for AI assistants

This capsule summarizes how to use the package correctly. For more detail, see
the vignettes `quickstart`, `tidy-and-structured`, `about-schema`,
`interactive-calls`, `reproducibility-and-cost`, and `experiments`.

## Install and keys

```r
install.packages("LLMR")    # CRAN; dev: remotes::install_github("asanaei/LLMR")
```

Keys come from environment variables (`GROQ_API_KEY`, `OPENAI_API_KEY`,
`ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, `DEEPSEEK_API_KEY`, ...). Never put
a literal key in code; `llm_config()` resolves the provider's variable by
default, and configs print masked. A missing key errors at call time —
report it, do not work around it.

## Core API

```r
llm_config(provider, model, ..., api_key = NULL, embedding = NULL)
  # canonical params: temperature, max_tokens, top_p, seed, logprobs,
  # top_logprobs, thinking_budget, timeout, cache; api_url for any
  # OpenAI-compatible server (vLLM, llama.cpp, localhost)
call_llm(config, messages, verbose = FALSE)      # -> llmr_response
as.character(x); tokens(x); finish_reason(x)

# tidy verbs over data frames
llm_fn(x, prompt, .config, ...)
llm_mutate(.data, output, prompt, .config, ...)  # + llm_mutate_structured()
enable_structured_output(config, schema = NULL, ...)
llm_parse_structured(x); llm_parse_structured_col(.data, ...)

# scale
build_factorial_experiments(...); call_llm_par(experiments, ...)
call_llm_broadcast(config, messages, ...); llm_par_resume(...)
llm_batch_submit(config, messages, state_path = NULL)   # half-price async
llm_batch_status(job); llm_batch_fetch(job); llm_batch_cancel(job)

# interactive layer
llm_tool(fn, name, description, parameters = NULL, required = NULL)
call_llm_tools(config, messages, tools, max_rounds = 8L,
               max_tool_calls = Inf, ...)   # attr(x,"tool_loop") = real spend
call_llm_stream(config, messages, callback = ..., verbose = FALSE)
llm_logprobs(x)

# embeddings
get_batched_embeddings(texts, embed_config, ...)

# reproducibility
llm_log_enable(path, include_messages = TRUE); llm_log_disable()
llm_replicate(.data, output, prompt, .config, .times = 3, ...)
llm_agreement(.data, cols = NULL, prefix = NULL)
llm_usage(x, price_table = NULL); llm_methods_text(x, ...)
llm_hash(x)        # the ecosystem content-hash convention
chat_session(config, system = NULL, ...)
```

## Messages: three accepted shapes

```r
"one user turn"
c(system = "be terse", user = "the question")          # named multi-role
c(user = "describe this", file = "path/to/image.png")  # multimodal
```

## Canonical patterns

```r
cfg <- llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
r <- call_llm(cfg, c(system = "One word only.", user = "Capital of Chile?"))

# data-frame annotation (the workhorse)
df <- data.frame(text = c("I loved every minute.", "A dreadful, boring slog."))
out <- llm_mutate(df, sentiment,
                  prompt = "One word, positive or negative: {text}",
                  .config = cfg)
llm_usage(out); llm_failures(out)

# schema-enforced output
schema <- list(type = "object",
               properties = list(stance = list(type = "string",
                                               enum = list("pro", "anti"))),
               required = list("stance"))
out <- llm_mutate_structured(df, ans, prompt = "...{text}",
                             .config = cfg, .schema = schema)
```

## Rules and provider facts

- For measurement, `temperature = 0`; for replication runs leave sampling
  on and use `llm_replicate()` + `llm_agreement()` (Krippendorff alpha).
- Strict JSON-schema mode is provider-gated: openai/groq/together/xai/
  ollama take real schemas (auto-hardened: `additionalProperties: false`,
  required filled); deepseek/alibaba/zhipu/moonshot get JSON-object mode
  with local validation. Anthropic uses a schema tool; Gemini uses
  `responseJsonSchema`.
- Logprobs: deepseek and openai expose them; groq and most gemini models
  reject the flag; anthropic has none.
- `tokens(x)` covers one call; after `call_llm_tools()` read
  `attr(x, "tool_loop")` for true loop spend.
- Failed rows in tidy verbs carry diagnostics (`llm_failures()`); braces in
  user text are safe everywhere.
- Local/open-weight: any OpenAI-compatible endpoint via
  `llm_config(..., api_url = "http://localhost:8000/v1/chat/completions")`
  or provider `"ollama"`.
- Retries: `tries`/`wait_seconds` on robust/parallel paths honor
  Retry-After; permanent errors fail fast.

## Error meanings

- "Missing API key. Set environment variable ..." → export the named
  variable; do not paste keys into code.
- 400 with "logprobs is not supported" → that model rejects the flag.
- `llmr_tool_limit` condition → `max_tool_calls` cap reached mid-loop.
- Batch "not finished" → poll `llm_batch_status()`; fetch later.
