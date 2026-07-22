---
name: llmr
description: One provider-neutral configuration and call interface for language-model research in R, from single calls to parallel and batch experiments with call logging.
---

# LLMR -- usage capsule

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
default, and configs print masked. A missing key errors at call time --
report it, do not work around it.

## Core API

```r
llm_config(provider, model, api_key = NULL, troubleshooting = FALSE,
           base_url = NULL, embedding = NULL, no_change = FALSE, ...)
  # common generation settings: temperature, max_tokens, top_p, seed, logprobs,
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
llm_batch_submit(config, messages, state_path = NULL)   # provider batch API;
                                        # pricing and delay are provider-defined
llm_batch_status(job); llm_batch_fetch(job); llm_batch_cancel(job)

# interactive layer
llm_tool(fn, name, description, parameters = NULL, required = NULL)
call_llm_tools(config, messages, tools, max_rounds = 8L,
               max_tool_calls = Inf, ...)   # x$tool_loop records total spend
call_llm_stream(config, messages, callback = ..., verbose = FALSE)
llm_logprobs(x)

# embeddings
get_batched_embeddings(texts, embed_config, ...)

# reproducibility
llm_log_enable(path, include_messages = TRUE); llm_log_disable()
llm_replicate(.data, output, prompt, .config, .times = 3, ...)
llm_agreement(.data, cols = NULL, prefix = NULL)
llm_usage(x, price_table = NULL); report(x, ...)
llm_hash(x)        # the stable content hash used across these packages
chat_session(config, system = NULL, ...)
```

## Messages: three accepted shapes

```r
"one user turn"
c(system = "be terse", user = "the question")          # named multi-role
c(user = "describe this", file = "path/to/image.png")  # multimodal
```

## Common usage patterns

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

# comparative experiment with a call log
cfg2 <- llm_config("groq", "llama-3.1-8b-instant", temperature = 0)
design <- build_factorial_experiments(
  configs = list(cfg, cfg2),
  user_prompts = c("Summarize the Apollo program.",
                   "Summarize the Manhattan Project."),
  repetitions = 2
)
log_path <- tempfile(fileext = ".jsonl")
llm_log_enable(log_path)
study <- call_llm_par(design)
llm_log_disable()
llm_usage(study)
llm_failures(study)
report(study)
```

## Rules and provider facts

- Choose temperature for the estimand: `temperature = 0` for single-label
  annotation; for replication runs leave sampling on and use
  `llm_replicate()` + `llm_agreement()` (Krippendorff alpha).
- Strict JSON-schema mode is provider-gated: openai/groq/together/xai/
  openrouter/ollama take real schemas (auto-hardened:
  `additionalProperties: false`, required filled);
  deepseek/alibaba/zhipu/moonshot get JSON-object mode with local
  validation. Anthropic uses a schema tool; Gemini uses
  `responseJsonSchema`.
- Logprobs: openai and deepseek expose them; support elsewhere varies by
  model and endpoint (an unsupported endpoint returns a provider error);
  anthropic has none.
- `tokens(x)` covers one call; after `call_llm_tools()` read
  `x$tool_loop` for total loop spend.
- Failed rows in tidy verbs carry diagnostics (`llm_failures()`); braces in
  user text are safe everywhere.
- Local/open-weight: any OpenAI-compatible endpoint via
  `llm_config(..., api_url = "http://localhost:8000/v1/chat/completions")`
  or provider `"ollama"`.
- Retries: `tries`/`wait_seconds` on robust/parallel paths honor
  Retry-After; permanent errors fail fast.

## Error meanings

- "Missing API key. Set environment variable ..." -> export the named
  variable; do not paste keys into code.
- 400 with "logprobs is not supported" -> that model rejects the flag.
- `llmr_tool_limit` condition -> `max_tool_calls` cap reached mid-loop.
- Batch "not finished" -> poll `llm_batch_status()`; fetch later.
