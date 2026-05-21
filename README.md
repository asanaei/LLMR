# LLMR

<img src="https://github.com/asanaei/LLMR/raw/main/assets/LLMR_512x512.png" width="120" alt="LLMR logo">

[![CRAN status](https://www.r-pkg.org/badges/version/LLMR)](https://CRAN.R-project.org/package=LLMR)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/LLMR)](https://cran.r-project.org/package=LLMR)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/asanaei/LLMR/workflows/R-CMD-check/badge.svg)](https://github.com/asanaei/LLMR/actions)
[![Website](https://img.shields.io/badge/website-pkgdown-blue.svg)](https://asanaei.github.io/LLMR/)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub issues](https://img.shields.io/github/issues/asanaei/LLMR)](https://github.com/asanaei/LLMR/issues)

LLMR is an R package for running large language model workflows as ordinary R code. It provides one interface for hosted and local providers, with tidy helpers for repeated calls, structured JSON, XML-like tags, embeddings, chat sessions, retries, and parallel experiments.

## Installation

```r
install.packages("LLMR") # CRAN
# Development:
# remotes::install_github("asanaei/LLMR")
```

## Quick start

### Configure a model

```r
library(LLMR)

cfg <- llm_config(
  provider = "openai",
  model = "gpt-4.1-nano",
  temperature = 0.2,
  max_tokens = 256
)
```

Store keys in environment variables such as OPENAI_API_KEY, ANTHROPIC_API_KEY, GEMINI_API_KEY.
Reasoning models such as gpt-5-nano often reject custom temperature values. Omit `temperature` unless the selected model accepts it.

For Gemini on Vertex AI, keep `provider = "gemini"` and opt in to Vertex routing:

```r
cfg_vertex <- llm_config(
  provider = "gemini",
  model = "gemini-2.5-flash-lite",
  vertex = TRUE,
  project = "my-gcp-project",
  location = "us-central1",
  api_key = "VERTEX_ACCESS_TOKEN"
)
```

### One-shot generation

```r
r <- call_llm(
  config = cfg,
  messages = c(
    system = "You are a branding expert.",
    user = "Six-word catch-phrase for eco-friendly balloons."
  )
)

print(r) # text + status line
as.character(r) # just the text
finish_reason(r)
tokens(r)
is_truncated(r)
```

### Structured output (JSON with schema)

```r
schema <- list(
  type = "object",
  properties = list(
    label = list(type = "string"),
    score = list(type = "number")
  ),
  required = list("label", "score"),
  additionalProperties = FALSE
)

cfg_s <- enable_structured_output(cfg, schema = schema)

resp <- call_llm(cfg_s, c(system = "Reply JSON only.", user = "Label and score for 'MNIST'."))
parsed <- llm_parse_structured(resp)
str(parsed)
```

Or use higher-level helpers:

```r
words <- c("excellent", "awful", "fine")

out <- llm_fn_structured(
  x = words,
  prompt = "Classify '{x}' and output {label, score in [0,1]} as JSON.",
  .config = cfg,
  .schema = schema,
  .fields = c("label", "score")
)
out
```

### Tidy helpers

Use `llm_fn()` for vectors and `llm_mutate()` inside data-frame pipelines. The shorthand form puts the output column and prompt in one argument.

```r
words <- c("excellent", "awful", "fine")
llm_fn(words, "Classify '{x}'.", .config = cfg, .return = "text")

df <- tibble::tibble(
  city = c("Cairo", "Lima"),
  text = c("Cairo is large.", "Lima is coastal.")
)

df |>
  llm_mutate(
    country = "Where is {city}? Answer with only the country.",
    .config = cfg
  )
```

Strict JSON extraction can be used directly in `llm_mutate()`:

```r
df |>
  llm_mutate(
    out = "{text}",
    .config = cfg,
    .structured = TRUE,
    .schema = schema
  )
```

For lighter extraction, use XML-like tags:

```r
df |>
  llm_mutate(
    geo = "Where is {city}? Give country and continent in their own tags.",
    .config = cfg,
    .system_prompt = paste(
      "Use XML tags to specify different parts of the answer, but do not nest tags.",
      "Return <country>...</country> and <continent>...</continent>."
    ),
    .tags = c("country", "continent")
  )
```

### Embeddings

```r
sentences <- c(
  one = "Quiet rivers mirror bright skies.",
  two = "Thunder shakes the mountain path."
)

emb_cfg <- llm_config(
  provider = "voyage",
  model = "voyage-3.5-lite",
  embedding = TRUE
)

emb <- call_llm(emb_cfg, sentences) |> parse_embeddings()
dim(emb)
```

Batch embeddings:

```r
emb <- get_batched_embeddings(
  texts = sentences,
  embed_config = emb_cfg,
  batch_size = 8
)
```

### Conversation with history

```r
chat <- chat_session(cfg, system = "You teach statistics tersely.")
chat$send("Explain p-values in 12 words.")
chat$send("Now give a three-word analogy.")
print(chat)
```

### Parallel runs

```r
setup_llm_parallel(workers = 4)

experiments <- build_factorial_experiments(
  configs = list(cfg),
  user_prompts = c("Summarize in one sentence: The Apollo program."),
  system_prompts = "Be concise."
)

res <- call_llm_par(experiments, progress = TRUE)
reset_llm_parallel()
```

### Which helper should I use?

- One prompt: `call_llm()` or `call_llm_robust()`.
- Vector inputs: `llm_fn()`.
- Data-frame pipelines: `llm_mutate()`.
- Strict JSON and typed columns: `.structured = TRUE`, `llm_fn_structured()`, or `llm_mutate_structured()`.
- Soft field extraction: `.tags` or `llm_mutate_tags()`.
- Existing response columns: `llm_parse_structured_col()` or `llm_parse_tags_col()`.
- Factorial prompt/model designs: `build_factorial_experiments()` plus `call_llm_par()`.

## Notes

- Generative calls return `llmr_response`; use `as.character()` when you want plain text.
- Structured output uses `enable_structured_output()` for provider-specific request settings, then `llm_parse_structured()` and `llm_validate_structured_col()` for local parsing and validation.
- Embeddings from `call_llm()` remain provider-native until `parse_embeddings()` converts them to a numeric matrix.
- `call_llm_robust()` handles transient failures and rate limits with backoff.

## Contributions and support

Bug reports, feature requests, and support questions should be opened as GitHub issues. Please include a minimal reproducible example when relevant. Pull requests are welcome for focused improvements. See [CONTRIBUTING.md](CONTRIBUTING.md) for contribution, support, and reporting guidance. See [COPYING](COPYING) for the full MIT license text.
