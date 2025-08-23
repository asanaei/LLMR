# LLMR

<img src="https://github.com/asanaei/LLMR/raw/main/assets/LLMR_512x512.png" width="120" alt="LLMR logo">

[![CRAN status](https://www.r-pkg.org/badges/version/LLMR)](https://CRAN.R-project.org/package=LLMR)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/LLMR)](https://cran.r-project.org/package=LLMR)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/asanaei/LLMR/workflows/R-CMD-check/badge.svg)](https://github.com/asanaei/LLMR/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub issues](https://img.shields.io/github/issues/asanaei/LLMR)](https://github.com/asanaei/LLMR/issues)

LLMR offers a unified interface for Large Language Models in R. It supports multiple providers, robust retries, structured output, and embeddings.

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
model = "gpt-4o-mini",
temperature = 0.2,
max_tokens = 256
)
```

Store keys in environment variables such as OPENAI_API_KEY, ANTHROPIC_API_KEY, GEMINI_API_KEY.

### One-shot generation

```r
r <- call_llm(
config = cfg,
messages = c(
system = "You are a branding expert.",
user = "Six-word catch-phrase for eco-friendly balloons.")
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
required = list("label","score"),
additionalProperties = FALSE
)

cfg_s <- enable_structured_output(cfg, schema = schema)

resp <- call_llm(cfg_s, c(system="Reply JSON only.", user="Label and score for 'MNIST'."))
parsed <- llm_parse_structured(resp)
str(parsed)
```

Or use higher-level helpers:

```r
words <- c("excellent","awful","fine")

out <- llm_fn_structured(
x = words,
prompt = "Classify '{x}' and output {label, score in [0,1]} as JSON.",
.config = cfg,
.schema = schema,
.fields = c("label","score")
)
out
```

### Embeddings

```r
sentences <- c(
one="Quiet rivers mirror bright skies.",
two="Thunder shakes the mountain path."
)

emb_cfg <- llm_config(
provider = "voyage",
model = "voyage-large-2",
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
system_prompts= "Be concise."
)

res <- call_llm_par(experiments, progress = TRUE)
reset_llm_parallel()
```

## Notes

- Generative calls return llmr_response. Coerce with as.character() when you want plain text.
- Structured output:
- enable_structured_output() selects the correct provider toggle.
- Use llm_parse_structured() and optional llm_validate_structured_col() for robust local parsing and validation.
- Embeddings: call_llm() returns a provider-native list; parse_embeddings() converts it to a numeric matrix.
- Robust retries: call_llm_robust() handles rate limits with exponential backoff.

## Contributions

Issues and pull requests are welcome. Include a minimal reproducible exampleyy.
