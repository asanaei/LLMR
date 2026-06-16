# LLMR

<img src="https://github.com/asanaei/LLMR/raw/main/assets/LLMR_512x512.png" width="120" alt="LLMR logo">

[![CRAN status](https://www.r-pkg.org/badges/version/LLMR)](https://CRAN.R-project.org/package=LLMR)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/LLMR)](https://cran.r-project.org/package=LLMR)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/asanaei/LLMR/workflows/R-CMD-check/badge.svg)](https://github.com/asanaei/LLMR/actions)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMR/)

LLMR gives R a single tidy-friendly interface to hosted and local language models.
One `llm_config()` object selects the provider; every other function works the same regardless of which model is behind it.

**Providers:** OpenAI, Anthropic, Gemini (incl. Vertex AI), Groq, Together AI, DeepSeek, xAI, Voyage AI, Ollama, Xiaomi MiMo, Alibaba (Qwen), Zhipu, Moonshot.

## Install

```r
install.packages("LLMR")                        # CRAN
# remotes::install_github("asanaei/LLMR")       # dev
```

## Setup

Store API keys as environment variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, etc.) in `~/.Renviron`.

```r
library(LLMR)

cfg <- llm_config("openai", "gpt-4.1-nano", temperature = 0.2, max_tokens = 256)
```

## One call

```r
r <- call_llm(cfg, c(system = "Be concise.", user = "Capital of Mongolia?"))
r                    # prints text + status line
as.character(r)      # just the text
tokens(r)            # token counts
```

## Data-frame pipelines

`llm_mutate()` adds LLM-generated columns. The shorthand puts the output name and glue prompt in one argument.

```r
library(dplyr)

df <- tibble::tibble(city = c("Cairo", "Lima", "Oslo"))

df |>
  llm_mutate(country = "What country is {city} in? One word.", .config = cfg)
```

For vectors without a data frame, use `llm_fn()`.

## Structured output (JSON)

Get typed columns from a JSON schema:

```r
schema <- list(
  type = "object",
  properties = list(
    answer = list(type = "string"),
    confidence = list(type = "number")
  ),
  required = list("answer", "confidence"),
  additionalProperties = FALSE
)

df |>
  llm_mutate(
    result = "What country is {city} in?",
    .config = cfg,
    .structured = TRUE,
    .schema = schema
  )
```

## Soft structured output (XML-like tags)

When strict JSON is unnecessary, request fields as tags:

```r
df |>
  llm_mutate(
    geo = "Where is {city}? Give country and continent.",
    .config = cfg,
    .tags = c("country", "continent")
  )
```

## Embeddings

```r
emb_cfg <- llm_config("voyage", "voyage-3.5-lite", embedding = TRUE)

texts <- c("Quiet rivers mirror bright skies.", "Thunder shakes the mountain path.")
emb   <- get_batched_embeddings(texts, emb_cfg, batch_size = 8)
dim(emb)
```

## Chat sessions

```r
chat <- chat_session(cfg, system = "You teach statistics tersely.")
chat$send("Explain p-values in 12 words.")
chat$send("Now give a three-word analogy.")
```

## Parallel experiments

```r
setup_llm_parallel(workers = 4)

experiments <- build_factorial_experiments(
  configs = list(cfg),
  user_prompts = c("Summarize: The Apollo program.", "Summarize: The Manhattan Project."),
  system_prompts = "One sentence."
)

res <- call_llm_par(experiments, progress = TRUE)
reset_llm_parallel()
```

## Which function do I need?

| Task | Function |
|---|---|
| Single prompt | `call_llm()` / `call_llm_robust()` |
| Vector of prompts | `llm_fn()` |
| Data-frame pipeline | `llm_mutate()` |
| JSON with schema | `.structured = TRUE` / `llm_mutate_structured()` |
| Tag-based extraction | `.tags` / `llm_mutate_tags()` |
| Parse existing column | `llm_parse_structured_col()` / `llm_parse_tags_col()` |
| Factorial designs | `build_factorial_experiments()` + `call_llm_par()` |

## The LLMR ecosystem

LLMR is the provider layer of a family of packages for LLM-assisted research.
[LLMRAgent](https://asanaei.github.io/LLMRAgent/) builds agents and
multi-agent designs on top of it.
[LLMRcontent](https://asanaei.github.io/LLMRcontent/) carries the measurement
workflow in one package: codebook-first annotation with sealed gold-set
validation, robustness audits of LLM-mediated estimates, and verifiable
replication archives built from LLMR's audit logs.
[LLMRpanel](https://asanaei.github.io/LLMRpanel/) provides calibrated silicon
samples for survey and experiment design, and
[FocusGroup](https://asanaei.github.io/FocusGroup/) supports moderated
multi-agent discussions. An overview of the family lives at the
[ecosystem page](https://asanaei.github.io/LLMR-ecosystem/).

## Contributing

Bug reports and feature requests: [GitHub Issues](https://github.com/asanaei/LLMR/issues).
Pull requests welcome. See [CONTRIBUTING.md](https://github.com/asanaei/LLMR/blob/main/CONTRIBUTING.md).
