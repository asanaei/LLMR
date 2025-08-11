# LLMR

<img src="https://github.com/asanaei/LLMR/raw/main/assets/LLMR_512x512.png" width="120" alt="LLMR logo">

[![CRAN status](https://www.r-pkg.org/badges/version/LLMR)](https://CRAN.R-project.org/package=LLMR)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/LLMR)](https://cran.r-project.org/package=LLMR)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/asanaei/LLMR/workflows/R-CMD-check/badge.svg)](https://github.com/asanaei/LLMR/actions)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![GitHub issues](https://img.shields.io/github/issues/asanaei/LLMR)](https://github.com/asanaei/LLMR/issues)

LLMR offers a unified interface for interacting with multiple Large Language Model APIs in R.

## Installation

```r
install.packages("LLMR")        # CRAN (preferred)
# Development version:
# remotes::install_github("asanaei/LLMR")
````

---

## Quick Start

### Configuration

Configuration is designed so the same code can easily be done with multiple providers, models, parameters. 

```r
llm_config(
  provider     = "openai",
  model        = "gpt-4o-mini",
  api_key      = "YOUR API KEY", # never write it directly
  temperature  = 0,
  max_tokens   = 256,
)
```



### One‑shot text generation (`call_llm()`)

```r
library(LLMR)

cfg <- llm_config(
  provider = "openai",
  model    = "gpt-4o",
  api_key  = Sys.getenv("OPENAI_API_KEY"),
  temperature = 0.7)

slogan <- call_llm( 
  config   = cfg,
  messages = c(
    system = "You are a branding expert.",
    user   = "Six‑word catch‑phrase for eco‑friendly balloons." )
)

cat(slogan)
```

### Inspect the response

```r
  r <- call_llm(cfg, "Say hello in Greek.", json = TRUE)
  r
  as.character(r)
  finish_reason(r)
  tokens(r)
  is_truncated(r)
```

### Short embeddings

```r
sentences <- c(
  one="Quiet rivers mirror bright skies.",
  two="Thunder shakes the mountain path.",
  three="Water is the juice of life!")

emb_cfg <- llm_config(
  provider = "voyage",
  model    = "voyage-large-2",
  api_key  = Sys.getenv("VOYAGE_KEY") )

emb <- call_llm(emb_cfg, sentences) |> parse_embeddings()

dim(emb)
cor(t(emb))

# also see get_batched_embeddings
```

### Conversation when you need memory (`chat_session()`)

```r
chat <- chat_session(
  config = cfg,
  system = "You teach statistics tersely.")
  
chat$send( "Explain p‑values in 12 words.")
chat$send( "Now give a three‑word analogy.")
print(chat)
```

---

## Functional Mapping (`llm_fn()`)

```r
movies <- c("Inception", "Spirited Away", "Parasite")

taglines <- llm_fn(
  x      = movies,
  prompt = "One‑line playful tagline for the film {x}",
  .config = cfg)

tibble(movies, taglines)
```

---

## Data‑Frame Helper (`llm_mutate()`)

```r
library(dplyr)

songs <- tibble(
  title  = c("Blue in Green", "Giant Steps to Jupiter"),
  artist = c("Miles Davis", "Pinocchio Geppetto") )

sdf = songs |>
  llm_mutate(
    .config = cfg,
    output=two_word,
    .system_prompt = 'answer in exactly two words',
   prompt = "Guess the jazz sub‑genre for '{title}' (two words)."
  ) |>
  llm_mutate(
    .config = cfg,
    output=three_word,
    .system_prompt = 'answer in exactly three words',
   prompt = "Guess the jazz sub‑genre for title='{title}' by '{artist}'. (three words)."
  )

sdf[,c('title','artist','two_word','three_word')]

# see names(sdf) for other information returned from each call
# like sdf$two_word_finish 
  
```

---

## Multimodal in One Short Request

```r
png(tmp <- tempfile(fileext = ".png"))
plot(rnorm(10000)|>sort(), pch = '.',col='blue',ann=FALSE)
grid()
dev.off()

vision_cfg <- llm_config(
  provider = "openai",
  model    = "gpt-5-chat-latest",
  api_key  = Sys.getenv("OPENAI_API_KEY")
)

call_llm(
  vision_cfg,
  c( system = "you are a scientist",
     user = "Describe this picture in five words.",
     file = tmp)  # tmp is the path for the example plot created above
)
```

---


* **Provider‑specific settings** (e.g., `model`, `endpoint`) are forwarded automatically.
* Raw results: 
  - call with `json = TRUE` to get an `llmr_response`; 
  - use `finish_reason(x)`, `tokens(x)`, `is_truncated(x)`;
  - or `attr(x, "raw_json")` to see raw JSON output.
  
---

## Removed Legacy Objects

`Agent` class and  `LLMConversation` were removed to give `LLMR` better focus. 

## Contributions

Pull requests and issues welcome---please include a minimal reproducible example.


