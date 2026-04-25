# Parallel API calls: Multiple Configs, Fixed Message

Compares different configurations (models, providers, settings) using
the same message. Perfect for benchmarking across different models or
providers. This function requires setting up the parallel environment
using `setup_llm_parallel`.

## Usage

``` r
call_llm_compare(configs_list, messages, ...)
```

## Arguments

- configs_list:

  A list of llm_config objects to compare.

- messages:

  A character vector or a list of message objects (same for all
  configs).

- ...:

  Additional arguments passed to `call_llm_par` (e.g., tries, verbose,
  progress).

## Value

A tibble with columns: config_index (metadata), provider, model, all
varying model parameters, response_text, raw_response_json, success,
error_message.

## Parallel Workflow

All parallel functions require the `future` backend to be configured.
The recommended workflow is:

1.  Call
    [`setup_llm_parallel()`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md)
    once at the start of your script.

2.  Run one or more parallel experiments (e.g.,
    [`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md)).

3.  Call
    [`reset_llm_parallel()`](https://asanaei.github.io/LLMR/reference/reset_llm_parallel.md)
    at the end to restore sequential processing.

## See also

[`setup_llm_parallel`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md),
[`reset_llm_parallel`](https://asanaei.github.io/LLMR/reference/reset_llm_parallel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Compare different models
  config1 <- llm_config(provider = "openai", model = "gpt-4o-mini")
  config2 <- llm_config(provider = "openai", model = "gpt-4.1-nano")

  configs_list <- list(config1, config2)
  messages <- "Explain quantum computing"

  setup_llm_parallel(workers = 4, verbose = TRUE)
  results <- call_llm_compare(configs_list, messages)
  reset_llm_parallel(verbose = TRUE)
} # }
```
