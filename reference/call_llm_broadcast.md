# Parallel API calls: Fixed Config, Multiple Messages

Broadcasts different messages using the same configuration in parallel.
Perfect for batch processing different prompts with consistent settings.
This function requires setting up the parallel environment using
`setup_llm_parallel`.

## Usage

``` r
call_llm_broadcast(config, messages, ...)
```

## Arguments

- config:

  Single llm_config object to use for all calls.

- messages:

  A character vector (each element is a prompt) OR a list where each
  element is a pre-formatted message list.

- ...:

  Additional arguments passed to `call_llm_par` (e.g., tries, verbose,
  progress).

## Value

A tibble with columns: message_index (metadata), provider, model, all
model parameters, response_text, raw_response_json, success,
error_message.

## Parallel Workflow

All parallel functions require the `future` backend to be configured.
The recommended workflow is:

1.  Call
    [`setup_llm_parallel()`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md)
    once at the start of your script.

2.  Run one or more parallel experiments (e.g., `call_llm_broadcast()`).

3.  Call
    [`reset_llm_parallel()`](https://asanaei.github.io/LLMR/reference/reset_llm_parallel.md)
    at the end to restore sequential processing.

## See also

[`setup_llm_parallel`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md),
[`reset_llm_parallel`](https://asanaei.github.io/LLMR/reference/reset_llm_parallel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Broadcast different questions
  config <- llm_config(provider = "openai", model = "gpt-4.1-nano")

  messages <- list(
    list(list(role = "user", content = "What is 2+2?")),
    list(list(role = "user", content = "What is 3*5?")),
    list(list(role = "user", content = "What is 10/2?"))
  )

  setup_llm_parallel(workers = 4, verbose = TRUE)
  results <- call_llm_broadcast(config, messages)
  reset_llm_parallel(verbose = TRUE)
} # }
```
