# Parallel API calls: Parameter Sweep - Vary One Parameter, Fixed Message

Sweeps through different values of a single parameter while keeping the
message constant. Perfect for hyperparameter tuning, temperature
experiments, etc. This function requires setting up the parallel
environment using `setup_llm_parallel`.

## Usage

``` r
call_llm_sweep(base_config, param_name, param_values, messages, ...)
```

## Arguments

- base_config:

  Base llm_config object to modify.

- param_name:

  Character. Name of the parameter to vary (e.g., "temperature",
  "max_tokens").

- param_values:

  Vector. Values to test for the parameter.

- messages:

  A character vector or a list of message objects (same for all calls).

- ...:

  Additional arguments passed to `call_llm_par` (e.g., tries, verbose,
  progress).

## Value

A tibble with columns: swept_param_name, the varied parameter column,
provider, model, all other model parameters, response_text,
raw_response_json, success, error_message.

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
  # Temperature sweep
  config <- llm_config(provider = "openai", model = "gpt-4.1-nano")

  messages <- "What is 15 * 23?"
  temperatures <- c(0, 0.3, 0.7, 1.0, 1.5)

  setup_llm_parallel(workers = 4, verbose = TRUE)
  results <- call_llm_sweep(config, "temperature", temperatures, messages)
  results |> dplyr::select(temperature, response_text)
  reset_llm_parallel(verbose = TRUE)
} # }
```
