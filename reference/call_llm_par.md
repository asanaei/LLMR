# Parallel LLM Processing with Tibble-Based Experiments (Core Engine)

Processes experiments from a tibble where each row contains a config and
message pair. This is the core parallel processing function. Metadata
columns are preserved. This function requires setting up the parallel
environment using `setup_llm_parallel`.

## Usage

``` r
call_llm_par(
  experiments,
  simplify = TRUE,
  tries = 10,
  wait_seconds = 2,
  backoff_factor = 120^(1/tries),
  verbose = FALSE,
  memoize = FALSE,
  max_workers = NULL,
  progress = FALSE,
  json_output = NULL,
  start_jitter = 5
)
```

## Arguments

- experiments:

  A tibble/data.frame with required list-columns 'config' (llm_config
  objects) and 'messages' (character vector OR message list).

- simplify:

  Whether to cbind 'experiments' to the output data frame or not.

- tries:

  Integer. Number of retries for each call. Default is 10.

- wait_seconds:

  Numeric. Initial wait time (seconds) before retry. Default is 2.

- backoff_factor:

  Numeric. Multiplier for wait time after each failure. Default is 3.

- verbose:

  Logical. If TRUE, prints progress and debug information.

- memoize:

  Logical. If TRUE, enables caching for identical requests.

- max_workers:

  Integer. Maximum number of parallel workers. If NULL, auto-detects.

- progress:

  Logical. If TRUE, shows progress bar.

- json_output:

  Deprecated. Raw JSON string is always included as raw_response_json.
  This parameter is kept for backward compatibility but has no effect.

- start_jitter:

  Calls are made after a uniformly distributed delay between 0 and
  `start_jitter` seconds.

## Value

A tibble containing all original columns plus:

- `response_text` – assistant text (or `NA` on failure)

- `raw_response_json` – raw JSON string

- `success`, `error_message`

- `finish_reason` – e.g. "stop", "length", "filter", "tool", or
  "error:`category`"

- `sent_tokens`, `rec_tokens`, `total_tokens`, `reasoning_tokens`

- `response_id`

- `duration` – seconds

- `response` – the full `llmr_response` object (or `NA` on failure)

The `response` column holds `llmr_response` objects on success, or
`NULL` on failure.

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

For setting up the environment:
[`setup_llm_parallel`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md),
[`reset_llm_parallel`](https://asanaei.github.io/LLMR/reference/reset_llm_parallel.md).
For simpler, pre-configured parallel tasks:
[`call_llm_broadcast`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md),
[`call_llm_sweep`](https://asanaei.github.io/LLMR/reference/call_llm_sweep.md),
[`call_llm_compare`](https://asanaei.github.io/LLMR/reference/call_llm_compare.md).
For creating experiment designs:
[`build_factorial_experiments`](https://asanaei.github.io/LLMR/reference/build_factorial_experiments.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple example: Compare two models on one prompt
cfg1 <- llm_config("openai", "gpt-4.1-nano")
cfg2 <- llm_config("groq", "llama-3.3-70b-versatile")

experiments <- tibble::tibble(
  model_id = c("gpt-4.1-nano", "groq-llama-3.3"),
  config = list(cfg1, cfg2),
  messages = "Count the number of the letter e in this word: Freundschaftsbeziehungen "
)

setup_llm_parallel(workers = 2)
results <- call_llm_par(experiments, progress = TRUE)
reset_llm_parallel()

print(results[, c("model_id", "response_text")])

} # }
```
