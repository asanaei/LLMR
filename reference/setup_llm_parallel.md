# Setup Parallel Environment for LLM Processing

Convenience function to set up the future plan for optimal LLM parallel
processing. Automatically detects system capabilities and sets
appropriate defaults.

## Usage

``` r
setup_llm_parallel(workers = NULL, strategy = NULL, verbose = FALSE)
```

## Arguments

- workers:

  Integer. Number of workers to use. If NULL, auto-detects optimal
  number (availableCores - 1, capped at 8). If called as
  `setup_llm_parallel(4)`, the single numeric positional argument is
  interpreted as `workers`.

- strategy:

  Character. The future strategy to use. Options: "multisession",
  "multicore", "sequential". If NULL (default), automatically chooses
  "multisession".

- verbose:

  Logical. If TRUE, prints setup information.

## Value

Invisibly returns the previous future plan.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Automatic setup
  setup_llm_parallel()

  # Manual setup with specific workers
  setup_llm_parallel(workers = 4, verbose = TRUE)

  # Force sequential processing for debugging
  setup_llm_parallel(strategy = "sequential")

  # Restore old plan if needed
  reset_llm_parallel()
} # }
```
