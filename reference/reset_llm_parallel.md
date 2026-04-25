# Reset Parallel Environment

Resets the future plan to sequential processing.

## Usage

``` r
reset_llm_parallel(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. If TRUE, prints reset information.

## Value

Invisibly returns the future plan that was in place before resetting to
sequential.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Setup parallel processing
  old_plan <- setup_llm_parallel(workers = 2)

  # Do some parallel work...

  # Reset to sequential
  reset_llm_parallel(verbose = TRUE)

  # Optionally restore the specific old_plan if it was non-sequential
  # future::plan(old_plan)
} # }
```
