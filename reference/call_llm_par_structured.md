# Parallel experiments with structured parsing

Enables structured output on each config (if not already set), runs,
then parses JSON.

## Usage

``` r
call_llm_par_structured(experiments, schema = NULL, .fields = NULL, ...)
```

## Arguments

- experiments:

  Tibble with `config` and `messages` list-columns.

- schema:

  Optional JSON Schema list.

- .fields:

  Optional fields to hoist from parsed JSON (supports nested paths).

- ...:

  Passed to
  [`call_llm_par()`](https://asanaei.github.io/LLMR/reference/call_llm_par.md).
