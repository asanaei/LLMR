# Apply an LLM prompt over vectors/data frames

Apply an LLM prompt over vectors/data frames

## Usage

``` r
llm_fn(
  x,
  prompt,
  .config,
  .system_prompt = NULL,
  ...,
  .return = c("text", "columns", "object")
)
```

## Arguments

- x:

  A character vector *or* a data.frame/tibble.

- prompt:

  A glue template string. With a data-frame you may reference columns
  (`{col}`); with a vector the placeholder is `{x}`.

- .config:

  An
  [llm_config](https://asanaei.github.io/LLMR/reference/llm_config.md)
  object.

- .system_prompt:

  Optional system message (character scalar).

- ...:

  Passed unchanged to
  [`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md)
  (e.g. `tries`, `progress`, `verbose`).

- .return:

  One of `c("text","columns","object")`. `"columns"` returns a tibble of
  diagnostic columns; `"text"` returns a character vector; `"object"`
  returns a list of `llmr_response` (or `NA` on failure).

## Value

For generative mode:

- `.return = "text"`: character vector

- `.return = "columns"`: tibble with diagnostics

- `.return = "object"`: list of `llmr_response` (or `NA` on failure) For
  embedding mode, always a numeric matrix.

## See also

[`llm_mutate()`](https://asanaei.github.io/LLMR/reference/llm_mutate.md),
[`setup_llm_parallel()`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md),
[`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md)

## Examples

``` r
if (interactive()) {
  words <- c("excellent","awful")
  cfg <- llm_config("openai","gpt-4o-mini", temperature = 0)
  llm_fn(words, "Classify '{x}' as Positive/Negative.",
         cfg,
         .system_prompt="One word.",
         .return="columns")
}
```
