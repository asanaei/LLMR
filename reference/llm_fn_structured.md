# Vectorized structured-output LLM

Schema-first variant of
[`llm_fn()`](https://asanaei.github.io/LLMR/reference/llm_fn.md). It
enables structured output on the config, calls the model via
[`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md),
parses JSON, and optionally validates.

## Usage

``` r
llm_fn_structured(
  x,
  prompt,
  .config,
  .system_prompt = NULL,
  ...,
  .schema = NULL,
  .fields = NULL,
  .local_only = FALSE,
  .validate_local = TRUE
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

- .schema:

  Optional JSON Schema list; if `NULL`, only JSON object is enforced.

- .fields:

  Optional fields to hoist from parsed JSON (supports nested paths).

- .local_only:

  If TRUE, do not send schema to the provider (parse/validate locally).

- .validate_local:

  If TRUE and `.schema` provided, validate locally.
