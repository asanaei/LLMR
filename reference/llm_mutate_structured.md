# Data-frame mutate with structured output

Drop-in schema-first variant of
[`llm_mutate()`](https://asanaei.github.io/LLMR/reference/llm_mutate.md).
Produces parsed columns.

## Usage

``` r
llm_mutate_structured(
  .data,
  output,
  prompt = NULL,
  .messages = NULL,
  .config,
  .system_prompt = NULL,
  .before = NULL,
  .after = NULL,
  .schema = NULL,
  .fields = NULL,
  ...
)
```

## Arguments

- .data:

  A data.frame / tibble.

- output:

  Unquoted name that becomes **the new column** (generative) *or* **the
  prefix** for embedding columns.

- prompt:

  Optional glue template string for a single user turn; reference any
  columns in `.data` (e.g. `"{id}. {question}\nContext: {context}"`).
  Ignored if `.messages` is supplied.

- .messages:

  Optional **named** character vector of glue templates to build a
  multi-turn message, using roles in
  `c("system","user","assistant","file")`. Values are glue templates
  evaluated per-row; all can reference multiple columns. For multimodal,
  use role `"file"` with a column containing a path template.

- .config:

  An
  [llm_config](https://asanaei.github.io/LLMR/reference/llm_config.md)
  object (generative or embedding).

- .system_prompt:

  Optional system message sent with every request when `.messages` does
  not include a `system` entry.

- .before, .after:

  Standard
  [dplyr::relocate](https://dplyr.tidyverse.org/reference/relocate.html)
  helpers controlling where the generated column(s) are placed.

- .schema:

  Optional JSON Schema (R list). When provided, this schema is sent to
  the provider for strict validation and used for local parsing. When
  `NULL`, only JSON mode is enabled (no strict schema validation). The
  schema should follow JSON Schema specification (e.g., with `type`,
  `properties`, `required`).

- .fields:

  Optional character vector of fields to extract from parsed JSON.
  Supports:

  - Character vector: `c("name", "score")` - extract these fields

  - Named vector: `c(person_name = "name", rating = "score")` - extract
    and rename

  - Nested paths: `c("user.name", "/data/items/0")` - dot notation or
    JSON Pointer

  - `NULL` (default): auto-extracts all top-level properties from
    `.schema`

  - `FALSE`: skip field extraction (keep only `structured_data`
    list-column)

- ...:

  Passed to the underlying calls:
  [`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md)
  in generative mode,
  [`get_batched_embeddings()`](https://asanaei.github.io/LLMR/reference/get_batched_embeddings.md)
  in embedding mode.

## Shorthand syntax

Like
[`llm_mutate()`](https://asanaei.github.io/LLMR/reference/llm_mutate.md),
this function supports shorthand syntax:


    df |> llm_mutate_structured(result = "{text}", .schema = schema)
    df |> llm_mutate_structured(result = c(system = "Be brief.", user = "{text}"), .schema = schema)
