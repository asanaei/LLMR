# Mutate a data frame with LLM output

Adds one or more columns to `.data` that are produced by a
Large-Language-Model.

## Usage

``` r
llm_mutate(
  .data,
  output,
  prompt = NULL,
  .messages = NULL,
  .config,
  .system_prompt = NULL,
  .before = NULL,
  .after = NULL,
  .return = c("columns", "text", "object"),
  .structured = FALSE,
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

- .return:

  One of `c("columns","text","object")`. For generative mode, controls
  how results are added. `"columns"` (default) adds text plus diagnostic
  columns; `"text"` adds a single text column; `"object"` adds a
  list-column of `llmr_response` objects.

- .structured:

  Logical. If `TRUE`, enables structured JSON output with automatic
  parsing. Requires `.schema` to be provided. When enabled, this is
  equivalent to calling
  [`llm_mutate_structured()`](https://asanaei.github.io/LLMR/reference/llm_mutate_structured.md).
  Default is `FALSE`.

- .schema:

  Optional JSON Schema (R list). When `.structured = TRUE`, this schema
  is sent to the provider for validation and used for local parsing.
  When `NULL`, only JSON mode is enabled (no strict schema validation).

- .fields:

  Optional character vector of fields to extract from parsed JSON.
  Supports nested paths (e.g., `"user.name"` or `"/data/items/0"`). When
  `NULL` and `.schema` is provided, auto-extracts all top-level schema
  properties. Set to `FALSE` to skip field extraction entirely.

- ...:

  Passed to the underlying calls:
  [`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md)
  in generative mode,
  [`get_batched_embeddings()`](https://asanaei.github.io/LLMR/reference/get_batched_embeddings.md)
  in embedding mode.

## Value

`.data` with the new column(s) appended.

## Details

- **Multi-column injection:** templating is NA-safe (`NA` -\> empty
  string).

- **Multi-turn templating:** supply
  `.messages = c(system=..., user=..., file=...)`. Duplicate role names
  are allowed (e.g., two `user` turns).

- **Generative mode:** one request per row via
  [`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md).
  Parallel execution follows the active **future** plan; see
  [`setup_llm_parallel()`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md).

- **Embedding mode:** the per-row text is embedded via
  [`get_batched_embeddings()`](https://asanaei.github.io/LLMR/reference/get_batched_embeddings.md).
  Result expands to numeric columns named `paste0(<output>, 1:N)`. If
  all rows fail to embed, a single `<output>1` column of `NA` is
  returned.

- Diagnostic columns use suffixes: `_finish`, `_sent`, `_rec`, `_tot`,
  `_reason`, `_ok`, `_err`, `_id`, `_status`, `_ecode`, `_param`, `_t`.

## Shorthand

You can supply the output column and prompt in one argument:

    df |> llm_mutate(answer = "{question} (hint: {hint})", .config = cfg)
    df |> llm_mutate(answer = c(system = "One word.", user = "{question}"), .config = cfg)

This is equivalent to:

    df |> llm_mutate(answer, prompt = "{question} (hint: {hint})", .config = cfg)
    df |> llm_mutate(answer, .messages = c(system = "One word.", user = "{question}"), .config = cfg)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

df <- tibble::tibble(
  id       = 1:2,
  question = c("Capital of France?", "Author of 1984?"),
  hint     = c("European city", "English novelist")
)

cfg <- llm_config("openai", "gpt-4o-mini",
                  temperature = 0)

# Generative: single-turn with multi-column injection
df |>
  llm_mutate(
    answer,
    prompt = "{question} (hint: {hint})",
    .config = cfg,
    .system_prompt = "Respond in one word."
  )

# Generative: multi-turn via .messages (system + user)
df |>
  llm_mutate(
    advice,
    .messages = c(
      system = "You are a helpful zoologist. Keep answers short.",
      user   = "What is a key fact about this? {question} (hint: {hint})"
    ),
    .config = cfg
  )

# Multimodal: include an image path with role 'file'
pics <- tibble::tibble(
  img    = c("inst/extdata/cat.png", "inst/extdata/dog.jpg"),
  prompt = c("Describe the image.", "Describe the image.")
)
pics |>
  llm_mutate(
    vision_desc,
    .messages = c(user = "{prompt}", file = "{img}"),
    .config = llm_config("openai","gpt-4.1-mini")
  )

# Embeddings: output name becomes the prefix of embedding columns
emb_cfg <- llm_config("voyage", "voyage-3.5-lite",
                      embedding = TRUE)
df |>
  llm_mutate(
    vec,
    prompt  = "{question}",
    .config = emb_cfg,
    .after  = id
  )

# Structured output: using .structured = TRUE (equivalent to llm_mutate_structured)
schema <- list(
  type = "object",
  properties = list(
    answer = list(type = "string"),
    confidence = list(type = "number")
  ),
  required = list("answer", "confidence")
)

df |>
  llm_mutate(
    result,
    prompt = "{question}",
    .config = cfg,
    .structured = TRUE,
    .schema = schema
  )

# Structured with shorthand
df |>
  llm_mutate(
    result = "{question}",
    .config = cfg,
    .structured = TRUE,
    .schema = schema
  )
} # }
```
