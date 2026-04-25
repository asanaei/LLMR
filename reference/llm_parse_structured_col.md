# Parse structured fields from a column into typed vectors

Extracts fields from a column containing structured JSON (string or
list) and appends them as new columns. Adds `structured_ok` (logical)
and `structured_data` (list).

## Usage

``` r
llm_parse_structured_col(
  .data,
  fields,
  structured_col = "response_text",
  prefix = "",
  allow_list = TRUE
)
```

## Arguments

- .data:

  data.frame/tibble

- fields:

  Character vector of fields or named vector (dest_name = path).

- structured_col:

  Column name to parse from. Default "response_text".

- prefix:

  Optional prefix for new columns.

- allow_list:

  Logical. If TRUE (default), non-scalar values (arrays/objects) are
  hoisted as list-columns instead of being dropped. If FALSE, only
  scalar fields are hoisted and non-scalars become NA.

## Value

`.data` with diagnostics and one new column per requested field.

## Details

- Supports nested-path extraction via dot/bracket paths (e.g.,
  `a.b[0].c`) or JSON Pointer (`/a/b/0/c`).

- When `allow_list = TRUE`, non-scalar values become list-columns;
  otherwise they yield `NA` and only scalars are hoisted.
