# Validate structured JSON objects against a JSON Schema (locally)

Adds `structured_valid` (logical) and `structured_error` (chr) by
validating each row's `structured_data` against `schema`. No provider
calls are made.

## Usage

``` r
llm_validate_structured_col(
  .data,
  schema,
  structured_list_col = "structured_data"
)
```

## Arguments

- .data:

  A data.frame with a `structured_data` list-column.

- schema:

  JSON Schema (R list)

- structured_list_col:

  Column name with parsed JSON. Default "structured_data".
