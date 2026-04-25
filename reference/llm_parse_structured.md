# Parse structured output emitted by an LLM

Robustly parses an LLM's structured output (JSON). Works on character
scalars or an
[llmr_response](https://asanaei.github.io/LLMR/reference/llmr_response.md).
Strips code fences first, then tries strict parsing, then attempts to
extract the largest balanced {...} or \[...\].

## Usage

``` r
llm_parse_structured(x, strict_only = FALSE, simplify = FALSE)
```

## Arguments

- x:

  Character or
  [llmr_response](https://asanaei.github.io/LLMR/reference/llmr_response.md).

- strict_only:

  If TRUE, do not attempt recovery via substring extraction.

- simplify:

  Logical passed to jsonlite::fromJSON (`simplifyVector = FALSE` when
  FALSE).

## Value

A parsed R object (list), or NULL on failure.

## Details

The return contract is list-or-NULL; scalar-only JSON is treated as
failure.

Numerics are coerced to double for stability.
