# LLMR Response Object

A lightweight S3 container for **generative** model calls. It
standardizes finish reasons and token usage across providers and keeps
the raw response for advanced users.

Returns the standardized finish reason for an `llmr_response`.

Returns a list with token counts for an `llmr_response`.

Convenience check for truncation due to token limits.

## Usage

``` r
finish_reason(x)

tokens(x)

is_truncated(x)

# S3 method for class 'llmr_response'
as.character(x, ...)

# S3 method for class 'llmr_response'
print(x, ...)
```

## Arguments

- x:

  An `llmr_response` object.

- ...:

  Ignored.

## Value

A length-1 character vector or `NA_character_`.

A list `list(sent, rec, total, reasoning)`. Missing values are `NA`.

`TRUE` if truncated, otherwise `FALSE`.

## Details

### Fields

- `text`: character scalar. Assistant reply.

- `provider`: character. Provider id (e.g., `"openai"`, `"gemini"`).

- `model`: character. Model id.

- `finish_reason`: one of `"stop"`, `"length"`, `"filter"`, `"tool"`,
  `"other"`.

- `usage`: list with integers `sent`, `rec`, `total`, `reasoning` (if
  available).

- `response_id`: provider’s response identifier if present.

- `duration_s`: numeric seconds from request to parse.

- `raw`: parsed provider JSON (list).

- `raw_json`: raw JSON string.

### Printing

[`print()`](https://rdrr.io/r/base/print.html) shows the text, then a
compact status line with model, finish reason, token counts, and a terse
hint if truncated or filtered.

### Coercion

[`as.character()`](https://rdrr.io/r/base/character.html) extracts
`text` so the object remains drop-in for code that expects a character
return.

## See also

[`call_llm()`](https://asanaei.github.io/LLMR/reference/call_llm.md),
[`call_llm_robust()`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md),
[`llm_chat_session()`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md),
[`llm_config()`](https://asanaei.github.io/LLMR/reference/llm_config.md),
[`llm_mutate()`](https://asanaei.github.io/LLMR/reference/llm_mutate.md),
[`llm_fn()`](https://asanaei.github.io/LLMR/reference/llm_fn.md)

## Examples

``` r
# Minimal fabricated example (no network):
r <- structure(
  list(
    text = "Hello!",
    provider = "openai",
    model = "demo",
    finish_reason = "stop",
    usage = list(sent = 12L, rec = 5L, total = 17L, reasoning = NA_integer_),
    response_id = "resp_123",
    duration_s = 0.012,
    raw = list(choices = list(list(message = list(content = "Hello!")))),
    raw_json = "{}"
  ),
  class = "llmr_response"
)
as.character(r)
finish_reason(r)
tokens(r)
print(r)
if (FALSE) { # \dontrun{
fr <- finish_reason(r)
} # }
if (FALSE) { # \dontrun{
u <- tokens(r)
u$total
} # }
if (FALSE) { # \dontrun{
if (is_truncated(r)) message("Increase max_tokens")
} # }
```
