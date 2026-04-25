# Cache LLM API Calls

A memoised version of
[`call_llm`](https://asanaei.github.io/LLMR/reference/call_llm.md) to
avoid repeated identical requests.

## Usage

``` r
cache_llm_call(config, messages, verbose = FALSE)
```

## Arguments

- config:

  An `llm_config` object from
  [`llm_config`](https://asanaei.github.io/LLMR/reference/llm_config.md).

- messages:

  A list of message objects or character vector for embeddings.

- verbose:

  Logical. If TRUE, prints the full API response (passed to
  [`call_llm`](https://asanaei.github.io/LLMR/reference/call_llm.md)).

## Value

The (memoised) response object from
[`call_llm`](https://asanaei.github.io/LLMR/reference/call_llm.md).

## Details

- Requires the `memoise` package. Add `memoise` to your package's
  DESCRIPTION.

- Clearing the cache can be done via `memoise::forget(cache_llm_call)`
  or by restarting your R session.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Using cache_llm_call:
  response1 <- cache_llm_call(my_config, list(list(role="user", content="Hello!")))
  # Subsequent identical calls won't hit the API unless we clear the cache.
  response2 <- cache_llm_call(my_config, list(list(role="user", content="Hello!")))
} # }
```
