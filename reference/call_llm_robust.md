# Robustly Call LLM API (Simple Retry)

Wraps [`call_llm`](https://asanaei.github.io/LLMR/reference/call_llm.md)
to handle rate-limit errors (HTTP 429 or related "Too Many Requests"
messages). It retries the call a specified number of times, using
exponential backoff. You can also choose to cache responses if you do
not need fresh results each time.

## Usage

``` r
call_llm_robust(
  config,
  messages,
  tries = 5,
  wait_seconds = 10,
  backoff_factor = 5,
  verbose = FALSE,
  memoize = FALSE
)
```

## Arguments

- config:

  An `llm_config` object from
  [`llm_config`](https://asanaei.github.io/LLMR/reference/llm_config.md).

- messages:

  A list of message objects (or character vector for embeddings).

- tries:

  Integer. Number of retries before giving up. Default is 5.

- wait_seconds:

  Numeric. Initial wait time (seconds) before the first retry. Default
  is 10.

- backoff_factor:

  Numeric. Multiplier for wait time after each failure. Default is 5.

- verbose:

  Logical. If TRUE, prints the full API response.

- memoize:

  Logical. If TRUE, calls are cached to avoid repeated identical
  requests. Default is FALSE.

## Value

The successful result from
[`call_llm`](https://asanaei.github.io/LLMR/reference/call_llm.md), or
an error if all retries fail.

## See also

[`call_llm`](https://asanaei.github.io/LLMR/reference/call_llm.md) for
the underlying, non-robust API call.
[`cache_llm_call`](https://asanaei.github.io/LLMR/reference/cache_llm_call.md)
for a memoised version that avoids repeated requests.
[`llm_config`](https://asanaei.github.io/LLMR/reference/llm_config.md)
to create the configuration object.
[`chat_session`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md)
for stateful, interactive conversations.

## Examples

``` r
if (FALSE) { # \dontrun{
robust_resp <- call_llm_robust(
config = llm_config("openai","gpt-4o-mini"),
messages = list(list(role = "user", content = "Hello, LLM!")),
tries = 5,
wait_seconds = 10,
memoize = FALSE
)
print(robust_resp)
as.character(robust_resp)
} # }
```
