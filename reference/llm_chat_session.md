# Chat Session Object and Methods

Create and interact with a stateful chat session object that retains
message history. This documentation page covers the constructor function
`chat_session()` as well as all S3 methods for the `llm_chat_session`
class.

## Usage

``` r
chat_session(config, system = NULL, ...)

# S3 method for class 'llm_chat_session'
as.data.frame(x, ...)

# S3 method for class 'llm_chat_session'
summary(object, ...)

# S3 method for class 'llm_chat_session'
head(x, n = 6L, width = getOption("width") - 15, ...)

# S3 method for class 'llm_chat_session'
tail(x, n = 6L, width = getOption("width") - 15, ...)

# S3 method for class 'llm_chat_session'
print(x, width = getOption("width") - 15, ...)
```

## Arguments

- config:

  An
  [llm_config](https://asanaei.github.io/LLMR/reference/llm_config.md)
  **for a generative model** (`embedding = FALSE`).

- system:

  Optional system prompt inserted once at the beginning.

- ...:

  Default arguments forwarded to every
  [`call_llm_robust()`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md)
  call (e.g. `verbose = TRUE`).

- x, object:

  An `llm_chat_session` object.

- n:

  Number of turns to display.

- width:

  Character width for truncating long messages.

## Value

For `chat_session()`, an object of class **`llm_chat_session`**. Other
methods return what their titles state.

## Details

The `chat_session` object provides a simple way to hold a conversation
with a generative model. It wraps
[`call_llm_robust()`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md)
to benefit from retry logic, caching, and error logging.

## How it works

1.  A private environment stores the running list of
    `list(role, content)` messages.

2.  At each `$send()` the history is sent *in full* to the model.

3.  Provider-agnostic token counts are extracted from the JSON response.

## Public methods

- `$send(text, ..., role = "user")`:

  Append a message (default role `"user"`), query the model, print the
  assistant's reply, and invisibly return it.

- `$send_structured(text, schema, ..., role = "user", .fields = NULL, .validate_local = TRUE)`:

  Send a message with structured-output enabled using `schema`, append
  the assistant's reply, parse JSON (and optionally validate locally
  when `.validate_local = TRUE`), returning the parsed result invisibly.

- `$history()`:

  Raw list of messages.

- `$history_df()`:

  Two-column data frame (`role`, `content`).

- `$tokens_sent()`/`$tokens_received()`:

  Running token totals.

- `$reset()`:

  Clear history (retains the optional system message).

## See also

[`llm_config()`](https://asanaei.github.io/LLMR/reference/llm_config.md),
[`call_llm()`](https://asanaei.github.io/LLMR/reference/call_llm.md),
[`call_llm_robust()`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md),
[`llm_fn()`](https://asanaei.github.io/LLMR/reference/llm_fn.md),
[`llm_mutate()`](https://asanaei.github.io/LLMR/reference/llm_mutate.md)

## Examples

``` r
if (interactive()) {
  cfg  <- llm_config("openai", "gpt-4o-mini")
  chat <- chat_session(cfg, system = "Be concise.")
  chat$send("Who invented the moon?")
  chat$send("Explain why in one short sentence.")
  chat           # print() shows a summary and first 10 turns
  summary(chat)  # stats
  tail(chat, 2)
  as.data.frame(chat)
}
```
