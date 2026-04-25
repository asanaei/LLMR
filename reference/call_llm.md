# Call an LLM (chat/completions or embeddings) with optional multimodal input

`call_llm()` dispatches to the correct provider implementation based on
`config$provider`. It supports both generative chat/completions and
embeddings, plus a simple multimodal shortcut for local files.

## Usage

``` r
call_llm(config, messages, verbose = FALSE)

# S3 method for class 'ollama'
call_llm(config, messages, verbose = FALSE)
```

## Arguments

- config:

  An
  [`llm_config`](https://asanaei.github.io/LLMR/reference/llm_config.md)
  object.

- messages:

  One of:

  - Plain character vector — each element becomes a `"user"` message.

  - Named character vector — names are roles (`"system"`, `"user"`,
    `"assistant"`). **Multimodal shortcut:** include one or more
    elements named `"file"` whose values are local paths; consecutive
    `{user | file}` entries are combined into one user turn and files
    are inlined (base64) for capable providers.

  - List of message objects: `list(role=..., content=...)`. For
    multimodal content, set `content` to a list of parts like
    `list(list(type="text", text="..."), list(type="file", path="..."))`.

- verbose:

  Logical. If `TRUE`, prints the full parsed API response.

## Value

- Generative mode: an `llmr_response` object. Use `as.character(x)` to
  get just the text; `print(x)` shows text plus a status line; use
  helpers `finish_reason(x)` and `tokens(x)`.

- Embedding mode: provider-native list with an element `data`; convert
  with
  [`parse_embeddings()`](https://asanaei.github.io/LLMR/reference/parse_embeddings.md).

## Provider notes

- **OpenAI-compatible:** On a server 400 that identifies the bad
  parameter as `max_tokens`, LLMR will, unless `no_change=TRUE`, retry
  once replacing `max_tokens` with `max_completion_tokens` (and inform
  via a `cli_alert_info`). The former experimental "uncapped retry on
  empty content" is *disabled* by default to avoid unexpected costs.

- **Anthropic:** `max_tokens` is required; if omitted LLMR uses `2048`
  and warns. Multimodal images are inlined as base64. Extended thinking
  is supported: provide `thinking_budget` and `include_thoughts = TRUE`
  to include a `content` block of type `"thinking"` in the response;
  LLMR sets the beta header automatically.

- **Gemini (REST):** `systemInstruction` is supported; user parts use
  `text`/`inlineData(mimeType,data)`; responses are set to
  `responseMimeType = "text/plain"`.

- **Ollama (local):** OpenAI-compatible endpoints on
  `http://localhost:11434/v1/*`; no Authorization header is required.
  Override with `api_url` as needed.

- **Error handling:** HTTP errors raise structured conditions with
  classes like `llmr_api_param_error`, `llmr_api_rate_limit_error`,
  `llmr_api_server_error`; see the condition fields for status, code,
  request id, and (where supplied) the offending parameter.

## Message normalization

See the *"multimodal shortcut"* described under `messages`. Internally,
LLMR expands these into the provider's native request shape and
tilde-expands local file paths.

## Using a local Ollama server

Ollama provides an OpenAI-compatible HTTP API on localhost by default.
Start the daemon and pull a model first (terminal): `ollama serve` (in
background) and `ollama pull llama3`. Then configure LLMR with
`llm_config("ollama", "llama3", embedding = FALSE)` for chat or
`llm_config("ollama", "nomic-embed-text", embedding = TRUE)` for
embeddings. Override the endpoint with `api_url` if not using the
default `http://localhost:11434/v1/*`.

## See also

[`llm_config`](https://asanaei.github.io/LLMR/reference/llm_config.md),
[`call_llm_robust`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md),
[`llm_chat_session`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md),
[`parse_embeddings`](https://asanaei.github.io/LLMR/reference/parse_embeddings.md),
[`finish_reason`](https://asanaei.github.io/LLMR/reference/llmr_response.md),
[`tokens`](https://asanaei.github.io/LLMR/reference/llmr_response.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## 1) Basic generative call
cfg <- llm_config("openai", "gpt-4o-mini")
call_llm(cfg, "Say hello in Greek.")

## 2) Generative with rich return
r <- call_llm(cfg, "Say hello in Greek.")
r
as.character(r)
finish_reason(r); tokens(r)

## 3) Anthropic extended thinking (single example)
a_cfg <- llm_config("anthropic", "claude-sonnet-4-20250514",
                    max_tokens = 5000,
                    thinking_budget = 16000,
                    include_thoughts = TRUE)
r2 <- call_llm(a_cfg, "Compute 87*93 in your head. Give only the final number.")
# thinking (if present): r2$raw$content[[1]]$thinking
# final text:            r2$raw$content[[2]]$text

## 4) Multimodal (named-vector shortcut)
msg <- c(
  system = "Answer briefly.",
  user   = "Describe this image in one sentence.",
  file   = "~/Pictures/example.png"
)
call_llm(cfg, msg)

## 5) Embeddings
e_cfg <- llm_config("voyage", "voyage-large-2",
                    embedding = TRUE)
emb_raw <- call_llm(e_cfg, c("first", "second"))
emb_mat <- parse_embeddings(emb_raw)

## 6) With a chat session
ch <- chat_session(cfg)
ch$send("Say hello in Greek.")   # prints the same status line as `print.llmr_response`
ch$history()
} # }
```
