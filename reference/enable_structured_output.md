# Enable Structured Output (Provider-Agnostic)

Turn on structured output for a model configuration. Supports
OpenAI‑compatible providers (OpenAI, Groq, Together, x.ai, DeepSeek),
Anthropic, and Gemini.

## Usage

``` r
enable_structured_output(
  config,
  schema = NULL,
  name = "llmr_schema",
  method = c("auto", "json_mode", "tool_call"),
  strict = TRUE
)
```

## Arguments

- config:

  An
  [llm_config](https://asanaei.github.io/LLMR/reference/llm_config.md)
  object.

- schema:

  A named list representing a JSON Schema. If `NULL`, OpenAI-compatible
  providers enforce a JSON object; Gemini switches to JSON mime type;
  Anthropic only injects a tool when a schema is supplied.

- name:

  Character. Schema/tool name for providers requiring one. Default
  "llmr_schema".

- method:

  One of c("auto","json_mode","tool_call"). "auto" chooses the best per
  provider. You rarely need to change this.

- strict:

  Logical. Request strict validation when supported (OpenAI-compatible).

## Value

Modified `llm_config`.
