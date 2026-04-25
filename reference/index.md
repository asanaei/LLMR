# Package index

## Core

- [`llm_config()`](https://asanaei.github.io/LLMR/reference/llm_config.md)
  : Create an LLM configuration (provider-agnostic)
- [`call_llm()`](https://asanaei.github.io/LLMR/reference/call_llm.md) :
  Call an LLM (chat/completions or embeddings) with optional multimodal
  input
- [`call_llm_robust()`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md)
  : Robustly Call LLM API (Simple Retry)
- [`finish_reason()`](https://asanaei.github.io/LLMR/reference/llmr_response.md)
  [`tokens()`](https://asanaei.github.io/LLMR/reference/llmr_response.md)
  [`is_truncated()`](https://asanaei.github.io/LLMR/reference/llmr_response.md)
  [`as.character(`*`<llmr_response>`*`)`](https://asanaei.github.io/LLMR/reference/llmr_response.md)
  [`print(`*`<llmr_response>`*`)`](https://asanaei.github.io/LLMR/reference/llmr_response.md)
  : LLMR Response Object
- [`chat_session()`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md)
  [`as.data.frame(`*`<llm_chat_session>`*`)`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md)
  [`summary(`*`<llm_chat_session>`*`)`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md)
  [`head(`*`<llm_chat_session>`*`)`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md)
  [`tail(`*`<llm_chat_session>`*`)`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md)
  [`print(`*`<llm_chat_session>`*`)`](https://asanaei.github.io/LLMR/reference/llm_chat_session.md)
  : Chat Session Object and Methods

## Structured output

- [`enable_structured_output()`](https://asanaei.github.io/LLMR/reference/enable_structured_output.md)
  : Enable Structured Output (Provider-Agnostic)
- [`disable_structured_output()`](https://asanaei.github.io/LLMR/reference/disable_structured_output.md)
  : Disable Structured Output (clean provider toggles)
- [`llm_parse_structured()`](https://asanaei.github.io/LLMR/reference/llm_parse_structured.md)
  : Parse structured output emitted by an LLM
- [`llm_parse_structured_col()`](https://asanaei.github.io/LLMR/reference/llm_parse_structured_col.md)
  : Parse structured fields from a column into typed vectors
- [`llm_validate_structured_col()`](https://asanaei.github.io/LLMR/reference/llm_validate_structured_col.md)
  : Validate structured JSON objects against a JSON Schema (locally)
- [`llm_fn_structured()`](https://asanaei.github.io/LLMR/reference/llm_fn_structured.md)
  : Vectorized structured-output LLM
- [`llm_mutate_structured()`](https://asanaei.github.io/LLMR/reference/llm_mutate_structured.md)
  : Data-frame mutate with structured output
- [`call_llm_par_structured()`](https://asanaei.github.io/LLMR/reference/call_llm_par_structured.md)
  : Parallel experiments with structured parsing

## Tidy helpers and parallel

- [`llm_fn()`](https://asanaei.github.io/LLMR/reference/llm_fn.md) :
  Apply an LLM prompt over vectors/data frames
- [`llm_mutate()`](https://asanaei.github.io/LLMR/reference/llm_mutate.md)
  : Mutate a data frame with LLM output
- [`build_factorial_experiments()`](https://asanaei.github.io/LLMR/reference/build_factorial_experiments.md)
  : Build Factorial Experiment Design
- [`call_llm_par()`](https://asanaei.github.io/LLMR/reference/call_llm_par.md)
  : Parallel LLM Processing with Tibble-Based Experiments (Core Engine)
- [`call_llm_broadcast()`](https://asanaei.github.io/LLMR/reference/call_llm_broadcast.md)
  : Parallel API calls: Fixed Config, Multiple Messages
- [`call_llm_sweep()`](https://asanaei.github.io/LLMR/reference/call_llm_sweep.md)
  : Parallel API calls: Parameter Sweep - Vary One Parameter, Fixed
  Message
- [`call_llm_compare()`](https://asanaei.github.io/LLMR/reference/call_llm_compare.md)
  : Parallel API calls: Multiple Configs, Fixed Message
- [`setup_llm_parallel()`](https://asanaei.github.io/LLMR/reference/setup_llm_parallel.md)
  : Setup Parallel Environment for LLM Processing
- [`reset_llm_parallel()`](https://asanaei.github.io/LLMR/reference/reset_llm_parallel.md)
  : Reset Parallel Environment

## Embeddings

- [`parse_embeddings()`](https://asanaei.github.io/LLMR/reference/parse_embeddings.md)
  : Parse Embedding Response into a Numeric Matrix
- [`get_batched_embeddings()`](https://asanaei.github.io/LLMR/reference/get_batched_embeddings.md)
  : Generate Embeddings in Batches

## Utilities

- [`bind_tools()`](https://asanaei.github.io/LLMR/reference/bind_tools.md)
  : Bind tools to a config (provider-agnostic)
