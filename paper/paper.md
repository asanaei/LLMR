---
title: "LLMR: A unified interface for research with Large Language Models in R"
date: 2025-11-03
authors:
  - name: Ali Sanaei
    orcid: 0000-0002-8513-8169
    affiliation: 1
affiliations:
  - name: Computational Social Science, University of Chicago
    index: 1
bibliography: paper.bib
---

## Summary
LLMR is an R package that provides researchers with a single provider-agnostic interface for generation and embeddings independently and also in inside tidy data workflows. The design enables cross model studies, parameter sweeps, and structured extraction within familiar R idioms. LLMR supports Ollama, OpenAI, Anthropic, Gemini, Groq, DeepSeek, xAI, Together AI, and Voyage AI, and extending the support to new providers is straightforward through writing an S3 call method.
LLMR integrates with tidyverse, runs jobs in parallel, caches repeated calls, and offers structured output via JSON Schema. The package is available on CRAN. The source code and the development version are hosted on GitHub. 

## Statement of need
Researchers now employ large language models to embed text, label text, extract fields, conduct vignette scale experiments, and compare models across prompts and parameters. In R, many available tools bind to a single provider or require people to rewrite code when changing models, which slows comparisons and makes replication difficult [@openai_r]. Local deployments via Ollama are common in privacy sensitive work and teaching, yet local and hosted workflows have previously been referred to separate packages [@ollamar_joss; @rollama_arxiv]. LLMR addresses these barriers by treating model calls as data inside tidy pipelines [@wickham2019welcome]. Users define configurations for providers, run model calls as vectorized or row-wise operations, and analyze the resulting columns. This approach would make systematic comparisons and factorial experiments straightforward to set up, and thus researchers retain full control of prompts and parameters while keeping the same idiom for analysis. Similarly, performing combinations of text embedding and text generation is smooth within LLMR.

## Features
LLMR provides a consistent configuration object for models and providers, a standard response with finish reasons and token counts, and a tidy pipeline that makes calls over vectors or data frames. It supports structured output using provider switches for JSON Schema, with local parsing and optional validation. Parallel execution uses the future package to scale out jobs, caching can reduce repeated cost during iterative analysis, and error handling applies retries with backoff and records basic diagnostics [@bengtsson2021parallelly; @memoise]. For structured output, OpenAI compatible endpoints accept response_format with json_schema [@openai_structured], Anthropic uses tool definitions with an input_schema [@anthropic_tools], and Gemini supports response_mime_type and response_schema for JSON output [@gemini_structured]. In fact the configuration, execution, and parsing steps are kept separate, and thus each portion of the workflow can be replaced or extended without modifying the rest.

## Design Logic
All workflows need one or more model configurations via `llm_config`, after that, the model can be called via 
`call_llm` for either embedding or generative calls. `llm_config` tries to resolve the API key from enviornment variables
through a sensible lookup table based on provider ('OPENAI_API_KEY', 'GEMINI_API_KEY', and so on), but can also be directly provided, either as the actual key string (which should be discouraged) or as the name of an environmental variable. 

Provider implementations are S3 methods of `call_llm` but given that most providers follow Open AI's specifications, unsupported providers can typically be called using `provider='openai'` but with 
an overwriting of the `api_url` argument. The rest of the package is essentially wrappers around the `call_llm` function. The first layer is a `call_llm_robust`.

- A stateful chat object can be created by `chat_session` which provides an object with methods like `$send` and `$print`. 
- A typical experimental workflow builds an experiment data frame where each row becomes an API call (this can be done via the helper function `build_factorial_experiments`) and then call `call_llm_par` (or `call_llm_par_structured` for structured calls). 
- A tidy implementation can use the `llm_mutate` (or `llm_mutate_structured` for structured calls).
- Finally, a typical embedding call can be done via `get_batched_embeddings`.

Using the future package [@bengtsson2021parallelly] all functions that needs to make more than one api call can be parallized and given the low local computational cost, except for locally run models, there is a considerable speed boost unless a rate-limit is hit from the provider side. 

Structured output follows is implemented differently by different providers, and is presently better supported for Open AI and Anthropic models [@openai_structured, @anthropic_tools]. Gemini accepts response_mime_type for JSON and can take response_schema to constrain the shape [@gemini_structured]. The package also parses and validates JSON locally to keep behavior consistent across providers. Parallel execution uses , and caching during development uses memoise [@memoise]. Error handling applies retries with exponential backoff and records concise diagnostics.

## Related work
In R, single‑provider packages such as openai give direct access to one API [@openai_r]. Several packages target local models through Ollama [@ollamar_joss; @rollama_arxiv]. Others provide a tidy interface for chats across vendors [@tidyllm_cran; @tidychatmodels_rapp]. The Ellmer package provides (younger than LLMR) provides cross-platform support and now supports parallization and structured outputs, but lacks support for embeddings, and its design objective is not scientific experimentation. 

These tools serve important use cases. LLMR's distinct focus is a unified research workflow inside R that treats model calls as data, supports parallel factorial designs and parameter sweeps, and offers provider‑aware structured output with local validation. Libraries in other languages, such as LangChain, give multi‑provider abstractions in Python and JavaScript, but they do not integrate with R's data analysis idiom [@langchain_python].

## Quality control
The package includes some unit tests for message normalization, structured parsing and validation, and parallel utilities. Continuous integration runs on Linux, macOS, and Windows. Examples that call external APIs are guarded by environment variables to avoid unintended network use during checks. LLMR is available on CRAN and passes routine CRAN checks for the current release.

## Conclusion
LLMR simplifies research with and research about large language models. It makes cross‑provider model studies simple inside R by turning model calls into data that can be analyze with standard tools. The package covers hosted and local models, supports structured output, and offers parallel execution with caching and retries, which helps researchers run careful comparisons at scale.

## Acknowledgements
The author thanks the R community for feedback and contributions to the package development.

## References
