---
title: "LLMR: A unified interface for research with Large Language Models in R"
date: 2026-05-01
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
LLMR is an R package for research workflows that use large language models across multiple providers. It provides a common interface for text generation, embeddings, structured extraction, chat-style interaction, and batch execution while keeping prompts, model settings, and outputs inside ordinary R objects and tidy data workflows [@llmr_cran; @wickham2019welcome]. This enables researchers to compare prompts, models, and parameter settings without rewriting downstream analysis code.

LLMR is available on CRAN and supports both hosted providers and local Ollama models [@llmr_cran]. The package also includes helpers for repeated or parallel calls, structured output parsing, and experiment-oriented workflows intended for comparative research in R rather than single-provider application development.

## Statement of need
Researchers increasingly use large language models for text classification, field extraction, embeddings, annotation, and comparative evaluation. In R, existing tools address this space from different angles, including direct provider access, local inference, chat-oriented interfaces, and broader multi-provider packages [@openai_r; @ollamar_joss; @rollama_cran; @tidychatmodels_rapp; @tidyllm_cran; @ellmer]. For many research tasks, the main challenge is not obtaining a single response but designing reproducible workflows in which prompts, model settings, and outputs can be stored as data, crossed into experimental designs, and analyzed with ordinary R tools [@wickham2019welcome]. LLMR addresses this need by providing provider-agnostic configurations, batch and tidy helpers, structured output handling, and embeddings in a single workflow-oriented package [@llmr_cran]. The target users are researchers and instructors who need to compare models or prompts, run repeated calls at scale, or integrate model outputs into downstream analysis in R.

## State of the field
Several R packages already address parts of this problem space. `openai` provides direct access to the OpenAI API [@openai_r]. `ollamar` and `rollama` focus on local Ollama-based workflows [@ollamar_joss; @rollama_cran]. `tidychatmodels` provides a common interface for chat-style interactions across vendors [@tidychatmodels_rapp]. `tidyllm` and `ellmer` provide broader multi-provider interfaces and higher-level workflows such as batch requests, structured extraction, and tool use [@tidyllm_cran; @ellmer].

LLMR was created because the main design goal here is narrower and more research-specific: treating prompts, model settings, and responses as data that can be crossed into factorial designs, executed in batches, parsed into structured outputs, and analyzed in tidy pipelines [@wickham2019welcome]. Contributing this approach to an existing package would likely have required a material shift in scope toward comparative experiment management and R-native research workflows. LLMR therefore occupies a distinct niche for users who need provider-agnostic experimental workflows rather than only direct API access, local inference, or chat application tooling.

## Software Design
LLMR is organized around a small set of stable abstractions: a model configuration object, a unified call layer, provider-specific methods behind that layer, and separate parsing or post-processing steps. This design trades some provider-specific expressiveness for a consistent interface that preserves downstream analysis code when a researcher switches models or vendors. The package deliberately keeps chat, tidy data workflows, factorial experiments, structured calls, and embedding calls close to the same core call mechanism instead of defining separate interface families. That choice reduces duplicated logic and keeps retry behavior, caching, parallel execution, and response handling consistent across one-off calls and repeated experiments.

A second design decision is to separate provider-aware request formatting from local parsing and validation. Providers expose different conventions for structured output, embeddings, and response metadata, but LLMR aims to return objects that can be handled predictably inside R and then converted to ordinary vectors, lists, matrices, or data-frame columns. This matters for comparative research, where prompts, providers, and response formats often change repeatedly during the same project. A stable workflow layer lowers the cost of those changes and reduces the need to rewrite analysis code between experiments. The package therefore prioritizes cross-provider workflow stability over exposing every provider-specific feature through a bespoke interface.

## Research Impact Statement
LLMR is best characterized at present as having credible near-term significance rather than extensive realized impact. The package is available on CRAN, has public source code and documentation, includes vignettes covering chat workflows, structured output, experiments, and embeddings, and is accompanied by automated checks and a public issue tracker [@llmr_cran]. These are community-readiness signals rather than downstream citations, but they matter for a package whose main contribution is methodological infrastructure. LLMR lowers the cost of designing comparative studies across providers, prompts, and parameter settings while keeping those workflows in the R environment used for data preparation, modeling, and analysis. That makes the package potentially useful in computational social science, digital humanities, text analysis, teaching, and other research settings where the same project may combine generation, extraction, embeddings, and structured post-processing. Although the current evidence base is still early, the package is reviewable, reusable, and positioned for uptake by researchers who need reproducible large language model workflows in R.

## AI usage disclosure
Generative AI tools were used to assist with documentation and manuscript revision for this submission, including section drafting, copy-editing, and bibliography cleanup. In the development of the package, too, AI tools were used to speed up code writing, and to write help files.

All AI-assisted outputs were reviewed, edited, and validated by the author, who remained responsible for the package design, implementation, and final wording.

## Acknowledgements
The author thanks the R community for feedback and contributions to the package development.

## References

