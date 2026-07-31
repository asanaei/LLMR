---
title: "LLMR: An R Package Family for Research with Large Language Models"
tags:
  - R
  - large language models
  - computational social science
  - reproducible research
  - content analysis
authors:
  - name: Ali Sanaei
    orcid: 0000-0000-0000-0000
    affiliation: 1
affiliations:
  - name: University of Chicago, USA
    index: 1
date: 27 July 2026
bibliography: paper.bib
---

# Summary

A researcher can move from one model call to a complete social science study
while retaining the configuration object that identifies the provider, model,
and generation settings. The LLMR family comprises six R packages, and its
core reaches more than a dozen hosted providers as well as local models
through Ollama and provides dedicated embedding endpoints. Through the same
interface, a researcher can apply one instruction across a data frame of
texts or turn those texts into a factorial comparison whose rows run in
parallel.

The family treats embeddings as measurements of text, and a corpus becomes a
matrix of vectors for similarity and clustering. Researchers can then use
those vectors for ideological or thematic scaling and subsequent statistical
analysis, while retrieval-augmented generation remains another application of
the same representation. Companion packages place model agents in designed
conversations, and the design specifies their personas, memory, and explicit
budgets. One package administers surveys and estimates conjoint effects for
panels built from real survey microdata; another convenes moderated focus
groups under chosen turn-taking rules and reruns a specified next turn after
an earlier message changes. A further package validates model coding against
held-out human labels. One configuration and common result conventions
connect these operations, so a researcher can use the same system for the
first exploratory call and for the finished study.

# Statement of need

Researchers can use language models as text annotators
[@gilardi2023; @ziems2024] or simulated respondents [@argyle2023], and they
can also use them as interactive agents and as vector encoders. Each use turns
a model response into evidence through a research design. Quantitative content
analysis requires a codebook and tests against human coding
[@krippendorff2018]. Survey experiments require recorded randomization, and
conjoint estimates depend on the profiles assigned to each respondent
[@hainmueller2014]. Agent studies must specify memory and tool powers as well
as stopping rules, and moderated discussions add a guide and a
speaker-selection process.

Provider-specific APIs make research designs that satisfy these requirements
costly to reproduce across models. A change of provider can otherwise alter request construction, result
fields, and error handling at the same time that the researcher is varying a
substantive condition. Although several R packages wrap LLM APIs, this family contributes functions
that make social science research with LLMs practical.
Within R [@rcore], the family lets researchers make provider-independent
calls, construct experiments, and apply domain methods. Researchers can use the
persona-panel and focus-group workflows to pilot instruments before fielding
them to people, and the workflows preserve assigned personas and interaction
rules as study data.

# Design

A researcher can begin with `call_llm()`, apply the same instruction to a
vector with `llm_fn()`, and add responses to rows with `llm_mutate()`.
`build_factorial_experiments()` crosses model and prompt conditions with system
messages and repetitions. `call_llm_par()` executes that design concurrently and retains each condition
beside its response; a researcher can resume failed rows.
The results retain structured fields, timing, and token use, while JSONL
logs record requests and stable hashes.

Embedding studies use the same `llm_config()` class and dedicated provider
routes. `get_batched_embeddings()` converts texts to numeric vectors in
batches, and researchers can then calculate semantic proximity or cluster
documents. They can construct ideological or thematic scales before joining
vector-derived measures to other variables. They can also use those vectors to
retrieve passages for a generative model. This common configuration lets
researchers compare embedding models or move between a hosted endpoint and
Ollama while preserving the analysis code.

| Package and version | Research operation | Recorded product |
|---|---|---|
| LLMR 0.8.11 | Calls generative and embedding models; runs factorial designs | Responses and vectors; usage and call records |
| LLMR.shiny 0.1.2 | Supplies shared controls for three studios | Provider settings, run plans, and marked demonstrations |
| LLMRagent 0.8.1 | Constructs agents and designed conversations | Transcripts and events; tool use and state |
| LLMRcontent 0.2.0 | Builds and validates text-coding instruments | Labels and validation results; corrected estimates and archives |
| LLMRpanel 0.6.0 | Administers surveys and conjoint experiments to personas | Assignments and responses; comparisons and AMCE estimates |
| FocusGroup 0.5.1 | Runs moderated sessions and continuation experiments | Attributed transcripts, usage, and paired continuations |

LLMRagent builds a participant by attaching a persona and a memory policy
to the common configuration. Declared tools and budgets make an agent's
available actions part of the design. Conversations can take the form of
interviews, debates, deliberations, or general multi-agent exchanges, and
each form uses the same result structure. Factorial experiments repeat
each procedure afresh across conditions; their run records separate
utterances from events and calls and provide distinct views of tool use and
state changes.

Persona panels are sampled from population margins or complete microdata
rows, including a bundled dataset derived from the ANES 2024 survey. LLMRpanel
administers Likert and choice items, collects open responses, records
respondent-level order assignments, and compares response shares with
supplied human data. For conjoint studies, it stores each profile
shown to each persona and estimates average marginal component effects with
standard errors clustered by persona.

The same personas can sit in a FocusGroup session, where a moderator guide
controls the phases and round-robin or probabilistic rules provide specified
speaker sequences; a desire-based rule makes speaker selection responsive to
the transcript. Continuation experiments rerun the next turn under the
original history and under a history in which one earlier message changed, so
that the two continuations provide an explicit contrast for local
conversational influence.

Content analysis starts from a codebook, and researchers compare candidate
protocols on a development split; they lock the selected protocol before
testing it against held-out human labels. LLMRcontent then
codes the corpus and uses matched errors to estimate corrected category
prevalence with standard errors. Audits recompute an estimator across prompts
and models, and researchers can also vary label order and temperature as
additional design axes. Hash-sealed archives connect these measurements to the underlying LLMR
call records.

Across all six packages, classed results provide analysis tables and support
`diagnostics()` and `report()` methods. Configuration objects pass into the
method packages unchanged, and logs and identifiers connect calls to study
artifacts. A suite of about 3,100 tests exercises the six packages in the
family.

# Graphical interfaces

Three Shiny studios [@shiny] provide point-and-click interfaces through
which researchers can carry out these workflows.
Content Studio develops a coding protocol and displays its validation; Panel
Studio constructs personas, administers instruments, and estimates conjoint
effects; and Focus Studio runs discussions, analyzes transcripts, and
compares altered continuations. LLMR.shiny supplies their common provider
controls and usage displays, and each studio includes an offline
demonstration mode with marked example results.

# Use in research and teaching

One application combines retrieval over oral histories with generation
[@karimzadeh2025], and a related working paper uses design depth and model
autonomy to evaluate social science applications of LLMs [@sanaei2025depth]. In teaching, *Unsupervised Learning for Social Scientists*
uses LLMR to introduce contextual embeddings [@sanaeiUML4SS]. Offline runners
and studio demonstrations let students inspect factorial experiments,
simulations, and validated coding workflows.

# AI usage disclosure

The author used large language models for code generation and refactoring,
for test scaffolding and documentation drafting, and for copy-editing of the
paper. The author used Anthropic Claude models through the Claude Code
environment and OpenAI GPT-5-series models through the Codex command-line
tool. The author framed the problems, made the design decisions, and reviewed,
edited, and validated all output, including every statistical procedure.

# References
