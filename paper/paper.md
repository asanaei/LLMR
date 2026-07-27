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
and generation settings. The LLMR family comprises six R packages. Its
core reaches more than a dozen hosted providers plus local models through
Ollama, with dedicated embedding endpoints. The same interface applies an
instruction across a data frame of texts or turns those texts into a factorial
comparison whose rows execute in parallel.

The family treats embeddings as measurements of text. A corpus becomes a
matrix of vectors for similarity and clustering. Those vectors can then enter
ideological or thematic scaling and downstream statistical work, while
retrieval-augmented generation remains another application of the same
representation. Companion packages place model agents with personas and
memory under explicit budgets into designed conversations. Others administer
surveys and estimate conjoint effects for panels built from real survey
microdata, or convene moderated focus groups under chosen turn-taking rules
and rerun a specified next turn after an earlier message changes. A further
package validates model coding against held-out human labels. One configuration and
common result conventions connect these operations, so the first exploratory
call and the finished study belong to one system.

# Statement of need

Researchers can use language models as text annotators
[@gilardi2023; @ziems2024] or simulated respondents [@argyle2023]. They can
also use them as interactive agents and vector encoders. Each use turns a model
response into evidence through a research design. Quantitative content
analysis requires a codebook and tests against human coding
[@krippendorff2018]. Survey experiments require recorded randomization, and
conjoint estimates depend on the profiles assigned to each respondent
[@hainmueller2014]. Agent studies must specify memory and tool powers as well
as stopping rules. Moderated discussions add a guide and a speaker-selection
process.

Provider-specific APIs make these requirements costly to reproduce across
models. A change of provider can otherwise alter request construction, result
fields, and error handling at the same time that the researcher is varying a
substantive condition. Several R packages wrap LLM APIs; this family
contributes functions that make social science research with LLMs practical.
Within R [@rcore], it joins provider-independent calls to experimental
construction and domain methods. Built for piloting instruments before
fielding them to people, the persona-panel and focus-group workflows preserve
assigned personas and interaction rules as study data.

# Design

A researcher can begin with `call_llm()`, apply the same instruction to a
vector with `llm_fn()`, and add responses to rows with `llm_mutate()`.
`build_factorial_experiments()` crosses model and prompt conditions with system
messages and repetitions. `call_llm_par()` executes that design concurrently,
retaining each condition beside its response. Failed rows can be resumed.
Structured fields, timing, and token use travel with the results, while JSONL
logs record requests and stable hashes.

Embedding studies use the same `llm_config()` class and dedicated provider
routes. `get_batched_embeddings()` converts texts to numeric vectors in
batches. Researchers can calculate semantic proximity or cluster documents.
They can construct ideological or thematic scales before joining
vector-derived measures to other variables. They can also use those vectors to
retrieve passages for a generative model. This common configuration lets a
study compare embedding models or move between a hosted endpoint and Ollama
while preserving the analysis code.

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
interviews or debates, with deliberations and general multi-agent exchanges
available through the same result structure. Factorial experiments repeat
fresh procedures across conditions. Run records separate utterances from
events and calls; tool use and state changes have distinct views.

Persona panels are sampled from population margins or complete microdata
rows, including a bundled dataset derived from the ANES 2024 survey. LLMRpanel
administers Likert and choice items alongside open responses, records respondent-level order assignments, and compares response
shares with supplied human data. For conjoint studies, it stores each profile
shown to each persona and estimates average marginal component effects with
standard errors clustered by persona.

The same personas can sit in a FocusGroup session. A moderator guide
controls the phases, while round-robin and probabilistic rules provide
specified speaker sequences. A desire-based rule makes speaker selection
responsive to the transcript. Continuation experiments rerun the next turn
under an original history and a history in which one earlier message changed,
turning local conversational influence into an explicit contrast.

Content analysis starts from a codebook: candidate protocols compete on a
development split, and the selected protocol is locked before testing against
held-out human labels. LLMRcontent then
codes the corpus and uses matched errors to estimate corrected category
prevalence with standard errors. Audits recompute an estimator across prompts
and models, with label order and temperature available as additional design
axes. Hash-sealed archives connect these measurements to the underlying LLMR
call records.

Across all six packages, classed results expose analysis tables plus
`diagnostics()` and `report()` methods. Configuration objects pass into the
method packages unchanged. Logs and identifiers connect calls to study
artifacts, and about 3,100 tests exercise the family.

# Graphical interfaces

Three Shiny studios [@shiny] make these workflows available point and
click. Content Studio develops a coding protocol and displays its
validation. Panel Studio constructs personas, administers instruments, and
estimates conjoint effects. Focus Studio runs discussions, analyzes
transcripts, and compares altered continuations. LLMR.shiny supplies their
common provider controls and usage displays. Each studio includes an offline
demonstration mode with marked example results.

# Use in research and teaching

One application combines retrieval over oral histories with generation
[@karimzadeh2025]. A related working paper uses design depth and
model autonomy to evaluate social science applications of LLMs
[@sanaei2025depth]. In teaching, *Unsupervised Learning for Social Scientists*
uses LLMR to introduce contextual embeddings [@sanaeiUML4SS]. Offline runners
and studio demonstrations let students inspect factorial experiments,
simulations, and validated coding workflows.

# AI usage disclosure

The author used large language models for code generation and refactoring, as
well as test scaffolding and documentation drafting. This assistance included
copy-editing of the paper. Anthropic Claude models were used through the Claude
Code environment, and OpenAI GPT-5-series models were used through the Codex
command-line tool. The author framed the problems, made the design decisions, and reviewed,
edited, and validated all output, including every statistical procedure.

# References
