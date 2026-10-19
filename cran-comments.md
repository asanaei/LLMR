## Submission

LLMR 0.8.14 updates 0.8.11 on CRAN; versions 0.8.12 and 0.8.13 were
development versions and never submitted, so NEWS.md lists their changes
under their own headings. The release adds the Meta Model API as a provider
and corrects several results that were wrong without notice: experiment
runners no longer evaluate braces in prompts, failed or interrupted streams
raise errors instead of returning partial text as a finished reply, tool
calls with unparseable arguments are not run, Gemini and Anthropic token
counts and cost estimates follow each provider's counting rules, unknown
usage is no longer reported as zero, and an invalid `timeout` is refused
instead of removing the request timeout. The deprecated `llm_methods_text()`
is removed; no package on CRAN calls it.

## Test environments

- local macOS 26.7.1 (aarch64), R 4.4.3
- R CMD check --as-cran --no-manual, with all suggested packages installed

## R CMD check results

0 errors | 0 warnings | 2 notes

Both notes are environmental:

- "checking CRAN incoming feasibility ... NOTE": the `\source` URL in
  `man/anes_2024_personas.Rd` and `inst/CITATION`
  (https://electionstudies.org/data-center/2024-time-series-study/) is
  reported as possibly invalid because that host returns HTTP 403 to
  automated requests. The URL is correct and opens in a browser.
- "checking for future file timestamps ... NOTE (unable to verify current
  time)": the local machine could not reach a time server.

## Reverse dependencies

The CRAN versions of all four reverse dependencies were checked against this
version with `R CMD check`, with Status OK for each: LLMR.shiny 0.1.2,
LLMRagent 0.8.1, LLMRpanel 0.6.1, and FocusGroup 0.5.2 (all this
maintainer's). Updates of these packages follow this submission.
