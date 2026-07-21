## Submission

LLMR 0.8.11 adds OpenRouter and a focused set of corrections described in
NEWS.md. Request hashes now distinguish multimodal attachments, embedding
batch failures are surfaced, invalid Anthropic thinking budgets fail before a
request is built, ordered agreement metrics require a defensible category
order, audit-log destinations are validated when enabled, and experiment and
tool-loop objects expose their documented reporting and provenance contracts.
Ten non-core helpers remain available inside the package but are no longer
exported.

## Test environments

- local macOS (R 4.4.3)
- R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 2 notes

Both notes are environmental, not package issues:

- "checking for future file timestamps ... NOTE (unable to verify current time)":
  a clock-comparison artifact of the local build machine; it does not appear on
  CRAN's check machines.
- "checking HTML version of manual ... NOTE": emitted by an older system `tidy`
  that does not recognize valid HTML5 elements (`<main>`, `<details>`) in the help
  pages R itself generates; it does not reproduce on CRAN.
- The `man/anes_2024_personas.Rd` \source URL (electionstudies.org) may be
  flagged as "possibly invalid" because that host returns HTTP 403 to automated
  requests; it is correct and reachable in a browser.

## Reverse dependencies

None on CRAN. Several of the author's other packages depend on LLMR and will be
submitted after it is accepted.
