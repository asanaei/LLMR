## Submission

This release of LLMR (0.8.10) rolls up the additive helpers of 0.8.7-0.8.9
(the bundled `anes_2024_personas` example data and persona helpers, the
`transcript_as_messages()` / `ensure_alternating_messages()` message helpers,
the audit-log readers `llm_log_read()` / `llm_request_from_log()`, the shared
`reset()` generic, `llm_uuid()`, `llm_tool_signature()`, `llm_log_active()`,
`llm_add_request_hash()`, and `llm_agreement(metric=)`) together with a set of
bug fixes (see NEWS.md). All changes are backward compatible: new arguments
default to the previous behavior, and no existing exported behavior changes.

`tidyselect` is added to Imports (it backs the `.before`/`.after` column
resolution and is already a transitive dependency via `dplyr`).

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
