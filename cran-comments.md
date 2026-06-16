## Submission

This is a new feature release of LLMR (0.8.6). It adds three additive helpers --
`llm_log_read()`, `llm_request_from_log()`, and a shared `reset()` generic -- and
canonicalizes message shape in `llm_request_hash()`. No existing exported behavior
changes.

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

## Reverse dependencies

None on CRAN. Several of the author's other packages depend on LLMR and will be
submitted after it is accepted.
