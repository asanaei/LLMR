## Submission

LLMR 0.9.0 updates 0.8.11 on CRAN; versions 0.8.12 to 0.8.14 were
development versions and never submitted, so NEWS.md lists their changes
under their own headings. The release corrects several results that were
wrong without notice in 0.8.11: experiment runners no longer evaluate braces
in prompts, failed or interrupted streams raise errors instead of returning
partial text as a finished reply, tool calls with unparseable arguments are
not run, Gemini and Anthropic token counts and cost estimates follow each
provider's counting rules, unknown usage is no longer reported as zero, and
an invalid `timeout` is refused instead of removing the request timeout. The
deprecated `llm_methods_text()` is removed; no package on CRAN calls it.

It also adds the Meta Model API as a provider and local models: small open
language models that run on the user's computer through llama.cpp, with
functions for research uses that hosted providers do not offer (next-token
distributions, continuations of a reply whose beginning the user writes,
and probabilities of given texts), and a provider, `"llamacpp"`, through
which the rest of the package uses them.

## Downloading pre-compiled software

We ask for the CRAN team's agreement to one runtime download of
pre-compiled software. A local model needs the llama.cpp server program, a
C++ program of considerable size with platform-specific GPU support; LLMR
contains no compiled code and does not build llama.cpp. When, and only when,
the user calls `llm_local_install()`, the function downloads the official
prebuilt server from the llama.cpp project's GitHub releases, together with
the model file from Hugging Face. LLMR pins one llama.cpp build rather than
using whatever version a system may have installed, because the reported
probabilities change between builds and research results must be
reproducible; a user can still point `llm_local_start(server = )` at a
llama-server of their own. The download is kept within the policy as
follows:

- Nothing is downloaded at installation or load time, by examples, or by
  tests; examples that use a model run only when `llm_local_ready()` reports
  one installed, and the downloading example is in `\dontrun{}`.
- Before downloading, the function lists every file with its size, source,
  and license and asks for agreement; in a non-interactive session it stops
  unless `accept_license = TRUE`. For a model the user describes by its
  Hugging Face repository (`llm_local_model()`), the function first reads the
  file's size, checksum, and license from the Hugging Face API, a small
  metadata request, so that it can show them.
- Every file is pinned to one release (llama.cpp b11669) or one repository
  commit and checked against a recorded sha256 after an https download; a
  file that fails the check is deleted.
- Files are written only under `tools::R_user_dir("LLMR", "cache")` or a
  directory the user names; builds from earlier pins are removed when a new
  one is installed, and `llm_local_uninstall()` removes everything.
- The server listens only on 127.0.0.1, requires a random key generated
  each time it starts for every endpoint but its health check, and is
  stopped when R exits (processx supervises it). Examples start it with two
  threads and stop it.

Precedents for this pattern on CRAN include torch (`install_torch()`),
onnxr (`onnx_install()`), localLLM (`install_localLLM()`), and tinytex.

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

To be checked against this version before submission: LLMR.shiny,
LLMRagent, LLMRpanel, and FocusGroup (all this maintainer's).
