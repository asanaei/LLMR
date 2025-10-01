# LLMR 0.6.2

## New Features

- `setup_llm_parallel()` changed parameter order to accept numeric positional argument for `workers`.
- `llm_mutate()` adds a shorthand form: `llm_mutate(answer = "<prompt>" | c(system=..., user=...), .config=...)`.
- `llm_mutate()` gains `.structured` flag: set `.structured = TRUE` to enable JSON output with automatic parsing (equivalent to calling `llm_mutate_structured()`).
- `llm_mutate_structured()` now supports shorthand syntax: `llm_mutate_structured(result = "{text}", .schema = schema)`.

## Improvements

- Enhanced `.fields` documentation to clarify auto-extraction behavior and nested path support.
- Clarified that `.schema = NULL` enables JSON mode without strict schema validation.
- Added comprehensive examples demonstrating new structured output features in vignettes.

## Bug Fixes

- Removed unused internal function `.category_from_condition` in parallel utilities.
- Fixed `build_factorial_experiments()` documentation to correctly describe return value columns.
- Corrected `call_llm_par()` default value for `backoff_factor` parameter (now correctly documented as 3).
- Added missing `@importFrom purrr` declarations for imported functions.

# LLMR 0.6.1
- Fixed bug in that affected `claude` calls.

# LLMR 0.6.0

## Breaking changes

- Returns and objects:
- call_llm() in generative mode now returns an llmr_response object by default. Use as.character(x) to extract text; print(x) shows a concise status line; helpers include finish_reason(), tokens(), and is_truncated().
- Legacy json= arguments are removed. Generative calls always return an llmr_response.

## New

- Improved API key handling. 
	(1) You can give the key name instead of actual token
	(2) Even if actual token is provided, it is immediately turned into a system variable and not stored directly

- json output and fixed structure are now possible.
- llm_mutate can inject multiple pieces of text from columns.
