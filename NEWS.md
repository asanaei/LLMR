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
