# LLM_param_translate.R  -------------------------------------------------------------
# Canonical-to-provider parameter translation (inspired by LangChain)
#   – canonical names follow the OpenAI spelling
#   – unknown keys are forwarded untouched for maximal future-proofing
#
# Supported canonical names:
#   temperature, max_tokens, top_p, top_k,
#   frequency_penalty, presence_penalty, repetition_penalty,
#   thinking_budget, include_thoughts
#
##############################################################################

.translate_params <- function(provider, mp = list()) {

  ## --- 1. canonical ---> provider field names ---------------------------------
  map <- switch(
    provider,
    gemini = c(
      max_tokens      = "maxOutputTokens",
      top_p           = "topP",
      top_k           = "topK",
      thinking_budget = "thinkingBudget",
      include_thoughts= "includeThoughts"
    ),
    anthropic = c(
      thinking_budget = "budget_tokens",    # lives in $thinking
      include_thoughts= "include_thoughts"  # handled later
    ),
    openai   = character(),                 # identical
    groq     = character(),                 # identical
    together = character(),                 # identical
    deepseek = character(),                 # identical
    voyage   = character(),                 # identical
    character()                             # default: pass-through
  )

  if (length(map)) {
    renames <- intersect(names(mp), names(map))
    names(mp)[match(renames, names(mp))] <- map[renames]
  }

  ## --- 2. warn on obviously unsupported knobs ------------------------------
  unsupported <- switch(
    provider,
    gemini    = c("frequency_penalty", "presence_penalty", "repetition_penalty"),
    anthropic = c("top_k", "frequency_penalty", "presence_penalty",
                  "repetition_penalty"),
    character()
  )
  bad <- intersect(names(mp), unsupported)
  if (length(bad))
    warning(
      sprintf("Parameters not recognised by %s API dropped: %s",
              provider, paste(bad, collapse = ", "))
    )

  mp[setdiff(names(mp), unsupported)]
}
