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

.translate_params <- function(provider, mp = list(), auto_fix = TRUE) {
  auto_fix <- isTRUE(auto_fix)

  map <- switch(
    provider,
    gemini = c(
      max_tokens       = "maxOutputTokens",
      top_p            = "topP",
      top_k            = "topK",
      thinking_budget  = "thinkingBudget",
      include_thoughts = "includeThoughts"
    ),
    anthropic = c(
      thinking_budget  = "budget_tokens",
      include_thoughts = "include_thoughts"
    ),
    character()
  )

  if (length(map) && auto_fix) {
    renames <- intersect(names(mp), names(map))
    if (length(renames)) {
      old <- renames; new <- unname(map[renames])
      names(mp)[match(old, names(mp))] <- new
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert_info(
          sprintf("Renamed %s \u2192 %s for %s",
                  paste(old, collapse = ", "),
                  paste(new, collapse = ", "),
                  provider)
        )
      }
    }
  }

  unsupported <- switch(
    provider,
    gemini    = c("frequency_penalty", "presence_penalty", "repetition_penalty"),
    anthropic = c("top_k", "frequency_penalty", "presence_penalty", "repetition_penalty"),
    character()
  )
  bad <- intersect(names(mp), unsupported)
  if (length(bad) && auto_fix) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_info(
        sprintf("Dropped unsupported parameters for %s: %s", provider, paste(bad, collapse = ", "))
      )
    }
    mp <- mp[setdiff(names(mp), bad)]
  }

  mp
}
