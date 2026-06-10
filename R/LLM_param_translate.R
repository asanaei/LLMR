# LLM_param_translate.R  -------------------------------------------------------------
# Canonical-to-provider parameter translation
#   - canonical names follow the OpenAI spelling
#   - unknown keys are forwarded untouched for maximal future-proofing
#
# Supported canonical names:
#   temperature, max_tokens, top_p, top_k,
#   frequency_penalty, presence_penalty, repetition_penalty,
#   seed, logprobs, top_logprobs,
#   thinking_budget, include_thoughts
#
##############################################################################

# Internal: emit a parameter note unless the user opted out via
# options(llmr.quiet = TRUE).
.llmr_param_note <- function(txt) {
  if (isTRUE(getOption("llmr.quiet"))) return(invisible(NULL))
  if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info(txt)
  invisible(NULL)
}

.translate_params <- function(provider, mp = list(), auto_fix = TRUE) {
  auto_fix <- isTRUE(auto_fix)

  map <- switch(
    provider,
    gemini = c(
      max_tokens        = "maxOutputTokens",
      top_p             = "topP",
      top_k             = "topK",
      presence_penalty  = "presencePenalty",
      frequency_penalty = "frequencyPenalty",
      logprobs          = "responseLogprobs",
      top_logprobs      = "logprobs",
      thinking_budget   = "thinkingBudget",
      include_thoughts  = "includeThoughts"
    ),
    anthropic = c(
      thinking_budget  = "budget_tokens"
    ),
    character()
  )

  if (length(map) && auto_fix) {
    renames <- intersect(names(mp), names(map))
    # only names that actually change spelling are worth a note
    renames <- renames[unname(map[renames]) != renames]
    if (length(renames)) {
      old <- renames; new <- unname(map[renames])
      names(mp)[match(old, names(mp))] <- new
      .llmr_param_note(
        sprintf("Renamed %s \u2192 %s for %s",
                paste(old, collapse = ", "),
                paste(new, collapse = ", "),
                provider)
      )
    }
  }

  # Parameters the provider's API genuinely does not accept. Anthropic supports
  # top_k; Gemini supports presence/frequency penalties (handled by the rename
  # map above), so neither is dropped here.
  unsupported <- switch(
    provider,
    gemini    = c("repetition_penalty"),
    anthropic = c("frequency_penalty", "presence_penalty", "repetition_penalty",
                  "seed", "logprobs", "top_logprobs", "include_thoughts"),
    character()
  )
  bad <- intersect(names(mp), unsupported)
  if (length(bad) && auto_fix) {
    extra_note <- if (provider == "anthropic" && "include_thoughts" %in% bad) {
      " (Anthropic returns thinking blocks whenever thinking is enabled; no flag is needed)"
    } else ""
    .llmr_param_note(
      sprintf("Dropped unsupported parameters for %s: %s%s",
              provider, paste(bad, collapse = ", "), extra_note)
    )
    mp <- mp[setdiff(names(mp), bad)]
  }

  mp
}
