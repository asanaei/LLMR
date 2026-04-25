# Build Factorial Experiment Design

Creates a tibble of experiments for factorial designs where you want to
test all combinations of configs, messages, and repetitions with
automatic metadata.

## Usage

``` r
build_factorial_experiments(
  configs,
  user_prompts,
  system_prompts = NULL,
  repetitions = 1,
  config_labels = NULL,
  user_prompt_labels = NULL,
  system_prompt_labels = NULL
)
```

## Arguments

- configs:

  List of llm_config objects to test.

- user_prompts:

  Character vector (or list) of user‑turn prompts.

- system_prompts:

  Optional character vector of system messages (recycled against user
  prompts). Missing/NA values are ignored; messages are user-only.

- repetitions:

  Integer. Number of repetitions per combination. Default is 1.

- config_labels:

  Character vector of labels for configs. If NULL, uses
  "provider_model".

- user_prompt_labels:

  Optional labels for the user prompts.

- system_prompt_labels:

  Optional labels for the system prompts.

## Value

A tibble with columns: config (list-column), messages (list-column),
config_label, user_prompt_label, system_prompt_label, and repetition.
Ready for use with call_llm_par().

## Examples

``` r
if (FALSE) { # \dontrun{
  # Factorial design: 3 configs x 2 user prompts x 10 reps = 60 experiments
  configs <- list(gpt4_config, claude_config, llama_config)
  user_prompts <- c("Control prompt", "Treatment prompt")

  experiments <- build_factorial_experiments(
    configs = configs,
    user_prompts = user_prompts,
    repetitions = 10,
    config_labels = c("gpt4", "claude", "llama"),
    user_prompt_labels = c("control", "treatment")
  )

  # Use with call_llm_par
  results <- call_llm_par(experiments, progress = TRUE)
} # }
```
