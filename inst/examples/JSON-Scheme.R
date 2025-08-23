#LLMR VERSION >= 0.6.0
library(tibble)
library(dplyr)
library(purrr)
library(LLMR)

# ----- Data
df <- tibble(sentence = c("Cats are lovely.", "Dogs are loyal.", "Cars don't fly"))

# ----- Strict JSON schema
schema <- list(
  type = "object",
  properties = list(
    family     = list(type = "string", description = "zoological family or 'NA'"),
    confidence = list(type = "number", minimum = 0, maximum = 1),
    rationale  = list(type = "string")
  ),
  required = list("family","confidence","rationale"),
  additionalProperties = FALSE
)

# ----- Configs
cfg_openai <- llm_config(
  provider = "openai",
  model    = "gpt-5-mini",
  
#  temperature = 1 # must be one if being set
)

cfg_anthropic <- llm_config(
  provider = "anthropic",
  model    = "claude-sonnet-4-20250514",
  max_tokens = 512,
  temperature = 1
)


cfg_gemini <- llm_config(
  provider = "gemini",
  model    = "gemini-2.5-flash",
  temperature = 1
)

# ----- Enforce JSON per provider
# OpenAI: server json_schema
cfg_openai_json <- enable_structured_output(cfg_openai, schema = schema, strict = TRUE)

# Anthropic: FORCE tool-only JSON
cfg_anthropic_json <- enable_structured_output(cfg_anthropic, schema = schema)

cfg_anthropic_json$tools <- list(list(
  name = "emit_json",
  description = "Return ONLY the final JSON object.",
  input_schema = schema
))
cfg_anthropic_json$tool_choice <- list(type = "tool", name = "emit_json")

# Gemini: DO NOT send server schema (avoid 400); validate locally instead!
cfg_gemini_json <- enable_structured_output(cfg_gemini, schema = NULL)

# ---------- 1) Single-config mutate (OpenAI) with local validation ON too
out_json <- df |>
  llm_mutate_structured(
    answer,
    prompt = paste(
      "Sentence: {sentence}",
      "Return a JSON object with keys: family, confidence \\in [0,1], rationale.",
      "If an animal is present, do NOT return 'NA'.",
      sep = "\n"
    ),
    .config = cfg_openai_json,
    .schema = schema,                 # <— keep local guard ON
    .fields = c("family","confidence")
  ) |>
  mutate(confidence = suppressWarnings(as.numeric(confidence)))

print(out_json |> select(sentence, family, confidence, structured_ok))

# ---------- 2) Parallel, cross-provider with LOCAL schema validation


experiments <- build_factorial_experiments(
  configs        = list(cfg_openai_json, cfg_anthropic_json, cfg_gemini_json),
  user_prompts   = paste(
    "Sentence: {sentence}",
    "Return JSON with keys: family, confidence, rationale.",
    "Return the taxonomic family of the animal in the sentence, or NA if none",
    "Even if you are returning an NA, give your confidence and rationale",
    "Reply with JSON only.",
    sep = "\n"
  ),
  system_prompts = "Output must be valid JSON conforming to the schema.",
  repetitions    = 2,
  config_labels  = c("openai","anthropic","gemini")
)


setup_llm_parallel(workers = 12, verbose = TRUE)

res_par <- experiments |>
  left_join(df, by = character()) |>
  # IMPORTANT: use local `.schema` so all providers are validated & coerced the same way
  call_llm_par_structured(schema = schema,
                    .fields = c("family","confidence","rationale"))
reset_llm_parallel()


res_par_cn = res_par |>
  mutate(confidence = suppressWarnings(as.numeric(confidence)))


res_par_cn |>
  select(config_label, provider, model, sentence, family, confidence, rationale,
         structured_ok, finish_reason) %>%
  arrange(config_label) %>% print(n = Inf)

## sometimes "hi/lo" is given instead of numbers
table(res_par_cn$confidence, res_par$confidence)


# ---------- 3) Vector helper with local schema + post-coercion
vec_out <- llm_fn_structured(
  x       = df$sentence,
  prompt  = "For '{x}', Return the taxonomic family of the animal in the sentence,
  or NA if none; return JSON with keys: family, confidence (\\in [0,1]), rationale.",
  .config = cfg_openai_json, # cfg_anthropic_json,
  .schema = schema,                    # turn ON
  .fields = c("family","confidence")) |>
  mutate(confidence = suppressWarnings(as.numeric(confidence)))

print(vec_out %>% select(response_text, family, confidence, structured_ok))


vec_out2 <- llm_fn_structured(
  x       = df$sentence,
  prompt  = "For '{x}', Return the taxonomic family of the animal in the sentence,
  or NA if none; return JSON with keys: family, confidence (\\in [0,1]), rationale.",
  .config = cfg_anthropic_json,
  .schema = schema,                    # turn ON
  .fields = c("family","confidence")) |>
  mutate(confidence = suppressWarnings(as.numeric(confidence)))

print(vec_out2 %>% select(response_text, family, confidence, structured_ok))


vec_out3 <- llm_fn_structured(
  x       = df$sentence,
  prompt  = "For '{x}', Return the taxonomic family of the animal in the sentence,
  or NA if none; return JSON with keys: rationale, family, confidence (\\in [0,1]).",
  .config = cfg_gemini,
   # .schema = schema,  # gemini doesn't support schema and gives an error, for now the package ignores this to avoid error
  .fields = c("family","confidence","rationale")) |>
   mutate(confidence = suppressWarnings(as.numeric(confidence)))

print(vec_out3 |> select(rationale, family, confidence, structured_ok))

