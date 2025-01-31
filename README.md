# LLMR

LLMR offers a unified interface for interacting with multiple Large Language Model APIs in R, including OpenAI, Anthropic, Groq, Together AI, DeepSeek, and Voyage AI.

## Installation

- **CRAN**:  
  ```r
  install.packages("LLMR")
  ```

-**GitHub (development version)**:
```r
remotes::install_github("YOUR_USERNAME/LLMR")
```


Example Usage

Below is an example demonstrating a comprehensive configuration and API call using OpenAI. In this example, the model name is changed to gpt-4-lite and the response object is named detailed_response.

library(LLMR)

# Create a configuration with more parameters
comprehensive_openai_config <- llm_config(
  provider = "openai",
  model = "gpt-4-mini",          # Changed model name
  api_key = Sys.getenv("OPENAI_KEY"),
  temperature = 1,               # Controls randomness
  max_tokens = 750,              # Maximum tokens to generate
  top_p = 1,                     # Nucleus sampling parameter
  frequency_penalty = 0.5,       # Penalizes token frequency
  presence_penalty = 0.3         # Penalizes token presence
)

# Define a more complex message
comprehensive_message <- list(
  list(role = "system", content = "You are an expert data scientist."),
  list(role = "user", content = "When will you ever use OLS?")
)

# Call the LLM with all parameters and retrieve raw JSON as an attribute
detailed_response <- call_llm(
  config = comprehensive_openai_config,
  messages = comprehensive_message,
  json = TRUE
)

# Display the generated text response
cat("Comprehensive OpenAI Response:", detailed_response, "\n")

# Access and print the raw JSON response
raw_json_response <- attr(detailed_response, "raw_json")
print(raw_json_response)

Contributions: are welcome.
