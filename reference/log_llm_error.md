# Log LLMR Errors

Logs an error with a timestamp for troubleshooting.

## Usage

``` r
log_llm_error(err)
```

## Arguments

- err:

  An error object.

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example of logging an error by catching a failure:
  # Use a deliberately fake API key to force an error
  config_test <- llm_config(
    provider = "openai",
    model = "gpt-3.5-turbo",
    api_key = "FAKE_KEY",
    temperature = 0.5,
    top_p = 1,
    max_tokens = 30
  )

  tryCatch(
    call_llm(config_test, list(list(role = "user", content = "Hello world!"))),
    error = function(e) log_llm_error(e)
  )
} # }
```
