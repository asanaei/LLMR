# Parse Embedding Response into a Numeric Matrix

Converts the embedding response data to a numeric matrix.

## Usage

``` r
parse_embeddings(embedding_response)
```

## Arguments

- embedding_response:

  The response returned from an embedding API call.

## Value

A numeric matrix of embeddings with column names as sequence numbers.

## Examples

``` r
if (FALSE) { # \dontrun{
  text_input <- c("Political science is a useful subject",
                  "We love sociology",
                  "German elections are different",
                  "A student was always curious.")

  # Configure the embedding API provider (example with Voyage API)
  voyage_config <- llm_config(
    provider = "voyage",
    model = "voyage-large-2",
    api_key = Sys.getenv("VOYAGE_API_KEY")
  )

  embedding_response <- call_llm(voyage_config, text_input)
  embeddings <- parse_embeddings(embedding_response)
  # Additional processing:
  embeddings |> cor() |> print()
} # }
```
