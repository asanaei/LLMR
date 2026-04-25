# Generate Embeddings in Batches

A wrapper function that processes a list of texts in batches to generate
embeddings, avoiding rate limits. This function calls
[`call_llm_robust`](https://asanaei.github.io/LLMR/reference/call_llm_robust.md)
for each batch and stitches the results together and parses them (using
`parse_embeddings`) to return a numeric matrix.

## Usage

``` r
get_batched_embeddings(texts, embed_config, batch_size = 50, verbose = FALSE)
```

## Arguments

- texts:

  Character vector of texts to embed. If named, the names will be used
  as row names in the output matrix.

- embed_config:

  An `llm_config` object configured for embeddings.

- batch_size:

  Integer. Number of texts to process in each batch. Default is 50.

- verbose:

  Logical. If TRUE, prints progress messages. Default is TRUE.

## Value

A numeric matrix where each row is an embedding vector for the
corresponding text. Columns are named `v1`, `v2`, ..., `vK` where K is
the embedding dimension. If embedding fails for certain texts, those
rows will be filled with NA values. The matrix will always have the same
number of rows as the input texts. Returns NULL if no embeddings were
successfully generated.

## See also

[`llm_config`](https://asanaei.github.io/LLMR/reference/llm_config.md)
to create the embedding configuration.
[`parse_embeddings`](https://asanaei.github.io/LLMR/reference/parse_embeddings.md)
to convert the raw response to a numeric matrix.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Basic usage
  texts <- c("Hello world", "How are you?", "Machine learning is great")
  names(texts) <- c("greeting", "question", "statement")

  embed_cfg <- llm_config(
    provider = "voyage",
    model = "voyage-large-2-instruct",
    embedding = TRUE,
    api_key = Sys.getenv("VOYAGE_API_KEY")
  )

  embeddings <- get_batched_embeddings(
    texts = texts,
    embed_config = embed_cfg,
    batch_size = 2
  )
} # }
```
