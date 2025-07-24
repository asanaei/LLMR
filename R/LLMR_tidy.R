# LLMR_tidy.R ---------------------------------------------------------------

#' Applies an LLM prompt to every element of a vector
#'
#' @importFrom tidyr expand_grid
#' @importFrom rlang `:=`
#'
#' \code{llm_fn()} turns every element (or row) of \code{x} into an LLM prompt
#' (via glue templating) and returns the model's reply.
#'
#' *Stateless:* no memory across calls, so it is **not** an agent.
#'
#' @param x  A character vector **or** a data.frame/tibble.
#' @param prompt A glue template string.
#'   *If* \code{x} is a data frame, use \code{{col}} placeholders;
#'   *if* \code{x} is a vector, refer to the element as \code{{x}}.
#' @param .config An \link{llm_config} object.
#' @param .system_prompt Optional system message (character scalar).
#' @param ... Passed unchanged to \link{call_llm_broadcast} (e.g.\ \code{tries},
#'   \code{progress}, \code{verbose}).
#'
#' @return
#' * In **generative mode**: a character vector the same length as \code{x}
#'   (failed calls yield \code{NA}).
#' * In **embedding mode**: a numeric matrix whose rows correspond to elements
#'   of \code{x} and whose columns are embedding dimensions.
#' @details
#' Runs each prompt through `call_llm_broadcast()`, which forwards the
#' requests to `call_llm_par()`.
#' Internally each prompt is passed as a
#' **plain character vector** (or a
#' named character vector when `.system_prompt` is supplied).
#' That core engine executes them *in parallel* according
#' to the current *future* plan.
#' For instant multi-core use, call `setup_llm_parallel(workers = 4)` (or whatever
#' number you prefer) once per session; revert with `reset_llm_parallel()`.
#'
#' **Embedding shortcut:**
#' If `.config$embedding` is `TRUE` (or the model
#' name contains `"embedding"`), `llm_fn()` skips chat calls and returns the
#' numeric matrix produced by `get_batched_embeddings()`. This only exists for consistency.
#' @export
#'
#' @seealso
#' \code{\link{setup_llm_parallel}},
#' \code{\link{reset_llm_parallel}},
#' \code{\link{call_llm_par}}, and
#' \code{\link{llm_mutate}} which is a tidy-friendly wrapper around `llm_fn()`.
#'
#' @examples
#' ## --- Generative example ---------------------------------------------------
#' \dontrun{
#' cfg <- llm_config(
#'   provider = "openai",
#'   model    = "gpt-4.1-nano",
#'   api_key  =  Sys.getenv("OPENAI_API_KEY"),
#'   temperature = 0
#' )
#'
#' words <- c("excellent", "awful", "average")
#'
#' llm_fn(
#'   words,
#'   prompt   = "Classify sentiment of '{x}' as Positive, Negative, or Neutral.",
#'   .config  = cfg,
#'   .system_prompt = "Respond with ONE word only."
#' )
#'
#' ## --- Data-frame input inside a tidyverse pipeline ----------------------
#' library(dplyr)
#'
#' reviews <- tibble::tibble(
#'   id     = 1:3,
#'   review = c("Great toaster!", "Burns bread.", "It's okay.")
#' )
#'
#' reviews |>
#'   llm_mutate(
#'     sentiment,
#'     prompt  = "Classify the sentiment of this review: {review}",
#'     .config = cfg,
#'     .system_prompt = "Respond with Positive, Negative, or Neutral."
#'   )
#'}

llm_fn <- function(x,
                   prompt,
                   .config,
                   .system_prompt = NULL,
                   ...) {

  stopifnot(inherits(.config, "llm_config"))

  user_txt <- if (is.data.frame(x)) {
    glue::glue_data(x, prompt)
  } else {
    glue::glue_data(list(x = x), prompt)
  }

  # ----- embedding shortcut --------------------------------------------------
  if (isTRUE(.config$embedding) ||
      grepl("embedding", .config$model, ignore.case = TRUE)) {

    emb <- get_batched_embeddings(
      texts        = user_txt,
      embed_config = .config,
      ...
    )

    return(emb)   # returns a matrix (rows = texts, cols = dimensions)
  }
  # ---------------------------------------------------------------------------


  msgs <- lapply(user_txt, function(txt) {
    if (is.null(.system_prompt)) {
      txt                                   # single-turn chat
    } else {
      c(system = .system_prompt, user = txt)  # named vector: system then user
    }
  })

  res <- call_llm_broadcast(
    config   = .config,
    messages = msgs,
    ...
  )

  ifelse(res$success, res$response_text, NA_character_)
}

#' Mutate a data frame with LLM output
#'
#' A convenience wrapper around \link{llm_fn} that inserts the result as a new
#' column via \link[dplyr]{mutate}.
#'
#' @inheritParams llm_fn
#' @param .data  A data frame / tibble.
#' @param output Unquoted name of the new column you want to add.
#' @param .before,.after Standard \link[dplyr]{mutate} column-placement helpers.
#' @details
#' Internally calls `llm_fn()`, so the API requests inherit the same
#' parallel behaviour.  Activate parallelism with
#' `setup_llm_parallel()` and shut it off with `reset_llm_parallel()`.
#'
#' **Embedding shortcut**
#' When `.config` is an embedding configuration the
#' returned matrix is expanded into one numeric column per dimension, named
#'   \code{paste0(<output>, 1:n)}.
#' Example: with \code{output = d} and a 1024-d model you get
#'   \code{d1} ... \code{d1024}.
#' @return
#' - In **generative mode**: the input data frame plus the new character column.
#' - In **embedding mode**: the input data frame plus the new numeric columns
#'   described above.
#' @export
#'
#' @seealso
#' \code{\link{setup_llm_parallel}},
#' \code{\link{reset_llm_parallel}},
#' \code{\link{call_llm_par}},
#' \code{\link{llm_fn}}
#'
#' @examples
#' ## --- Generative example ---------------------------------------------------
#' \dontrun{
#' df <- tibble::tibble(
#'   sentence = c("Cats are lovely.", "Dogs are loyal.")
#' )
#'
#' gen_cfg <- llm_config(
#'   provider = "openai",
#'   model    = "gpt-4.1-mini",
#'   api_key  = Sys.getenv("OPENAI_API_KEY")
#' )
#'
#' out_gen <- df |>
#'   llm_mutate(
#'     answer,
#'     prompt  = paste(
#'     "Return the taxonomic family of the animal in the sentence,",
#'     "or NA if none: {sentence}"
#'    ),
#'     .config = gen_cfg
#'   )
#'
#' head(out_gen)
#' # A tibble: 2 x 2
#' #  sentence         answer
#' #  <chr>            <chr>
#' #1 Cats are lovely. Felidae
#' #2 Dogs are loyal.  Canidae
#' }
#'
#' ## --- Embedding example ---------------------------------------------------
#' \dontrun{
#' text_df <- tibble::tibble(
#'   sentence = c("Cats are lovely.", "Dogs are loyal.")
#' )
#'
#' emb_cfg <- llm_config(
#'   provider  = "voyage",
#'   model     = "voyage-3.5-lite",
#'   api_key   = Sys.getenv("VOYAGE_KEY"),
#'   embedding = TRUE
#' )
#'
#' text_out <- text_df |>
#'   llm_mutate(
#'     emb,
#'     output  = d,
#'     prompt  = "{sentence}",
#'     .config = emb_cfg
#'   )
#'
#' head(text_out)
#' # A tibble: 2 x 1,025
#' #  sentence         d1      d2      d3  ... d1024
#' #  <chr>            <dbl>   <dbl>   <dbl>    <dbl>
#' #1 Cats are lovely. 0.0233  0.0588 0.00102 ...  0.0027
#' #2 Dogs are loyal.  0.0397 -0.0161 -0.0466 ... -0.0158
#' }
llm_mutate <- function(.data,
                       output,
                       prompt,
                       .config,
                       .system_prompt = NULL,
                       .before = NULL,
                       .after  = NULL,
                       ...) {

# ----- embedding shortcut --------------------------------------------------
  if (isTRUE(.config$embedding) ||
      grepl("embedding", .config$model, ignore.case = TRUE)) {

    ## 1. build the user strings ---------------------------------------------
    user_txt <- glue::glue_data(.data, prompt)

    ## 2. get the matrix ------------------------------------------------------
    emb_mat <- get_batched_embeddings(
      texts        = user_txt,
      embed_config = .config,
      ...
    )

    ## 3. figure out column prefix -------------------------------------------
    out_q  <- rlang::enquo(output)
    out_ex <- rlang::get_expr(out_q)

    col_prefix <- if (is.character(out_ex)) {
      out_ex[1]
    } else if (rlang::is_symbol(out_ex)) {
      rlang::as_string(out_ex)
    } else {
      stop("`output` must be an unquoted name or a single-string prefix.")
    }

    ## 4. convert matrix -> tibble with d1 ... dN -------------------------------
    emb_df <- tibble::as_tibble(
      emb_mat,
      .name_repair = ~ paste0(col_prefix, seq_along(.x))
    )

    ## 5. bind & relocate -----------------------------------------------------
    res <- dplyr::bind_cols(.data, emb_df)

    if (!is.null(.before) || !is.null(.after)) {
      res <- dplyr::relocate(
        res,
        dplyr::all_of(names(emb_df)),
        .before = {{ .before }},
        .after  = {{ .after }}
      )
    }

    return(res)
  }
# ---------------------------------------------------------------------------
out <- rlang::enquo(output)
  new_vals <- llm_fn(.data,
                     prompt         = prompt,
                     .config        = .config,
                     .system_prompt = .system_prompt,
                     ...)
  .data |>
    dplyr::mutate(
      !!out := new_vals,
      .before = {{ .before }},
      .after  = {{ .after }}
    )
}
