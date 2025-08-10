# LLMR_tidy.R ---------------------------------------------------------------

#' @importFrom glue glue_data


#' @title Apply an LLM prompt over vectors/data frames
#' @name llm_fn
#' @rdname llm_fn
#' @export
#' @param x A character vector *or* a data.frame/tibble.
#' @param prompt A glue template string. With a data-frame you may reference
#'   columns (`{col}`); with a vector the placeholder is `{x}`.
#' @param .config An [llm_config] object.
#' @param .system_prompt Optional system message (character scalar).
#' @param ... Passed unchanged to [call_llm_broadcast()] (e.g. `tries`,
#'   `progress`, `verbose`).
#' @param .return One of \code{c("text","columns","object")}. `"columns"`
#'   returns a tibble of diagnostic columns; `"text"` returns a character
#'   vector; `"object"` returns a list of `llmr_response` (or `NA` on failure).
#'
#' @return See `.return`. In embedding mode, always returns a numeric matrix.
#'
#' @seealso [llm_mutate()], [setup_llm_parallel()], [call_llm_broadcast()]
#'
#' @examples
#' if (interactive()) {
#'   words <- c("excellent","awful")
#'   cfg <- llm_config("openai","gpt-4o-mini", Sys.getenv("OPENAI_API_KEY"), temperature = 0)
#'   llm_fn(words, "Classify '{x}' as
#'   Positive/Negative.", cfg, .system_prompt="One word.", .return="columns")
#' }
llm_fn <- function(x,
                   prompt,
                   .config,
                   .system_prompt = NULL,
                   ...,
                   .return = c("text","columns","object")) {

  stopifnot(inherits(.config, "llm_config"))
  .return <- match.arg(.return)

  user_txt <- if (is.data.frame(x)) {
    glue::glue_data(x, prompt, .na = "")
  } else {
    glue::glue_data(list(x = x), prompt, .na = "")
  }

  # Embeddings branch unchanged
  if (isTRUE(.config$embedding) ||
      grepl("embedding", .config$model, ignore.case = TRUE)) {
    emb <- get_batched_embeddings(
      texts        = user_txt,
      embed_config = .config,
      ...
    )
    return(emb)
  }

  msgs <- lapply(as.character(user_txt), function(txt) {
    if (is.null(.system_prompt)) txt else c(system = .system_prompt, user = txt)
  })

  res <- call_llm_broadcast(
    config   = .config,
    messages = msgs,
    ...
  )

  if (.return == "text") {
    return(ifelse(res$success, res$response_text, NA_character_))
  }

  if (.return == "object") {
    out <- vector("list", nrow(res))
    for (i in seq_len(nrow(res))) {
      out[[i]] <- if (isTRUE(res$success[i])) res$response[[i]] else NA
    }
    return(out)
  }

  tibble::as_tibble(res[, c(
    "response_text","finish_reason",
    "sent_tokens","rec_tokens","total_tokens","reasoning_tokens",
    "success","error_message","status_code","error_code","bad_param",
    "response_id","duration","raw_response_json"
  )])

}


#' Mutate a data frame with LLM output
#'
#' Adds one or more columns to `.data` that are produced by a Large-Language-Model.
#'
#' @param .data A data.frame / tibble.
#' @param output Unquoted name that becomes **the new column** (generative) *or*
#'   **the prefix** for embedding columns.
#' @param prompt Optional glue template string for a single user turn; reference
#'   any columns in `.data` (e.g. `"{id}. {question}\nContext: {context}"`).
#'   Ignored if `.messages` is supplied.
#' @param .messages Optional **named** character vector of glue templates to build
#'   a multi-turn message, using roles in `c("system","user","assistant","file")`.
#'   Values are glue templates evaluated per-row; all can reference multiple columns.
#'   For multimodal, use role `"file"` with a column containing a path template.
#' @param .config An [llm_config] object (generative or embedding).
#' @param .system_prompt Optional system message sent with every request when
#'   `.messages` does not include a `system` entry.
#' @param .before,.after Standard [dplyr::relocate] helpers controlling where the
#'   generated column(s) are placed.
#' @param .return One of \code{c("columns","text","object")}. For generative mode,
#'   controls how results are added. `"columns"` (default) adds text plus
#'   diagnostic columns; `"text"` adds a single text column; `"object"` adds a
#'   list-column of `llmr_response` objects.
#' @param ... Passed to the underlying calls: [call_llm_broadcast()] in
#'   generative mode, [get_batched_embeddings()] in embedding mode.
#'
#' @details
#' - **Multi-column injection:** templating is NA-safe (`NA` -> empty string).
#' - **Multi-turn templating:** supply `.messages = c(system=..., user=..., file=...)`.
#'   Duplicate role names are allowed (e.g., two `user` turns).
#' - **Generative mode:** one request per row via [call_llm_broadcast()]. Parallel
#'   execution follows the active **future** plan; see [setup_llm_parallel()].
#' - **Embedding mode:** the per-row text is embedded via [get_batched_embeddings()].
#'   Result expands to numeric columns named `paste0(<output>, 1:N)`. If all rows
#'   fail to embed, a single `<output>1` column of `NA` is returned.
#'
#' @return `.data` with the new column(s) appended.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   id       = 1:2,
#'   question = c("Capital of France?", "Author of 1984?"),
#'   hint     = c("European city", "English novelist")
#' )
#'
#' cfg <- llm_config("openai", "gpt-4o-mini",
#'                   Sys.getenv("OPENAI_API_KEY"), temperature = 0)
#'
#' # Generative: single-turn with multi-column injection
#' df |>
#'   llm_mutate(
#'     answer,
#'     prompt = "{question} (hint: {hint})",
#'     .config = cfg,
#'     .system_prompt = "Respond in one word."
#'   )
#'
#' # Generative: multi-turn via .messages (system + user)
#' df |>
#'   llm_mutate(
#'     advice,
#'     .messages = c(
#'       system = "You are a helpful zoologist. Keep answers short.",
#'       user   = "What is a key fact about this? {question} (hint: {hint})"
#'     ),
#'     .config = cfg
#'   )
#'
#' # Multimodal: include an image path with role 'file'
#' pics <- tibble::tibble(
#'   img    = c("inst/extdata/cat.png", "inst/extdata/dog.jpg"),
#'   prompt = c("Describe the image.", "Describe the image.")
#' )
#' pics |>
#'   llm_mutate(
#'     vision_desc,
#'     .messages = c(user = "{prompt}", file = "{img}"),
#'     .config = llm_config("openai","gpt-4.1-mini", Sys.getenv("OPENAI_API_KEY"))
#'   )
#'
#' # Embeddings: output name becomes the prefix of embedding columns
#' emb_cfg <- llm_config("voyage", "voyage-3.5-lite",
#'                       Sys.getenv("VOYAGE_KEY"), embedding = TRUE)
#' df |>
#'   llm_mutate(
#'     vec,
#'     prompt  = "{question}",
#'     .config = emb_cfg,
#'     .after  = id
#'   )
#' }
#' @export
#' @importFrom glue glue_data
llm_mutate <- function(.data,
                       output,
                       prompt = NULL,
                       .messages = NULL,
                       .config,
                       .system_prompt = NULL,
                       .before = NULL,
                       .after  = NULL,
                       .return = c("columns","text","object"),
                       ...) {

  stopifnot(inherits(.config, "llm_config"))

  roles_allowed <- c("system", "user", "assistant", "file")

  # -- helpers (local) -------------------------------------------------------
  eval_messages_one_row <- function(row_df, tpl_vec) {
    roles <- names(tpl_vec)
    if (is.null(roles)) roles <- rep("user", length(tpl_vec))
    roles[is.na(roles) | roles == ""] <- "user"

    out <- vapply(
      unname(tpl_vec),
      function(s) as.character(glue::glue_data(row_df, s, .na = "")),
      FUN.VALUE = character(1)
    )
    names(out) <- roles
    out
  }

  build_generative_messages <- function(df, prompt, .messages, .system_prompt) {
    n <- nrow(df)
    msgs <- vector("list", n)

    if (!is.null(.messages)) {
      stopifnot(is.character(.messages), length(.messages) > 0)
      if (is.null(names(.messages))) {
        names(.messages) <- rep("user", length(.messages))
      }
      bad <- setdiff(unique(names(.messages)), roles_allowed)
      if (length(bad)) {
        stop(sprintf("Unsupported roles in .messages: %s",
                     paste(bad, collapse = ", ")))
      }
      for (i in seq_len(n)) {
        row_msg <- eval_messages_one_row(df[i, , drop = FALSE], .messages)
        # Add system if not present
        if (!is.null(.system_prompt) && !"system" %in% names(row_msg)) {
          row_msg <- c(system = .system_prompt, row_msg)
        }
        msgs[[i]] <- row_msg
      }
    } else {
      if (is.null(prompt)) stop("Either 'prompt' or '.messages' must be provided.")
      user_txt <- glue::glue_data(df, prompt, .na = "")
      for (i in seq_len(n)) {
        if (is.null(.system_prompt)) {
          msgs[[i]] <- as.character(user_txt[i])
        } else {
          msgs[[i]] <- c(system = .system_prompt, user = as.character(user_txt[i]))
        }
      }
    }

    msgs
  }

  pick_embed_text <- function(named_vec) {
    nm <- names(named_vec) %||% rep("user", length(named_vec))
    if (any(nm == "user")) {
      tail(named_vec[nm == "user"], 1)
    } else {
      paste(named_vec[nm != "file"], collapse = "\n\n")
    }
  }
  # -------------------------------------------------------------------------

  # ---- Embedding branch ----------------------------------------------------
  if (isTRUE(.config$embedding) ||
      grepl("embedding", .config$model, ignore.case = TRUE)) {

    user_txt <- if (!is.null(.messages)) {
      stopifnot(is.character(.messages), length(.messages) > 0)
      if (is.null(names(.messages))) names(.messages) <- rep("user", length(.messages))
      bad <- setdiff(unique(names(.messages)), roles_allowed)
      if (length(bad)) {
        stop(sprintf("Unsupported roles in .messages: %s",
                     paste(bad, collapse = ", ")))
      }
      vapply(seq_len(nrow(.data)), function(i) {
        row_msg <- eval_messages_one_row(.data[i, , drop = FALSE], .messages)
        as.character(pick_embed_text(row_msg))
      }, FUN.VALUE = character(1))
    } else {
      if (is.null(prompt)) stop("For embeddings, provide 'prompt' or '.messages' that yields a user text.")
      as.character(glue::glue_data(.data, prompt, .na = ""))
    }

    emb_mat <- get_batched_embeddings(
      texts        = user_txt,
      embed_config = .config,
      ...
    )

    # Determine prefix
    out_q  <- rlang::enquo(output)
    col_prefix <- rlang::as_name(out_q)

    # Robust fallback if all embeddings failed
    if (is.null(emb_mat)) {
      emb_mat <- matrix(NA_real_, nrow = nrow(.data), ncol = 1)
    }

    if (is.matrix(emb_mat) && ncol(emb_mat) > 0) {
      colnames(emb_mat) <- paste0(col_prefix, seq_len(ncol(emb_mat)))
    } else {
      # Ensure at least one column exists
      emb_mat <- matrix(NA_real_, nrow = nrow(.data), ncol = 1)
      colnames(emb_mat) <- paste0(col_prefix, 1L)
    }

    emb_df <- tibble::as_tibble(as.data.frame(emb_mat, stringsAsFactors = FALSE))

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
  # -------------------------------------------------------------------------

  # ---- Generative branch ---------------------------------------------------
  # ---- Generative branch ---------------------------------------------------
  if (!is.null(.messages) && !is.null(prompt)) {
    warning("Both '.messages' and 'prompt' supplied; 'prompt' will be ignored.")
  }

  msgs <- build_generative_messages(.data, prompt, .messages, .system_prompt)

  res <- call_llm_broadcast(
    config   = .config,
    messages = msgs,
    ...
  )

  .ret <- match.arg(.return)

  out_sym <- rlang::enquo(output)
  base_text <- ifelse(res$success, res$response_text, NA_character_)

  if (.ret == "text") {
    return(.data |>
             dplyr::mutate(!!out_sym := base_text,
                           .before = {{ .before }}, .after = {{ .after }}))
  }

  if (.ret == "object") {
    col_name <- paste0(rlang::as_name(out_sym), "_obj")
    objs <- vector("list", nrow(res))
    for (i in seq_len(nrow(res))) {
      objs[[i]] <- if (isTRUE(res$success[i])) res$response[[i]] else NA
    }
    return(.data |>
             dplyr::mutate(!!col_name := objs,
                           .before = {{ .before }}, .after = {{ .after }}))
  }


  added <- tibble::tibble(
    !!rlang::as_name(out_sym)              := base_text,
    !!paste0(rlang::as_name(out_sym), "_finish")  := res$finish_reason,
    !!paste0(rlang::as_name(out_sym), "_sent")    := res$sent_tokens,
    !!paste0(rlang::as_name(out_sym), "_rec")     := res$rec_tokens,
    !!paste0(rlang::as_name(out_sym), "_tot")     := res$total_tokens,
    !!paste0(rlang::as_name(out_sym), "_reason")  := res$reasoning_tokens,
    !!paste0(rlang::as_name(out_sym), "_ok")      := res$success,
    !!paste0(rlang::as_name(out_sym), "_err")     := res$error_message,
    !!paste0(rlang::as_name(out_sym), "_id")      := res$response_id,
    !!paste0(rlang::as_name(out_sym), "_status") := res$status_code,
    !!paste0(rlang::as_name(out_sym), "_ecode")  := res$error_code,
    !!paste0(rlang::as_name(out_sym), "_param")  := res$bad_param,
    !!paste0(rlang::as_name(out_sym), "_t")       := res$duration
  )

  res_df <- dplyr::bind_cols(.data, added)

  res_df <- dplyr::relocate(
    res_df,
    dplyr::all_of(names(added)),
    .before = {{ .before }},
    .after  = {{ .after }}
  )

  return(res_df)
}
