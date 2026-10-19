# local_provider.R --------------------------------------------------------------
# Provider "llamacpp": chat and embeddings through a local llama.cpp server.
# For a model installed with llm_local_install() the server is started on first
# use and the request goes to it with the session key; with `api_url` the
# request goes to a server the user runs, unmanaged. Chat goes through the
# shared OpenAI-compatible path, so structured output, tools, logprobs, and
# streaming behave as for hosted providers.

# Point a config at the managed server for its model, adding the server's key
# through the req_builder hook (excluded from request hashes, never logged).
.llmr_local_bind <- function(config, path) {
  # `thinking = FALSE` or `TRUE` reaches reasoning models such as Qwen3 through
  # their chat template, as llama.cpp's chat_template_kwargs.
  th <- config$model_params$thinking
  if (is.logical(th) && length(th) == 1L && !is.na(th)) {
    kw <- config$model_params$chat_template_kwargs %||% list()
    kw$enable_thinking <- th
    config$model_params$chat_template_kwargs <- kw
    config$model_params$thinking <- NULL
  }
  # llama.cpp spells the repetition penalty repeat_penalty.
  if (!is.null(config$model_params$repetition_penalty)) {
    config$model_params$repeat_penalty <- config$model_params$repeat_penalty %||%
      config$model_params$repetition_penalty
    config$model_params$repetition_penalty <- NULL
  }
  if (!is.null(config$model_params$api_url)) {
    # A server the user runs: send the key in LLAMACPP_API_KEY when it is set.
    ukey <- Sys.getenv("LLAMACPP_API_KEY", unset = "")
    if (nzchar(ukey)) {
      prev <- config$model_params$req_builder
      config$model_params$req_builder <- function(req) {
        if (is.function(prev)) req <- prev(req)
        httr2::req_headers(req, Authorization = paste("Bearer", ukey))
      }
    }
    return(list(config = config, srv = NULL))
  }
  srv <- .llmr_local_ensure(config$model)
  key <- srv$key
  prev <- config$model_params$req_builder
  config$model_params$req_builder <- function(req) {
    if (is.function(prev)) req <- prev(req)
    httr2::req_headers(req, Authorization = paste("Bearer", key))
  }
  config$model_params$api_url <- paste0(srv$url, path)
  # Reusing the server's prompt cache changes results slightly (and markedly
  # for hybrid recurrent models), so chat calls start fresh unless the
  # configuration asks for `cache_prompt = TRUE`.
  if (grepl("chat/completions", path, fixed = TRUE) && is.null(config$model_params$cache_prompt)) {
    config$model_params$cache_prompt <- FALSE
  }
  list(config = config, srv = srv)
}

# Bind every managed local config in a list once per model and kind, so the
# parallel runners talk to one server per model started in the main session.
.llmr_local_prebind <- function(configs) {
  lapply(configs, function(cfg) {
    if (!inherits(cfg, "llm_config") || !identical(cfg$provider, "llamacpp") ||
        !is.null(cfg$model_params$api_url)) {
      return(cfg)
    }
    # Settle in the main session what workers would otherwise look up again
    # (workers do not inherit options such as llmr.local_dir): the model's
    # kind and, for embeddings, the prefix for its task.
    emb <- .is_embedding_config(cfg)
    cfg$embedding <- emb
    if (emb) {
      pre <- .llmr_local_prefix(cfg)
      cfg$model_params$prefix <- pre$prefix
      cfg$model_params$task <- NULL
    }
    path <- if (emb) "/v1/embeddings" else "/v1/chat/completions"
    .llmr_local_bind(cfg, path)$config
  })
}

# The registry kind of a local model, NA when the name is not known.
.llmr_local_kind <- function(model) {
  e <- .llmr_local_lookup(model)
  if (is.null(e)) NA_character_ else e$kind
}

#' @export
call_llm.llamacpp <- function(config, messages, verbose = FALSE) {
  if (.is_embedding_config(config)) {
    return(.llmr_llamacpp_embedding(config, messages, verbose))
  }
  b <- .llmr_local_bind(config, "/v1/chat/completions")
  # llama.cpp names repetition penalties differently; every other canonical
  # parameter, top_k included, is accepted as written.
  .openai_compat_chat(b$config, messages, verbose,
                      endpoint = b$config$model_params$api_url,
                      auth_style = "none")
}

# The prefix a local embedding model expects for `task`, or an explicit
# `prefix`. A task named for a model trained without prefixes is an error, so a
# prefix cannot be applied, or omitted, by accident.
.llmr_local_prefix <- function(config) {
  mp <- config$model_params %||% list()
  if (!is.null(mp$prefix)) {
    if (!is.character(mp$prefix) || length(mp$prefix) != 1L || is.na(mp$prefix)) {
      stop("`prefix` must be one string.", call. = FALSE)
    }
    return(list(prefix = mp$prefix, task = NA_character_))
  }
  e <- .llmr_local_lookup(config$model)
  prefixes <- e$prefixes %||% character()
  task <- mp$task %||% e$default_task %||% NA_character_
  if (identical(task, "none")) return(list(prefix = "", task = "none"))
  if (!length(prefixes)) {
    if (!is.null(mp$task)) {
      stop(sprintf("%s takes no task prefix; drop `task`, or pass `task = \"none\"`.", config$model),
           call. = FALSE)
    }
    return(list(prefix = "", task = NA_character_))
  }
  if (is.na(task) || !task %in% names(prefixes)) {
    stop(sprintf("Unknown task \"%s\" for %s; one of: %s, or \"none\".", task, config$model,
                 paste(names(prefixes), collapse = ", ")), call. = FALSE)
  }
  list(prefix = unname(prefixes[[task]]), task = task)
}

.llmr_llamacpp_embedding <- function(config, messages, verbose = FALSE) {
  texts <- if (is.character(messages)) messages else vapply(messages, `[[`, "", "content")
  pre <- .llmr_local_prefix(config)
  mp <- config$model_params %||% list()
  dims <- mp$dimensions
  if (!is.null(dims)) {
    e <- .llmr_local_lookup(config$model)
    allowed <- e$mrl %||% integer()
    if (!length(allowed) || !is.numeric(dims) || length(dims) != 1L || !dims %in% allowed) {
      stop(sprintf("`dimensions` must be one of the truncation sizes %s was trained for%s.",
                   config$model,
                   if (length(allowed)) paste0(" (", paste(allowed, collapse = ", "), ")")
                   else "; it was trained for none"), call. = FALSE)
    }
  }
  b <- .llmr_local_bind(config, "/v1/embeddings")
  body <- list(model = config$model, input = as.list(paste0(pre$prefix, texts)),
               encoding_format = "float")
  body <- c(body, .embedding_extras(b$config$model_params,
                                    c(names(body), "task", "prefix", "dimensions", "truncate")))
  req <- httr2::request(b$config$model_params$api_url) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE)
  res <- tryCatch(
    perform_request(req, verbose, provider = config$provider, model = config$model, config = b$config),
    llmr_api_error = function(e) {
      # llama.cpp refuses an over-long input with HTTP 400 ("exceeds the
      # available context") or 500 ("too large to process"); either way the
      # request cannot succeed on retry, so name the inputs instead.
      if (!grepl("exceed|too large", conditionMessage(e), ignore.case = TRUE) || is.null(b$srv)) stop(e)
      n <- vapply(paste0(pre$prefix, texts), function(t)
        length(.llmr_local_request(b$srv, "/tokenize", list(content = t, add_special = TRUE))$tokens), 0L)
      limit <- b$srv$ctx
      over <- which(n > limit)
      .llmr_error(sprintf("%d input(s) exceed the %d-token context of %s (positions %s, with %s tokens). Shorten them, or start the server with a larger `ctx` in llm_local_start().",
                          length(over), limit, config$model,
                          paste(utils::head(over, 10L), collapse = ", "),
                          paste(utils::head(unname(n[over]), 10L), collapse = ", ")),
                  category = "param", provider = "llamacpp", model = config$model)
    })
  if (!is.null(dims) && length(res$data)) {
    res$data <- lapply(res$data, function(d) {
      v <- as.numeric(unlist(d$embedding))[seq_len(dims)]
      d$embedding <- as.list(v / sqrt(sum(v^2)))
      d
    })
  }
  res
}
