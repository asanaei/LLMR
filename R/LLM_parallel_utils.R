# LLM_parallel_utils.R
# -------------------------------------------------------------------
# This file provides parallelized services for dispatching multiple LLM API calls
# concurrently using tibble-based experiment designs. It leverages the 'future'
# package for OS-agnostic parallelization and uses call_llm_robust() as the default
# calling mechanism with configurable retry and delay settings.
#
# Key Features:
#   1. call_llm_sweep() - Parameter sweep mode: vary one parameter, fixed message
#   2. call_llm_broadcast() - Message broadcast mode: fixed config, multiple messages
#   3. call_llm_compare() - Model comparison mode: multiple configs, fixed message
#   4. call_llm_par() - General mode: tibble with config and message columns
#   5. build_factorial_experiments() - Helper for factorial experimental designs
#   6. setup_llm_parallel() / reset_llm_parallel() - Environment management
#   7. Automatic load balancing and error handling
#   8. Progress tracking and detailed logging
#   9. Native metadata support through tibble columns
#  10. Automatic capture of raw JSON API responses
#
# Design Philosophy:
#   All experiment functions use tibbles with list-columns for configs and messages.
#   call_llm_par() is the core parallel engine. Wrapper functions (_sweep, _broadcast,
#   _compare) prepare inputs for call_llm_par() and then use a helper to
#   format the output, including unnesting config parameters.
#   This enables natural use of dplyr verbs for building complex experimental designs.
#   Metadata columns are preserved automatically.
#
# Dependencies: future, future.apply, tibble, dplyr, progressr (optional), tidyr (for build_factorial_experiments)
# -------------------------------------------------------------------


# message builder (simple user + optional system) --------------------
.compose_msg <- function(user, sys = NULL) {
  # NA may be received here instead of NULL;
  # remove system message if it is missing
  if (is.null(sys) || (length(sys) == 1L && is.na(sys))) {
    # no system prompt supplied
    user
  } else {
    c(system = sys, user = user)
  }
}



# Internal helper to unnest config details into columns
.unnest_config_to_cols <- function(results_df, config_col = "config") {
  if (!config_col %in% names(results_df) || !is.list(results_df[[config_col]])) {
    warning(paste0("Config column '", config_col, "' not found or not a list-column. Cannot unnest parameters."))
    return(results_df)
  }

  # Extract provider and model
  #results_df$provider <- sapply(results_df[[config_col]], function(cfg) cfg$provider %||% NA_character_)
  #results_df$model    <- sapply(results_df[[config_col]], function(cfg) cfg$model %||% NA_character_)

  if (!"provider" %in% names(results_df)) {
    results_df$provider <- sapply(results_df[[config_col]],
                                  function(cfg) cfg$provider %||% NA_character_)
  }
  if (!"model" %in% names(results_df)) {
    results_df$model <- sapply(results_df[[config_col]],
                               function(cfg) cfg$model %||% NA_character_)
  }


  # Extract all model parameters
  all_model_param_names <- unique(unlist(lapply(results_df[[config_col]], function(cfg) names(cfg$model_params))))

  if (length(all_model_param_names) > 0) {

    is_scalar_atomic <- function(x) is.atomic(x) && length(x) == 1L

    param_cols_list <- lapply(all_model_param_names, function(p_name) {
      # collect one value per row (do NOT simplify here)
      vals <- lapply(results_df[[config_col]], function(cfg) {
        cfg$model_params[[p_name]] %||% NULL
      })

      # If every non-NULL is a scalar atomic -> produce a simple atomic column
      all_scalar <- all(vapply(vals, function(v) is.null(v) || is_scalar_atomic(v), logical(1)))

      if (all_scalar && requireNamespace("vctrs", quietly = TRUE)) {
        nonnull <- vals[!vapply(vals, is.null, logical(1))]
        if (length(nonnull)) {
          ptype  <- vctrs::vec_ptype_common(!!!nonnull)
          na_val <- vctrs::vec_cast(NA, ptype)
          casted <- lapply(vals, function(v) if (is.null(v)) na_val else vctrs::vec_cast(v, ptype))
          vctrs::vec_c(!!!casted)
        } else {
          rep(NA_character_, length(vals))
        }
      } else if (all_scalar) {
        nn <- vals[!vapply(vals, is.null, logical(1))]
        if (length(nn) && all(vapply(nn, is.numeric,  logical(1)))) {
          vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v),  numeric(1))
        } else if (length(nn) && all(vapply(nn, is.logical, logical(1)))) {
          vapply(vals, function(v) if (is.null(v)) NA       else as.logical(v), logical(1))
        } else {
          vapply(vals, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
        }
      } else {
        vals
      }
    })

    names(param_cols_list) <- all_model_param_names
    params_df <- tibble::as_tibble(param_cols_list)
    # ------------------------------------------------------------------
    # avoid duplicate names coming from the input tibble
    # ------------------------------------------------------------------
    dup_params <- intersect(names(params_df), names(results_df))
    if (length(dup_params) > 0) {
      for (p in dup_params) {
        if (!identical(params_df[[p]], results_df[[p]])) {
          warning(sprintf(
            "Parameter '%s' is present both in the input data and in the config; keeping the existing column.",
            p))
        }
      }
      params_df <- params_df[, setdiff(names(params_df), dup_params), drop = FALSE]
    }

    results_df <- dplyr::bind_cols(results_df, params_df)
  }


  # Identify metadata columns (everything except standard columns)
  meta_cols <- setdiff(names(results_df), c("provider", "model", all_model_param_names,
                                            config_col, "response_text", "raw_response_json",
                                            "success", "error_message"))

  # Separate standard config columns from other parameters
  standard_config_cols <- c("provider", "model")
  ordered_param_cols <- all_model_param_names[!all_model_param_names %in% standard_config_cols]

  # Define column order: metadata, config info, parameters, results
  final_cols_order <- c(
    meta_cols,
    standard_config_cols,
    ordered_param_cols,
    "response_text", "raw_response_json", "success", "error_message"
  )

  # Only include columns that exist
  final_cols_order_existing <- final_cols_order[final_cols_order %in% names(results_df)]
  remaining_cols <- setdiff(names(results_df), final_cols_order_existing)

  results_df <- results_df[, c(final_cols_order_existing, remaining_cols)]

  return(results_df)
}

#' Parallel API calls: Parameter Sweep - Vary One Parameter, Fixed Message
#'
#' Sweeps through different values of a single parameter while keeping the message constant.
#' Perfect for hyperparameter tuning, temperature experiments, etc.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param base_config Base llm_config object to modify.
#' @param param_name Character. Name of the parameter to vary (e.g., "temperature", "max_tokens").
#' @param param_values Vector. Values to test for the parameter.
#' @param messages A character vector or a list of message objects (same for all calls).
#' @param ... Additional arguments passed to `call_llm_par` (e.g., tries, verbose, progress).
#'
#' @return A tibble with columns: swept_param_name, the varied parameter column, provider, model,
#'   all other model parameters, response_text, raw_response_json, success, error_message.
#' @section Parallel Workflow:
#' All parallel functions require the `future` backend to be configured.
#' The recommended workflow is:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#'
#' @seealso \code{\link{setup_llm_parallel}}, \code{\link{reset_llm_parallel}}
#' @export
#'
#' @examples
#' \dontrun{
#'   # Temperature sweep
#'   config <- llm_config(provider = "openai", model = "gpt-4.1-nano")
#'
#'   messages <- "What is 15 * 23?"
#'   temperatures <- c(0, 0.3, 0.7, 1.0, 1.5)
#'
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'   results <- call_llm_sweep(config, "temperature", temperatures, messages)
#'   results |> dplyr::select(temperature, response_text)
#'   reset_llm_parallel(verbose = TRUE)
#' }
call_llm_sweep <- function(base_config,
                           param_name,
                           param_values,
                           messages,
                           ...) {

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }

  if (length(param_values) == 0) {
    warning("No parameter values provided. Returning empty tibble.")
    return(tibble::tibble(
      swept_param_name = character(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      raw_response_json = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  # Build experiments tibble
  experiments <- tibble::tibble(
    !!param_name := param_values, # Use the actual parameter name directly
    config = lapply(param_values, function(val) {
      modified_config <- base_config
      if (is.null(modified_config$model_params)) modified_config$model_params <- list()
      modified_config$model_params[[param_name]] <- val
      modified_config
    }),
    messages = rep(list(messages), length(param_values))
  )

  # Run parallel processing
  results_final <- call_llm_par(experiments, ...)
  results_final$config <- NULL
  return(results_final)
}

#' Parallel API calls: Fixed Config, Multiple Messages
#'
#' Broadcasts different messages using the same configuration in parallel.
#' Perfect for batch processing different prompts with consistent settings.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param config Single llm_config object to use for all calls.
#' @param messages A character vector (each element is a prompt) OR
#'   a list where each element is a pre-formatted message list.
#' @param ... Additional arguments passed to `call_llm_par` (e.g., tries, verbose, progress).
#'
#' @return A tibble with columns: message_index (metadata), provider, model,
#'   all model parameters, response_text, raw_response_json, success, error_message.
#' @section Parallel Workflow:
#' All parallel functions require the `future` backend to be configured.
#' The recommended workflow is:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#'
#' @seealso \code{\link{setup_llm_parallel}}, \code{\link{reset_llm_parallel}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Broadcast different questions
#'   config <- llm_config(provider = "openai", model = "gpt-4.1-nano")
#'
#'   messages <- list(
#'     list(list(role = "user", content = "What is 2+2?")),
#'     list(list(role = "user", content = "What is 3*5?")),
#'     list(list(role = "user", content = "What is 10/2?"))
#'   )
#'
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'   results <- call_llm_broadcast(config, messages)
#'   reset_llm_parallel(verbose = TRUE)
#' }
call_llm_broadcast <- function(config,
                               messages,
                               ...) {

  # Allow plain character vectors for convenience
  if (is.character(messages))
    messages <- as.list(messages)

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }

  if (length(messages) == 0) {
    warning("No messages provided. Returning empty tibble.")
    return(tibble::tibble(
      message_index = integer(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      raw_response_json = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  # Build experiments tibble
  experiments <- tibble::tibble(
    message_index = seq_along(messages),
    config = rep(list(config), length(messages)),
    messages = messages
  )

  # Run parallel processing
  results_final <- call_llm_par(experiments, ...)
  results_final$config <- NULL
  return(results_final)
}

#' Parallel API calls: Multiple Configs, Fixed Message
#'
#' Compares different configurations (models, providers, settings) using the same message.
#' Perfect for benchmarking across different models or providers.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param configs_list A list of llm_config objects to compare.
#' @param messages A character vector or a list of message objects (same for all configs).
#' @param ... Additional arguments passed to `call_llm_par` (e.g., tries, verbose, progress).
#'
#' @return A tibble with columns: config_index (metadata), provider, model,
#'   all varying model parameters, response_text, raw_response_json, success, error_message.
#' @section Parallel Workflow:
#' All parallel functions require the `future` backend to be configured.
#' The recommended workflow is:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#'
#' @seealso \code{\link{setup_llm_parallel}}, \code{\link{reset_llm_parallel}}
#' @export
#'
#' @examples
#' \dontrun{
#'   # Compare different models
#'   config1 <- llm_config(provider = "openai", model = "gpt-4o-mini")
#'   config2 <- llm_config(provider = "openai", model = "gpt-4.1-nano")
#'
#'   configs_list <- list(config1, config2)
#'   messages <- "Explain quantum computing"
#'
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'   results <- call_llm_compare(configs_list, messages)
#'   reset_llm_parallel(verbose = TRUE)
#' }
call_llm_compare <- function(configs_list,
                             messages,
                             ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }

  if (length(configs_list) == 0) {
    warning("No configs provided. Returning empty tibble.")
    return(tibble::tibble(
      config_index = integer(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      raw_response_json = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  # Build experiments tibble
  experiments <- tibble::tibble(
    config_index = seq_along(configs_list),
    config = configs_list,
    messages = rep(list(messages), length(configs_list))
  )

  # Run parallel processing
  results_final <- call_llm_par(experiments, ...)
  results_final$config <- NULL
  return(results_final)
}

#' Parallel LLM Processing with Tibble-Based Experiments (Core Engine)
#'
#' Processes experiments from a tibble where each row contains a config and message pair.
#' This is the core parallel processing function. Metadata columns are preserved.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param experiments A tibble/data.frame with required list-columns 'config' (llm_config objects)
#'   and 'messages' (character vector OR message list).
#' @param simplify Whether to cbind 'experiments' to the output data frame or not.
#' @param tries Integer. Number of retries for each call. Default is 10.
#' @param wait_seconds Numeric. Initial wait time (seconds) before retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 3.
#' @param verbose Logical. If TRUE, prints progress and debug information.
#' @param memoize Logical. If TRUE, enables caching for identical requests.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress bar.
#' @param json_output Deprecated. Raw JSON string is always included as raw_response_json.
#'                  This parameter is kept for backward compatibility but has no effect.
#'
#' @return A tibble containing all original columns plus:
#' \itemize{
#'   \item \code{response_text} – assistant text (or \code{NA} on failure)
#'   \item \code{raw_response_json} – raw JSON string
#'   \item \code{success}, \code{error_message}
#'   \item \code{finish_reason} – e.g. "stop", "length", "filter", "tool", or "error:`category`"
#'   \item \code{sent_tokens}, \code{rec_tokens}, \code{total_tokens}, \code{reasoning_tokens}
#'   \item \code{response_id}
#'   \item \code{duration} – seconds
#'   \item \code{response} – the full \code{llmr_response} object (or \code{NA} on failure)
#' }
#'
#' The `response` column holds `llmr_response` objects on success, or `NULL` on failure.

#' @section Parallel Workflow:
#' All parallel functions require the `future` backend to be configured.
#' The recommended workflow is:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#'
#' @seealso
#' For setting up the environment: \code{\link{setup_llm_parallel}}, \code{\link{reset_llm_parallel}}.
#' For simpler, pre-configured parallel tasks: \code{\link{call_llm_broadcast}}, \code{\link{call_llm_sweep}}, \code{\link{call_llm_compare}}.
#' For creating experiment designs: \code{\link{build_factorial_experiments}}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple example: Compare two models on one prompt
#' cfg1 <- llm_config("openai", "gpt-4.1-nano")
#' cfg2 <- llm_config("groq", "llama-3.3-70b-versatile")
#'
#' experiments <- tibble::tibble(
#'   model_id = c("gpt-4.1-nano", "groq-llama-3.3"),
#'   config = list(cfg1, cfg2),
#'   messages = "Count the number of the letter e in this word: Freundschaftsbeziehungen "
#' )
#'
#' setup_llm_parallel(workers = 2)
#' results <- call_llm_par(experiments, progress = TRUE)
#' reset_llm_parallel()
#'
#' print(results[, c("model_id", "response_text")])
#'
#' }
call_llm_par <- function(experiments,
                         simplify = TRUE,
                         tries = 10,
                         wait_seconds = 2,
                         backoff_factor = 3,
                         verbose = FALSE,
                         memoize = FALSE,
                         max_workers = NULL,
                         progress = FALSE,
                         json_output = NULL) {

  if (!is.null(json_output) && verbose) {
    message("Note: The 'json_output' parameter is deprecated. Raw JSON is always included as 'raw_response_json'.")
  }

  if (!requireNamespace("future", quietly = TRUE)) {
    stop("The 'future' package is required. Please install it with: install.packages('future')")
  }
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("The 'future.apply' package is required. Please install it with: install.packages('future.apply')")
  }
  if (progress && !requireNamespace("progressr", quietly = TRUE)) {
    warning("The 'progressr' package is not available. Progress tracking will be disabled.")
    progress <- FALSE
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required. Please install it with: install.packages('dplyr')")
  }

  if (!is.data.frame(experiments)) {
    stop("experiments must be a tibble/data.frame")
  }
  if (!all(c("config", "messages") %in% names(experiments))) {
    stop("experiments must have 'config' and 'messages' columns")
  }
  if (nrow(experiments) == 0) {
    warning("No experiments provided. Returning empty input tibble with result columns.")
    return(dplyr::bind_cols(experiments, tibble::tibble(
      response_text     = character(0),
      raw_response_json = character(0),
      success           = logical(0),
      error_message     = character(0),
      finish_reason     = character(0),
      sent_tokens       = integer(0),
      rec_tokens        = integer(0),
      total_tokens      = integer(0),
      reasoning_tokens  = integer(0),
      response_id       = character(0),
      duration          = numeric(0),
      response          = list(),
      status_code = integer(0),
      error_code  = character(0),
      bad_param   = character(0)
    )))
  }

  for (i in seq_len(nrow(experiments))) {
    if (!inherits(experiments$config[[i]], "llm_config")) {
      stop(sprintf("Row %d 'config' is not an llm_config object.", i))
    }
  }

  if (is.null(max_workers)) {
    max_workers <- min(future::availableCores(omit = 1L), nrow(experiments))
    max_workers <- max(1, max_workers)
  }
  current_plan <- future::plan()
  on.exit(future::plan(current_plan), add = TRUE)
  if (!inherits(current_plan, "FutureStrategy") || inherits(current_plan, "sequential")) {
    future::plan(future::multisession, workers = max_workers)
  }

  if (verbose) {
    n_metadata_cols <- ncol(experiments) - 2
    message(sprintf("Processing %d experiments with %d user metadata columns",
                    nrow(experiments), n_metadata_cols))
  }

  par_worker <- function(i_val) {
    # small helpers to guarantee column types
    as_char_or_na <- function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
      as.character(x[[1]])
    }
    as_int_or_na <- function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_integer_)
      suppressWarnings(as.integer(x[[1]]))
    }

    start_time <- Sys.time()
    current_config   <- experiments$config[[i_val]]
    current_messages <- experiments$messages[[i_val]]
    raw_json_str <- NA_character_

    tryCatch({
      result_content <- call_llm_robust(
        config        = current_config,
        messages      = current_messages,
        tries         = tries,
        wait_seconds  = wait_seconds,
        backoff_factor= backoff_factor,
        verbose       = FALSE,
        memoize       = memoize
      )

      raw_json_str <- attr(result_content, "raw_json") %||% NA_character_

      is_obj <- inherits(result_content, "llmr_response")
      fr  <- if (is_obj) finish_reason(result_content) else NA_character_
      u   <- if (is_obj) tokens(result_content)        else list(sent=NA, rec=NA, total=NA, reasoning=NA)
      rid <- if (is_obj) result_content$response_id    else NA_character_
      dur <- if (is_obj) result_content$duration_s     else as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      list(
        row_index         = i_val,
        response_text     = as.character(result_content),
        raw_response_json = raw_json_str,
        success           = TRUE,
        error_message     = NA_character_,
        finish_reason     = fr,
        sent_tokens       = as_int_or_na(u$sent),
        rec_tokens        = as_int_or_na(u$rec),
        total_tokens      = as_int_or_na(u$total),
        reasoning_tokens  = as_int_or_na(u$reasoning),
        response_id       = as_char_or_na(rid),
        duration          = dur,
        status_code       = NA_integer_,      # <-- force integer
        error_code        = NA_character_,    # <-- force character
        bad_param         = NA_character_,    # <-- force character
        response          = list(if (is_obj) result_content else NULL)
      )

    }, error = function(e) {
      # pull fields, then coerce types explicitly
      sc0  <- tryCatch(e$status_code, error = function(...) NA)
      rid0 <- tryCatch(e$request_id,  error = function(...) NA)
      prm0 <- tryCatch(e$param,       error = function(...) NA)
      cod0 <- tryCatch(e$code,        error = function(...) NA)

      list(
        row_index         = i_val,
        response_text     = NA_character_,
        raw_response_json = raw_json_str,
        success           = FALSE,
        error_message     = conditionMessage(e),
        finish_reason     = paste0("error:", {
          cls <- class(e)
          hit <- grep("^llmr_api_(.+)_error$", cls, value = TRUE)
          if (length(hit)) sub("^llmr_api_(.+)_error$", "\\1", hit[[1]]) else "unknown"
        }),
        sent_tokens       = NA_integer_,
        rec_tokens        = NA_integer_,
        total_tokens      = NA_integer_,
        reasoning_tokens  = NA_integer_,
        response_id       = as_char_or_na(rid0),   # character
        duration          = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
        status_code       = as_int_or_na(sc0),     # integer
        error_code        = as_char_or_na(cod0),   # character (prevents int/char clash)
        bad_param         = as_char_or_na(prm0),   # character
        response          = list(NULL)
      )
    })
  }


  if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(experiments))
      api_call_results_list <- future.apply::future_lapply(
        seq_len(nrow(experiments)),
        function(k) { res <- par_worker(k); p(); res },
        future.seed = TRUE,
        future.packages = "LLMR",
        future.globals  = TRUE
      )
    })
  } else {
    api_call_results_list <- future.apply::future_lapply(
      seq_len(nrow(experiments)),
      par_worker,
      future.seed = TRUE,
      future.packages = "LLMR",
      future.globals  = TRUE
    )
  }

  api_results_df <- dplyr::bind_rows(api_call_results_list)

  output_df <- experiments
  # --- Robust column handling to prevent overwriting user data ---
  new_cols_spec <- list(
    response_text     = NA_character_,
    raw_response_json = NA_character_,
    success           = NA,
    error_message     = NA_character_,
    finish_reason     = NA_character_,
    sent_tokens       = NA_integer_,
    rec_tokens        = NA_integer_,
    total_tokens      = NA_integer_,
    reasoning_tokens  = NA_integer_,
    response_id       = NA_character_,
    duration          = NA_real_,
    status_code       = NA_integer_,
    error_code        = NA_character_,
    bad_param         = NA_character_,
    response          = vector("list", nrow(experiments))
  )
  existing_names <- names(output_df)
  final_col_names <- stats::setNames(as.list(names(new_cols_spec)), names(new_cols_spec))
  for (col_name in names(new_cols_spec)) {
    final_name <- col_name
    suffix <- 1
    while (final_name %in% existing_names) {
      final_name <- paste0(col_name, ".", suffix)
      suffix <- suffix + 1
    }
    if (final_name != col_name && verbose) {
      warning(sprintf("Input data already has a column named '%s'. Results will be in '%s'.", col_name, final_name))
    }
    output_df[[final_name]] <- new_cols_spec[[col_name]]
    final_col_names[[col_name]] <- final_name
    existing_names <- c(existing_names, final_name)
  }

  # Fill by row index
  idx <- api_results_df$row_index
  for (original_name in names(final_col_names)) {
    final_name <- final_col_names[[original_name]]
    output_df[[final_name]][idx] <- api_results_df[[original_name]]
  }

  if (verbose) {
    succ_col <- final_col_names$success
    successful_calls <- sum(output_df[[succ_col]], na.rm = TRUE)
    message(sprintf("Parallel processing completed: %d/%d experiments successful",
                    successful_calls, nrow(output_df)))
  }

  if (simplify) {
    output_df <- .unnest_config_to_cols(output_df, config_col = "config")
  }
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(output_df) else output_df
}

#' Build Factorial Experiment Design
#'
#' Creates a tibble of experiments for factorial designs where you want to test
#' all combinations of configs, messages, and repetitions with automatic metadata.
#'
#' @param configs List of llm_config objects to test.
#' @param repetitions Integer. Number of repetitions per combination. Default is 1.
#' @param config_labels Character vector of labels for configs. If NULL, uses "provider_model".
#' @param user_prompts Character vector (or list) of user‑turn prompts.
#' @param user_prompt_labels Optional labels for the user prompts.
#' @param system_prompts Optional character vector of system messages (recycled against user prompts). Missing/NA values are ignored; messages are user-only.
#' @param system_prompt_labels Optional labels for the system prompts.
#'
#' @return A tibble with columns: config (list-column), messages (list-column),
#'   config_label, user_prompt_label, system_prompt_label, and repetition. Ready for use with call_llm_par().
#' @export
#'
#' @examples
#' \dontrun{
#'   # Factorial design: 3 configs x 2 user prompts x 10 reps = 60 experiments
#'   configs <- list(gpt4_config, claude_config, llama_config)
#'   user_prompts <- c("Control prompt", "Treatment prompt")
#'
#'   experiments <- build_factorial_experiments(
#'     configs = configs,
#'     user_prompts = user_prompts,
#'     repetitions = 10,
#'     config_labels = c("gpt4", "claude", "llama"),
#'     user_prompt_labels = c("control", "treatment")
#'   )
#'
#'   # Use with call_llm_par
#'   results <- call_llm_par(experiments, progress = TRUE)
#' }
build_factorial_experiments <- function(configs,
                                        user_prompts,
                                        system_prompts = NULL,
                                        repetitions = 1,
                                        config_labels  = NULL,
                                        user_prompt_labels = NULL,
                                        system_prompt_labels = NULL
                                        ) {

  # if (!missing(messages)) {
  #   lifecycle::deprecate_stop("0.5.0", "build_factorial_experiments(messages = )",
  #                             "build_factorial_experiments(user_prompts = )")
  # }

  if (inherits(configs, "llm_config")) configs <- list(configs)

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("The 'tidyr' package is required for expand_grid. Please install it with: install.packages('tidyr')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required for joins. Please install it with: install.packages('dplyr')")
  }

  # Validate inputs
  if (length(configs) == 0 || length(user_prompts) == 0) {
    stop("Both configs and user_prompts must have at least one element")
  }

  # Create config labels if not provided
  if (is.null(config_labels)) {
    config_labels <- sapply(configs, function(cfg) {
      paste(cfg$provider %||% "NA", cfg$model %||% "NA", sep = "_")
    })
  } else if (length(config_labels) != length(configs)) {
    stop("config_labels must have the same length as configs")
  }

  # Create user‑prompt labels if not provided
  if (is.null(user_prompt_labels)) {
    user_prompt_labels <- paste0("user_", seq_along(user_prompts))
  } else if (length(user_prompt_labels) != length(user_prompts)) {
    stop("user_prompt_labels must have the same length as user_prompts")
  }
  ## ------------------------------------------------------------------------
  configs_df <- tibble::tibble(
    config_idx   = seq_along(configs),
    config       = configs,
    config_label = config_labels %||%
      paste(vapply(configs, `[[`, "", "model"), seq_along(configs))
  )

  user_df <- tibble::tibble(
    user_idx   = seq_along(user_prompts),
    user_prompt = user_prompts,
    user_prompt_label = user_prompt_labels %||%
      paste0("user_", seq_along(user_prompts))
  )

  sys_df <- if (!is.null(system_prompts)) tibble::tibble(
    sys_idx   = seq_along(system_prompts),
    system_prompt = system_prompts,
    system_prompt_label = system_prompt_labels %||%
      paste0("system_", seq_along(system_prompts))
  ) else tibble::tibble(
    sys_idx = 1L,
    system_prompt = NA_character_,
    system_prompt_label = NA_character_
  )

  experiments <- tidyr::expand_grid(
    configs_df, user_df, sys_df,
    repetition = seq_len(repetitions)
  ) |>
    dplyr::mutate(
      messages = purrr::map2(user_prompt, system_prompt, .compose_msg)
    ) |>
    dplyr::select(config, messages,
                  config_label,
                  user_prompt_label,
                  system_prompt_label,
                  repetition)

  return(experiments)
}



#' Setup Parallel Environment for LLM Processing
#'
#' Convenience function to set up the future plan for optimal LLM parallel processing.
#' Automatically detects system capabilities and sets appropriate defaults.
#'
#' @param workers Integer. Number of workers to use. If NULL, auto-detects optimal number
#'                (availableCores - 1, capped at 8). If called as \code{setup_llm_parallel(4)},
#'                the single numeric positional argument is interpreted as \code{workers}.
#' @param strategy Character. The future strategy to use. Options: "multisession", "multicore", "sequential".
#'                If NULL (default), automatically chooses "multisession".
#' @param verbose Logical. If TRUE, prints setup information.
#'
#' @return Invisibly returns the previous future plan.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Automatic setup
#'   setup_llm_parallel()
#'
#'   # Manual setup with specific workers
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'
#'   # Force sequential processing for debugging
#'   setup_llm_parallel(strategy = "sequential")
#'
#'   # Restore old plan if needed
#'   reset_llm_parallel()
#' }
setup_llm_parallel <- function(workers = NULL, strategy = NULL, verbose = FALSE) {

  if (!requireNamespace("future", quietly = TRUE)) {
    stop("The 'future' package is required. Please install it with: install.packages('future')")
  }

  current_plan <- future::plan()
  strategy <- strategy %||% "multisession"

  if (is.null(workers)) {
    available_cores <- future::availableCores()
    workers <- max(1, available_cores - 1)
    workers <- min(workers, 8) # Cap at reasonable maximum for API calls
  } else {
    workers <- max(1, as.integer(workers))
  }

  if (verbose) {
    message(sprintf("Setting up parallel environment:"))
    message(sprintf("  Requested Strategy: %s", strategy))
    message(sprintf("  Requested Workers: %d", workers))
    message(sprintf("  Available cores on system: %d", future::availableCores()))
  }

  if (strategy == "sequential") {
    future::plan(future::sequential)
  } else if (strategy == "multicore") {
    if (.Platform$OS.type == "windows") {
      warning("'multicore' is not supported on Windows. Using 'multisession' instead.")
      future::plan(future::multisession, workers = workers)
    } else {
      future::plan(future::multicore, workers = workers)
    }
  } else if (strategy == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else {
    stop("Invalid strategy. Choose from: 'sequential', 'multicore', 'multisession'")
  }

  if (verbose) {
    message(sprintf("Parallel environment set to: %s with %d workers.",
                    class(future::plan())[1], future::nbrOfWorkers()))
  }

  invisible(current_plan)
}

#' Reset Parallel Environment
#'
#' Resets the future plan to sequential processing.
#'
#' @param verbose Logical. If TRUE, prints reset information.
#'
#' @return Invisibly returns the future plan that was in place before resetting to sequential.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Setup parallel processing
#'   old_plan <- setup_llm_parallel(workers = 2)
#'
#'   # Do some parallel work...
#'
#'   # Reset to sequential
#'   reset_llm_parallel(verbose = TRUE)
#'
#'   # Optionally restore the specific old_plan if it was non-sequential
#'   # future::plan(old_plan)
#' }
reset_llm_parallel <- function(verbose = FALSE) {

  if (!requireNamespace("future", quietly = TRUE)) {
    warning("The 'future' package is not available. Cannot reset plan.")
    return(invisible(NULL))
  }

  if (verbose) {
    message("Resetting parallel environment to sequential processing...")
  }

  previous_plan <- future::plan(future::sequential)

  if (verbose) {
    message("Parallel environment reset complete. Previous plan was: ", class(previous_plan)[1])
  }

  invisible(previous_plan)
}
