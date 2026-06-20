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
          tryCatch({
            ptype  <- vctrs::vec_ptype_common(!!!nonnull)
            na_val <- vctrs::vec_cast(NA, ptype)
            casted <- lapply(vals, function(v) if (is.null(v)) na_val else vctrs::vec_cast(v, ptype))
            vctrs::vec_c(!!!casted)
          }, error = function(e) {
            vapply(vals, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
          })
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
#' Use [setup_llm_parallel()] when you want explicit control over workers.
#'
#' @param base_config Base llm_config object to modify.
#' @param param_name Character. Name of the parameter to vary (e.g., "temperature", "max_tokens").
#' @param param_values Vector. Values to test for the parameter.
#' @param messages A character vector or a list of message objects (same for all calls).
#' @param ... Additional arguments passed to `call_llm_par` (e.g., tries, verbose, progress).
#'
#' @return A tibble with columns: swept_param_name, the varied parameter column,
#'   the `config` list-column (so [llm_par_resume()] can re-run failures),
#'   provider, model, all other model parameters, response_text,
#'   raw_response_json, success, error_message, and the other diagnostics
#'   documented in [call_llm_par()].
#' @section Parallel Workflow:
#' Recommended workflow:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#' If the active future plan is sequential, [call_llm_par()] temporarily switches
#' to `multisession` for the duration of the call.
#'
#' @seealso \code{\link{setup_llm_parallel}}, \code{\link{reset_llm_parallel}},
#'   \code{\link{call_llm_par}}
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

  # Run parallel processing. The config list-column is kept so that
  # llm_par_resume() can re-run failed rows.
  results_final <- call_llm_par(experiments, ...)
  return(results_final)
}

#' Parallel API calls: Fixed Config, Multiple Messages
#'
#' Broadcasts different messages using the same configuration in parallel.
#' Perfect for batch processing different prompts with consistent settings.
#' Use [setup_llm_parallel()] when you want explicit control over workers.
#'
#' @param config Single llm_config object to use for all calls.
#' @param messages A character vector (each element is a prompt) OR
#'   a list where each element is a pre-formatted message list.
#' @param ... Additional arguments passed to `call_llm_par` (e.g., tries, verbose, progress).
#'
#' @return A tibble with columns: message_index (metadata), the `config`
#'   list-column (so [llm_par_resume()] can re-run failures), provider, model,
#'   all model parameters, response_text, raw_response_json, success,
#'   error_message, and the other diagnostics documented in [call_llm_par()].
#' @section Parallel Workflow:
#' Recommended workflow:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#' If the active future plan is sequential, [call_llm_par()] temporarily switches
#' to `multisession` for the duration of the call.
#'
#' @seealso \code{\link{setup_llm_parallel}}, \code{\link{reset_llm_parallel}},
#'   \code{\link{call_llm_par}}, \code{\link{llm_fn}}, \code{\link{llm_mutate}}
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

  # Build experiments tibble; zero messages fall through to call_llm_par(),
  # whose empty return carries the full diagnostic column schema.
  experiments <- tibble::tibble(
    message_index = seq_along(messages),
    config = rep(list(config), length(messages)),
    messages = messages
  )

  # Run parallel processing. The config list-column is kept so that
  # llm_par_resume() can re-run failed rows.
  results_final <- call_llm_par(experiments, ...)
  return(results_final)
}

#' Parallel API calls: Multiple Configs, Fixed Message
#'
#' Compares different configurations (models, providers, settings) using the same message.
#' Perfect for benchmarking across different models or providers.
#' Use [setup_llm_parallel()] when you want explicit control over workers.
#'
#' @param configs_list A list of llm_config objects to compare.
#' @param messages A character vector or a list of message objects (same for all configs).
#' @param ... Additional arguments passed to `call_llm_par` (e.g., tries, verbose, progress).
#'
#' @return A tibble with columns: config_index (metadata), the `config`
#'   list-column (so [llm_par_resume()] can re-run failures), provider, model,
#'   all varying model parameters, response_text, raw_response_json, success,
#'   error_message, and the other diagnostics documented in [call_llm_par()].
#' @section Parallel Workflow:
#' Recommended workflow:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#' If the active future plan is sequential, [call_llm_par()] temporarily switches
#' to `multisession` for the duration of the call.
#'
#' @seealso \code{\link{setup_llm_parallel}}, \code{\link{reset_llm_parallel}},
#'   \code{\link{call_llm_par}}
#' @export
#'
#' @examples
#' \dontrun{
#'   # Compare different models
#'   config1 <- llm_config(provider = "openai", model = "gpt-5-nano")
#'   config2 <- llm_config(provider = "groq", model = "openai/gpt-oss-20b")
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

  # Run parallel processing. The config list-column is kept so that
  # llm_par_resume() can re-run failed rows.
  results_final <- call_llm_par(experiments, ...)
  return(results_final)
}

#' Parallel LLM Processing with Tibble-Based Experiments (Core Engine)
#'
#' Processes experiments from a tibble where each row contains a config and message pair.
#' This is the core parallel processing function. Metadata columns are preserved.
#' Use [setup_llm_parallel()] when you want explicit control over workers.
#'
#' @param experiments A tibble/data.frame with required list-columns 'config' (llm_config objects)
#'   and 'messages' (character vector OR message list).
#' @param simplify If TRUE (default), provider, model, and the model parameters
#'   stored in each row's config are unnested into regular columns for easy
#'   filtering and grouping.
#' @param tries Integer. Total number of attempts per call (first call plus
#'   retries). Default is 10.
#' @param wait_seconds Numeric. Initial wait time (seconds) before retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 3.
#' @param verbose Logical. If TRUE, prints progress and debug information.
#' @param memoize Logical. If TRUE, enables caching for identical requests.
#'   Note that under a multisession plan each worker process keeps its own
#'   cache, so deduplication is per worker, not global.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress bar.
#' @param json_output Deprecated. Raw JSON string is always included as raw_response_json.
#'                  This parameter is kept for backward compatibility but has no effect.
#' @param start_jitter Each call starts after a uniformly distributed delay
#'   between 0 and \code{start_jitter} seconds. The default is 0 (no delay);
#'   set a few seconds when launching very large runs against a provider with
#'   strict burst limits.
#' @param .request_hash Logical. If \code{TRUE}, append a \code{request_hash}
#'   column (the same key \code{\link{llm_request_hash}()} produces) so the
#'   results can be joined to the audit log. Default \code{FALSE}; the column is
#'   omitted unless requested. See also \code{\link{llm_add_request_hash}()}.
#'
#' @return A tibble containing all original columns plus:
#' \itemize{
#'   \item \code{response_text} - assistant text (or \code{NA} on failure)
#'   \item \code{raw_response_json} - raw JSON string (on failure: the
#'         provider's error body when available)
#'   \item \code{success}, \code{error_message}
#'   \item \code{finish_reason} - e.g. "stop", "length", "filter", "tool", or "error:`category`"
#'   \item \code{sent_tokens}, \code{rec_tokens}, \code{total_tokens}, \code{reasoning_tokens}
#'   \item \code{response_id}
#'   \item \code{duration} - seconds
#'   \item \code{status_code}, \code{error_code}, \code{bad_param} - error
#'         diagnostics (NA on success)
#'   \item \code{response} - the full \code{llmr_response} object (or \code{NULL} on failure)
#' }
#'
#' The `response` column holds `llmr_response` objects on success, or `NULL` on failure.

#' @section Parallel Workflow:
#' Recommended workflow:
#' 1. Call `setup_llm_parallel()` once at the start of your script.
#' 2. Run one or more parallel experiments (e.g., `call_llm_broadcast()`).
#' 3. Call `reset_llm_parallel()` at the end to restore sequential processing.
#' If the active future plan is sequential, this function temporarily switches
#' to `multisession` for the duration of the call.
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
#' cfg2 <- llm_config("groq", "openai/gpt-oss-20b")
#'
#' experiments <- tibble::tibble(
#'   model_id = c("gpt-4.1-nano", "groq-gpt-oss-20b"),
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
                         backoff_factor = 120^(1/tries),
                         verbose = FALSE,
                         memoize = FALSE,
                         max_workers = NULL,
                         progress = FALSE,
                         json_output = NULL,
                         start_jitter = 0,
                         .request_hash = FALSE) {

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
      cached_tokens     = integer(0),
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

  .llmr_log_file_capture <- getOption("llmr.log_file")
  .llmr_log_msgs_capture <- isTRUE(getOption("llmr.log_messages", TRUE))
  .llmr_parent_pid <- Sys.getpid()

  current_plan <- future::plan()
  on.exit(future::plan(current_plan), add = TRUE)
  if (!is.null(.llmr_log_file_capture)) {
    on.exit(try(.llmr_log_merge(.llmr_log_file_capture), silent = TRUE), add = TRUE)
  }
  if (!inherits(current_plan, "FutureStrategy") || inherits(current_plan, "sequential")) {
    future::plan(future::multisession, workers = max_workers)
  }

  if (verbose) {
    n_metadata_cols <- ncol(experiments) - 2
    message(sprintf("Processing %d experiments with %d user metadata columns",
                    nrow(experiments), n_metadata_cols))
  }

  par_worker <- function(i_val) {
    if (Sys.getpid() != .llmr_parent_pid) {
      on.exit(options(llmr.log_file = NULL,
                      llmr.log_messages = NULL,
                      llmr.log_parallel = NULL), add = TRUE)
      if (is.null(.llmr_log_file_capture)) {
        options(llmr.log_file = NULL,
                llmr.log_messages = NULL,
                llmr.log_parallel = NULL)
      } else {
        options(
          llmr.log_file     = .llmr_log_file_capture,
          llmr.log_messages = .llmr_log_msgs_capture,
          llmr.log_parallel = TRUE
        )
      }
    }

    # small helpers to guarantee column types
    as_char_or_na <- function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
      as.character(x[[1]])
    }
    as_int_or_na <- function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_integer_)
      suppressWarnings(as.integer(x[[1]]))
    }

    # random small delay so as to spread out the parallel calls
    if (start_jitter>0)
      Sys.sleep(stats::runif(1, 0, start_jitter))

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
      u   <- if (is_obj) tokens(result_content)        else list(sent=NA, rec=NA, total=NA, reasoning=NA, cached=NA)
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
        cached_tokens     = as_int_or_na(u$cached),
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
      # the provider's raw error body, when the typed condition carries one,
      # is preserved in raw_response_json so failures stay inspectable
      bod0 <- tryCatch(e$body, error = function(...) NULL)
      raw_err_json <- if (!is.null(bod0)) {
        tryCatch(as.character(jsonlite::toJSON(bod0, auto_unbox = TRUE, null = "null")),
                 error = function(...) raw_json_str)
      } else raw_json_str

      list(
        row_index         = i_val,
        response_text     = NA_character_,
        raw_response_json = raw_err_json,
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
        cached_tokens     = NA_integer_,
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

  if (!is.null(.llmr_log_file_capture)) {
    try(.llmr_log_merge(.llmr_log_file_capture), silent = TRUE)
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
    cached_tokens     = NA_integer_,
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
    if (final_name != col_name) {
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

  # Opt-in: append the audit-log join key, computed from the retained config and
  # messages columns (off by default; the column is added only when requested).
  if (isTRUE(.request_hash)) {
    output_df <- .llmr_add_request_hash(output_df)
  }

  if (simplify) {
    output_df <- .unnest_config_to_cols(output_df, config_col = "config")
  }
  res <- if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(output_df) else output_df
  class(res) <- unique(c("llmr_experiment", class(res)))
  res
}

# Append a `request_hash` column from a frame's retained `config`/`messages`
# columns. Internal worker for both `call_llm_par(.request_hash=TRUE)` and the
# exported `llm_add_request_hash()`.
.llmr_add_request_hash <- function(df) {
  if (!all(c("config", "messages") %in% names(df))) {
    stop("`config` and `messages` columns are required to add `request_hash`.")
  }
  df$request_hash <- vapply(seq_len(nrow(df)), function(i) {
    tryCatch(
      llm_request_hash(config = df$config[[i]], messages = df$messages[[i]]),
      error = function(e) NA_character_)
  }, character(1))
  df
}

#' Append the audit-log request hash to a parallel-results frame
#'
#' Adds a `request_hash` column to a data frame that still carries the `config`
#' and `messages` list-columns (such as the result of [call_llm_par()]). The
#' hash is the same key [llm_request_hash()] produces, so the parallel path can
#' be joined to the audit log written by [llm_log_enable()]. Equivalent to
#' calling `call_llm_par(..., .request_hash = TRUE)` after the fact.
#'
#' @param df A data frame with `config` and `messages` columns.
#' @return `df` with an added `request_hash` character column.
#' @seealso [call_llm_par()], [llm_request_hash()].
#' @export
llm_add_request_hash <- function(df) {
  stopifnot(is.data.frame(df))
  .llmr_add_request_hash(df)
}

#' Build Factorial Experiment Design
#'
#' Creates a tibble of experiments for factorial designs where you want to test
#' all combinations of configs, messages, and repetitions with automatic metadata.
#'
#' @param configs List of llm_config objects to test.
#' @param repetitions Integer. Number of repetitions per combination. Default is 1.
#' @param config_labels Character vector of labels for configs. If NULL, uses "provider_model".
#' @param user_prompts Character vector (or list) of user-turn prompts.
#' @param user_prompt_labels Optional labels for the user prompts.
#' @param system_prompts Optional character vector of system messages. These are
#'   fully crossed with the user prompts (every combination appears), like the
#'   other factors. Missing/NA values are ignored; those messages are user-only.
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

  # Create user-prompt labels if not provided
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

#' Expand an LLM Config Grid
#'
#' Creates a list of [llm_config] objects from a base configuration and sweeping
#' parameter vectors. Uses [expand.grid()] internally.
#'
#' @param base_config An [llm_config] object to use as the base.
#' @param ... Named vectors of parameter values to sweep (e.g., `model`, `temperature`).
#'   Parameters named `provider`, `model`, `api_key`, `embedding`, `troubleshooting`,
#'   or `no_change` are set as top-level config fields; all others are placed in
#'   `model_params`.
#' @return A list of [llm_config] objects.
#'
#' @examples
#' \dontrun{
#' base <- llm_config("openai", "gpt-4.1-nano")
#' cfgs <- expand_llm_config(base,
#'                           temperature = c(0, 0.5, 1),
#'                           model = c("gpt-4.1-nano", "gpt-4.1-mini"))
#' length(cfgs)
#' }
#'
#' @seealso [llm_config()], [llm_cross_design()], [call_llm_par()]
#' @export
expand_llm_config <- function(base_config, ...) {
  stopifnot(inherits(base_config, "llm_config"))
  dots <- list(...)
  if (length(dots) == 0) return(list(base_config))

  grid <- expand.grid(dots, stringsAsFactors = FALSE)
  top_level <- c("provider", "model", "api_key", "embedding", "troubleshooting", "no_change")
  out <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    cfg <- base_config
    mp <- cfg$model_params %||% list()
    for (nm in names(grid)) {
      if (nm %in% top_level) {
        cfg[[nm]] <- grid[[nm]][[i]]
      } else {
        mp[[nm]] <- grid[[nm]][[i]]
      }
    }
    cfg$model_params <- mp
    # The provider determines S3 dispatch in call_llm(); a swept provider must
    # be reflected in the class or the call would go to the wrong API.
    class(cfg) <- c("llm_config", cfg$provider)
    out[[i]] <- cfg
  }
  out
}

#' Cross a data frame with LLM configs
#'
#' Creates an experimental design tibble that crosses every row in `.data` with
#' every config in `configs`, evaluating glue prompt templates row-by-row.
#' The result has `config` and `messages` list-columns ready for [call_llm_par()].
#'
#' @param .data A data frame containing variables for the glue prompt.
#' @param configs A list of [llm_config] objects (or a single [llm_config]).
#' @param prompt A glue string for a single user turn.
#' @param .messages Optional named character vector of glue templates (roles as names).
#' @param .system_prompt Optional system prompt template (glue string).
#' @return A tibble with all original data columns plus `config` and `messages`
#'   list-columns.
#'
#' @examples
#' \dontrun{
#' cities <- data.frame(city = c("Cairo", "Lima"))
#' cfgs <- list(llm_config("openai", "gpt-4.1-nano"), llm_config("openai", "gpt-4.1-mini"))
#' design <- llm_cross_design(cities, cfgs, prompt = "What country is {city} in?")
#' results <- call_llm_par(design)
#' }
#'
#' @seealso [expand_llm_config()], [call_llm_par()], [build_factorial_experiments()]
#' @export
llm_cross_design <- function(.data, configs, prompt = NULL, .messages = NULL, .system_prompt = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data frame.")
  if (inherits(configs, "llm_config")) configs <- list(configs)

  n_data <- nrow(.data)
  n_conf <- length(configs)

  idx_data <- rep(seq_len(n_data), each = n_conf)
  idx_conf <- rep(seq_len(n_conf), times = n_data)

  out <- .data[idx_data, , drop = FALSE]
  out$config <- configs[idx_conf]

  msgs <- vector("list", nrow(out))
  for (i in seq_len(nrow(out))) {
    row_df <- out[i, setdiff(names(out), "config"), drop = FALSE]

    if (!is.null(.messages)) {
      roles <- names(.messages)
      if (is.null(roles)) roles <- rep("user", length(.messages))
      roles[is.na(roles) | roles == ""] <- "user"

      m <- vapply(unname(.messages),
                  function(s) as.character(glue::glue_data(row_df, s, .na = "")),
                  character(1))
      names(m) <- roles

      if (!is.null(.system_prompt) && !"system" %in% names(m)) {
        sys <- as.character(glue::glue_data(row_df, .system_prompt, .na = ""))
        m <- c(system = sys, m)
      }
      msgs[[i]] <- m
    } else if (!is.null(prompt)) {
      m <- as.character(glue::glue_data(row_df, prompt, .na = ""))
      if (!is.null(.system_prompt)) {
        sys <- as.character(glue::glue_data(row_df, .system_prompt, .na = ""))
        msgs[[i]] <- c(system = sys, user = m)
      } else {
        msgs[[i]] <- m
      }
    } else {
      stop("Must provide either prompt or .messages.")
    }
  }
  out$messages <- msgs
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out
}

#' Resume failed parallel LLM calls
#'
#' Finds rows where `success` is `FALSE` or `NA` in the output of [call_llm_par()],
#' re-runs them, and patches the results back into the original data frame.
#'
#' @param results Output from [call_llm_par()] (must contain `config`, `messages`,
#'   and `success` columns).
#' @param tries Number of retries per call. Default 3.
#' @param ... Passed to [call_llm_par()].
#' @return The patched data frame with re-run results filled in.
#'
#' @examples
#' \dontrun{
#' results <- call_llm_par(experiments)
#' results <- llm_par_resume(results, tries = 3)
#' }
#'
#' @seealso [call_llm_par()]
#' @export
llm_par_resume <- function(results, tries = 3, ...) {
  if (is.data.frame(results) &&
      any(grepl("^success\\.[0-9]+$", names(results)))) {
    stop("This result has a collision-renamed 'success' column (e.g. ",
         "'success.1'), which happens when the input frame already had a column ",
         "named 'success'. Rename that input column before calling ",
         "call_llm_par() so the diagnostic columns keep their canonical names.",
         call. = FALSE)
  }
  if (!is.data.frame(results) ||
      !all(c("config", "messages", "success") %in% names(results))) {
    stop("results must be the output of call_llm_par(), containing 'config', 'messages', and 'success'.")
  }

  # Element-wise: flag rows whose success is FALSE or NA. (isTRUE() collapses a
  # whole vector to one scalar, which would mark every row as failed.)
  failed_idx <- which(!(results$success %in% TRUE))
  if (length(failed_idx) == 0) {
    message("All runs successful. Nothing to resume.")
    return(results)
  }

  message(sprintf("Found %d failed runs. Retrying...", length(failed_idx)))

  failed_experiments <- results[failed_idx, c("config", "messages"), drop = FALSE]
  resumed <- call_llm_par(failed_experiments, tries = tries, simplify = FALSE, ...)

  for (col in names(resumed)) {
    if (col %in% names(results)) {
      results[[col]][failed_idx] <- resumed[[col]]
    }
  }

  # Refresh the structured-parsing diagnostics for the re-run rows so they
  # reflect the new responses rather than the failed ones.
  if (all(c("structured_ok", "structured_data") %in% names(results))) {
    sub <- results[failed_idx, "response_text", drop = FALSE]
    reparsed <- llm_parse_structured_col(sub, fields = character(0),
                                         structured_col = "response_text")
    results$structured_ok[failed_idx]   <- reparsed$structured_ok
    results$structured_data[failed_idx] <- reparsed$structured_data
    warning("Hoisted structured field columns are not re-extracted for resumed ",
            "rows; run llm_parse_structured_col() on the result if you need ",
            "them refreshed.", call. = FALSE)
  }
  if (all(c("tags_ok", "tags_data") %in% names(results))) {
    warning("Tag columns (tags_ok/tags_data and extracted tags) are not ",
            "re-parsed for resumed rows; run llm_parse_tags_col() on the ",
            "result to refresh them.", call. = FALSE)
  }

  if (!inherits(results, "llmr_experiment")) {
    class(results) <- unique(c("llmr_experiment", class(results)))
  }
  results
}

#' LLM-as-a-Judge Evaluation
#'
#' Evaluates outputs in a target column against a custom prompt using
#' [llm_mutate_tags()] for clean tag-based extraction. The target column value
#' is available in the prompt template as `{.target}`.
#'
#' @param .data Data frame of experiment results.
#' @param .target Bare column name containing the output to evaluate.
#' @param .config The judge [llm_config].
#' @param prompt Evaluation prompt template. Use `{.target}` to reference the
#'   target column value (other data columns are also available).
#' @param .tags Tags to extract from the judge response. Defaults to
#'   `c("reasoning", "score")`.
#' @param .output Name of the column that receives the judge's raw response.
#'   Default `"judge_res"`.
#' @param ... Passed to [llm_mutate_tags()].
#' @return `.data` with judge output columns appended.
#'
#' @examples
#' \dontrun{
#' results |>
#'   llm_judge(
#'     .target = response_text,
#'     .config = judge_cfg,
#'     prompt = "Rate this answer on a 1-5 scale:\n{.target}",
#'     .tags = c("reasoning", "score")
#'   )
#' }
#'
#' @seealso [llm_mutate_tags()], [llm_parse_tags()]
#' @export
llm_judge <- function(.data, .target, .config, prompt,
                      .tags = c("reasoning", "score"),
                      .output = "judge_res", ...) {
  target_sym <- rlang::ensym(.target)
  target_name <- rlang::as_name(target_sym)

  if (!target_name %in% names(.data)) stop("Target column not found in .data.")
  if (".target" %in% names(.data)) {
    stop("`.data` already contains a column named '.target', which llm_judge() ",
         "uses internally for the prompt template. Rename that column first.")
  }
  stopifnot(is.character(.output), length(.output) == 1L, nzchar(.output))

  eval_data <- .data
  eval_data[[".target"]] <- eval_data[[target_name]]

  out <- do.call(llm_mutate_tags, c(
    list(
      .data = eval_data,
      output = rlang::sym(.output),
      prompt = prompt,
      .config = .config,
      .tags = .tags
    ),
    list(...)
  ))

  out[[".target"]] <- NULL

  if (inherits(.data, "llmr_experiment") && !inherits(out, "llmr_experiment")) {
    class(out) <- unique(c("llmr_experiment", class(out)))
  }
  out
}

# Internal: resolve a diagnostic column that may have been collision-renamed
# (the engine appends ".1", ".2", ... when the input frame already had a column
# of that name; its own column is always the LAST matching one in column order).
.llmr_diag_col <- function(object, base) {
  cands <- grep(paste0("^", base, "(\\.[0-9]+)?$"), names(object), value = TRUE)
  if (!length(cands)) return(NA_character_)
  cands[length(cands)]
}

#' @export
summary.llmr_experiment <- function(object, ...) {
  sc <- .llmr_diag_col(object, "success")
  st <- .llmr_diag_col(object, "sent_tokens")
  rt <- .llmr_diag_col(object, "rec_tokens")
  du <- .llmr_diag_col(object, "duration")
  tt <- .llmr_diag_col(object, "total_tokens")
  fr <- .llmr_diag_col(object, "finish_reason")

  total <- nrow(object)
  success <- if (!is.na(sc)) sum(object[[sc]], na.rm = TRUE) else NA_integer_
  total_sent <- if (!is.na(st)) sum(object[[st]], na.rm = TRUE) else NA_integer_
  total_rec  <- if (!is.na(rt)) sum(object[[rt]], na.rm = TRUE) else NA_integer_

  cat("-- LLMR Experiment Summary --\n")
  cat(sprintf("Total Runs: %s (%.1f%% successful)\n",
              format(total, big.mark = ","),
              (success / max(1, total)) * 100))
  cat(sprintf("Total Tokens: %s sent | %s received\n\n",
              format(total_sent, big.mark = ","),
              format(total_rec, big.mark = ",")))

  if ("model" %in% names(object) && !anyNA(c(sc, du, tt, fr))) {
    cat("-- Performance By Model --\n")
    if (requireNamespace("dplyr", quietly = TRUE)) {
      agg <- object |>
        dplyr::group_by(model) |>
        dplyr::summarize(
          Success = sprintf("%s%%", round(mean(.data[[sc]], na.rm = TRUE) * 100, 1)),
          `Avg Latency` = sprintf("%.1fs", mean(.data[[du]], na.rm = TRUE)),
          `Avg Tokens` = format(round(mean(.data[[tt]], na.rm = TRUE)), big.mark = ","),
          Truncated = sprintf("%s%%", round(mean(.data[[fr]] == "length", na.rm = TRUE) * 100, 1)),
          .groups = "drop"
        )
      print(as.data.frame(agg), row.names = FALSE)
    } else {
      for (m in unique(object$model)) {
        sub <- object[object$model == m, ]
        cat(sprintf("%s: %.1f%% success | %.1fs latency\n",
                    m, mean(sub[[sc]], na.rm = TRUE) * 100,
                    mean(sub[[du]], na.rm = TRUE)))
      }
    }
  }
  invisible(object)
}
