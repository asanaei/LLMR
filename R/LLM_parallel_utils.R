# LLM_parallel_utils.R
# -------------------------------------------------------------------
# This file provides parallelized services for dispatching multiple LLM API calls
# concurrently. It leverages the 'future' package for OS-agnostic parallelization
# and uses call_llm_robust() as the default calling mechanism with configurable
# retry and delay settings.
#
# Key Features:
#   1. call_llm_sweep() - Parameter sweep mode: vary one parameter, fixed message
#   2. call_llm_broadcast() - Message broadcast mode: fixed config, multiple messages
#   3. call_llm_compare() - Model comparison mode: multiple configs, fixed message
#   4. call_llm_par() - Full flexibility mode: list of config-message pairs
#   5. call_llm_experiments() - Enhanced mode with automatic metadata tracking
#   6. build_factorial_experiments() - Helper for factorial experimental designs
#   7. setup_llm_parallel() / reset_llm_parallel() - Environment management
#   8. Automatic load balancing and error handling
#   9. Progress tracking and detailed logging
#   10. Memory-efficient batch processing (relative to sequential calls for large jobs)
#
# Function Categories:
#   • Basic Parallel Modes: call_llm_sweep(), call_llm_broadcast(), call_llm_compare()
#   • Advanced Modes: call_llm_par(), call_llm_experiments()
#   • Design Helpers: build_factorial_experiments()
#   • Environment: setup_llm_parallel(), reset_llm_parallel()
#
# Recommended Usage:
#   • Simple experiments: Use call_llm_sweep(), call_llm_broadcast(), or call_llm_compare()
#   • Complex experiments with metadata: Use call_llm_experiments() or build_factorial_experiments()
#   • Maximum flexibility: Use call_llm_par() for custom workflows
#
# Dependencies: future, future.apply, tibble, dplyr, progressr (optional)
# -------------------------------------------------------------------
#' Mode 1: Parameter Sweep - Vary One Parameter, Fixed Message
#'
#' Sweeps through different values of a single parameter while keeping the message constant.
#' Perfect for hyperparameter tuning, temperature experiments, etc.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param base_config Base llm_config object to modify.
#' @param param_name Character. Name of the parameter to vary (e.g., "temperature", "max_tokens").
#' @param param_values Vector. Values to test for the parameter.
#' @param messages List of message objects (same for all calls).
#' @param tries Integer. Number of retries for each call. Default is 10.
#' @param wait_seconds Numeric. Initial wait time (seconds) before retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 2.
#' @param verbose Logical. If TRUE, prints progress and debug information.
#' @param json Logical. If TRUE, requests raw JSON responses from the API (note: final tibble's `response_text` will be extracted text).
#' @param memoize Logical. If TRUE, enables caching for identical requests via `call_llm_robust`.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress bar.
#'
#' @return A tibble with columns: param_name, param_value, provider, model, response_text, success, error_message, plus all model parameters as additional columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Temperature sweep
#'   config <- llm_config(provider = "openai", model = "gpt-4o-mini",
#'                        api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#'   messages <- list(list(role = "user", content = "What is 15 * 23?"))
#'   temperatures <- c(0, 0.3, 0.7, 1.0, 1.5)
#'
#'   # set up the parallel enviornment
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'
#'   results <- call_llm_sweep(config, "temperature", temperatures, messages)
#'
#'   # Reset to sequential
#'   reset_llm_parallel(verbose = TRUE)
#' }
call_llm_sweep <- function(base_config,
                           param_name,
                           param_values,
                           messages,
                           tries = 10,
                           wait_seconds = 2,
                           backoff_factor = 2,
                           verbose = FALSE,
                           json = FALSE,
                           memoize = FALSE,
                           max_workers = NULL,
                           progress = FALSE) {

  # Input validation
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("The 'future' package is required for parallel processing. Please install it with: install.packages('future')")
  }

  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("The 'future.apply' package is required for parallel processing. Please install it with: install.packages('future.apply')")
  }

  if (progress && !requireNamespace("progressr", quietly = TRUE)) {
    warning("The 'progressr' package is not available. Progress tracking will be disabled.")
    progress <- FALSE
  }

  if (length(param_values) == 0) {
    warning("No parameter values provided. Returning empty tibble.")
    return(tibble::tibble(
      param_name = character(0),
      param_value = character(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  # Auto-detect workers if not specified
  if (is.null(max_workers)) {
    if (requireNamespace("future", quietly = TRUE)) {
      max_workers <- min(future::availableCores() - 1, length(param_values))
      max_workers <- max(1, max_workers) # Ensure at least 1 worker
    } else {
      max_workers <- 1 # Fallback if future is not available (though checked above)
    }
  }

  # Set up workers
  current_plan <- future::plan()
  if (verbose) {
    message(sprintf("Setting up parallel execution with %d workers using plan: %s", max_workers, class(current_plan)[1]))
  }
  on.exit(future::plan(current_plan), add = TRUE) # Restore previous plan on exit
  if (!inherits(current_plan, "FutureStrategy") || inherits(current_plan, "sequential")) { # Only change plan if not already parallel or if sequential
    future::plan(future::multisession, workers = max_workers)
  }


  if (verbose) {
    message(sprintf("Parameter sweep: %s with %d values", param_name, length(param_values)))
  }

  # Define the worker function
  sweep_worker <- function(i_val) { # Renamed i to i_val to avoid conflict if i is used in parent env
    # Note: base_config, param_name, param_values, messages, tries, wait_seconds,
    # backoff_factor, verbose (for worker message), json, memoize are accessed
    # from the calling environment of this worker function (lexical scoping).
    # future.apply handles making these available.

    current_param_value <- param_values[i_val]

    # Create modified config for this specific call
    modified_config <- base_config
    # R's list copy-on-write semantics apply. $model_params is a list.
    # This modification is local to this worker's copy of modified_config.
    if (is.null(modified_config$model_params)) modified_config$model_params <- list()
    modified_config$model_params[[param_name]] <- current_param_value

    if (verbose) { # This verbose is the main function's verbose, controls messages from master process
      # If you want worker-specific verbosity, pass it explicitly or manage differently
      # message(sprintf("Worker processing %s = %s (%d/%d)", param_name, current_param_value, i_val, length(param_values)))
      # The above message might be too noisy if many workers. Progressr handles overall progress.
    }

    tryCatch({
      result_content <- call_llm_robust(
        config = modified_config,
        messages = messages,
        tries = tries,
        wait_seconds = wait_seconds,
        backoff_factor = backoff_factor,
        verbose = FALSE,  # Individual call verbosity off in parallel, master verbose controls overview
        json = json,
        memoize = memoize
      )

      list(
        param_name = param_name,
        param_value = current_param_value,
        config_info = list(provider = modified_config$provider, model = modified_config$model),
        config_params = modified_config$model_params,
        response = result_content, # This is character, possibly with attributes if json=TRUE
        success = TRUE,
        error = NA_character_
      )
    }, error = function(e) {
      # warning(sprintf("Call with %s = %s failed: %s", param_name, current_param_value, conditionMessage(e)))
      # Warnings from many workers can be overwhelming. Error is captured in output.
      list(
        param_name = param_name,
        param_value = current_param_value,
        config_info = list(provider = modified_config$provider, model = modified_config$model),
        config_params = modified_config$model_params,
        response = NA_character_,
        success = FALSE,
        error = conditionMessage(e)
      )
    })
  }

  # Execute in parallel
  if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = length(param_values))
      results <- future.apply::future_lapply(seq_along(param_values), function(k) {
        res <- sweep_worker(k)
        p()
        res
      }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE) # Explicitly state package, keep future.globals=TRUE from original
    })
  } else {
    results <- future.apply::future_lapply(seq_along(param_values), function(k) {
      sweep_worker(k)
    }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE)
  }

  # Convert results to tibble
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }

  all_model_param_names <- unique(unlist(lapply(results, function(r) names(r$config_params))))

  result_df_list <- lapply(results, function(res) {
    core_data <- tibble::tibble(
      param_name = res$param_name,
      param_value = as.character(res$param_value), # Ensure character for heterogeneous values
      provider = res$config_info$provider,
      model = res$config_info$model,
      response_text = if (res$success) as.character(res$response) else NA_character_, # as.character strips attributes
      success = res$success,
      error_message = if (!res$success) res$error else NA_character_
    )

    # Add model parameters
    params_data <- stats::setNames(as.list(rep(NA, length(all_model_param_names))), all_model_param_names)
    if (length(res$config_params) > 0) {
      for(p_name in names(res$config_params)) {
        if(p_name %in% all_model_param_names) {
          params_data[[p_name]] <- res$config_params[[p_name]]
        }
      }
    }
    # Ensure all parameter columns are present, even if all NA for this row
    # Convert NULL to NA for tibble compatibility
    params_data <- lapply(params_data, function(x) if(is.null(x)) NA else x)

    dplyr::bind_cols(core_data, tibble::as_tibble(params_data))
  })

  result_df <- dplyr::bind_rows(result_df_list)

  if (verbose) {
    successful_calls <- sum(result_df$success, na.rm = TRUE)
    message(sprintf("Parameter sweep completed: %d/%d calls successful", successful_calls, nrow(result_df)))
  }

  return(result_df)
}

#' Mode 2: Message Broadcast - Fixed Config, Multiple Messages
#'
#' Broadcasts different messages using the same configuration in parallel.
#' Perfect for batch processing different prompts with consistent settings.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param config Single llm_config object to use for all calls.
#' @param messages_list A list of message lists, each for one API call.
#' @param tries Integer. Number of retries for each call. Default is 10.
#' @param wait_seconds Numeric. Initial wait time (seconds) before retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 2.
#' @param verbose Logical. If TRUE, prints progress and debug information.
#' @param json Logical. If TRUE, requests raw JSON responses from the API.
#' @param memoize Logical. If TRUE, enables caching for identical requests.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress bar.
#'
#' @return A tibble with columns: message_index, provider, model, response_text, success, error_message, plus all model parameters as additional columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Broadcast different questions
#'   config <- llm_config(provider = "openai", model = "gpt-4o-mini",
#'                        api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#'   messages_list <- list(
#'     list(list(role = "user", content = "What is 2+2?")),
#'     list(list(role = "user", content = "What is 3*5?")),
#'     list(list(role = "user", content = "What is 10/2?"))
#'   )
#'
#'   # setup paralle Environment
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'
#'   results <- call_llm_broadcast(config, messages_list)
#'
#'   # Reset to sequential
#'   reset_llm_parallel(verbose = TRUE)
#' }
call_llm_broadcast <- function(config,
                               messages_list,
                               tries = 10,
                               wait_seconds = 2,
                               backoff_factor = 2,
                               verbose = FALSE,
                               json = FALSE,
                               memoize = FALSE,
                               max_workers = NULL,
                               progress = FALSE) {

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

  if (length(messages_list) == 0) {
    warning("No messages provided. Returning empty tibble.")
    return(tibble::tibble(
      message_index = integer(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  if (is.null(max_workers)) {
    if (requireNamespace("future", quietly = TRUE)) {
      max_workers <- min(future::availableCores() - 1, length(messages_list))
      max_workers <- max(1, max_workers)
    } else {
      max_workers <- 1
    }
  }

  current_plan <- future::plan()
  if (verbose) {
    message(sprintf("Setting up parallel execution with %d workers using plan: %s", max_workers, class(current_plan)[1]))
  }
  on.exit(future::plan(current_plan), add = TRUE)
  if (!inherits(current_plan, "FutureStrategy") || inherits(current_plan, "sequential")) {
    future::plan(future::multisession, workers = max_workers)
  }

  if (verbose) {
    message(sprintf("Broadcasting %d different messages", length(messages_list)))
  }

  broadcast_worker <- function(i_val) {
    current_messages <- messages_list[[i_val]]

    tryCatch({
      result_content <- call_llm_robust(
        config = config,
        messages = current_messages,
        tries = tries,
        wait_seconds = wait_seconds,
        backoff_factor = backoff_factor,
        verbose = FALSE,
        json = json,
        memoize = memoize
      )
      list(
        message_index = i_val,
        config_info = list(provider = config$provider, model = config$model),
        config_params = config$model_params,
        response = result_content,
        success = TRUE,
        error = NA_character_
      )
    }, error = function(e) {
      list(
        message_index = i_val,
        config_info = list(provider = config$provider, model = config$model),
        config_params = config$model_params,
        response = NA_character_,
        success = FALSE,
        error = conditionMessage(e)
      )
    })
  }

  if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = length(messages_list))
      results <- future.apply::future_lapply(seq_along(messages_list), function(k) {
        res <- broadcast_worker(k)
        p()
        res
      }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE)
    })
  } else {
    results <- future.apply::future_lapply(seq_along(messages_list), function(k) {
      broadcast_worker(k)
    }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE)
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) { # For bind_rows, bind_cols
    stop("The 'dplyr' package is required for result formatting. Please install it with: install.packages('dplyr')")
  }

  all_model_param_names <- unique(unlist(lapply(results, function(r) names(r$config_params))))

  result_df_list <- lapply(results, function(res) {
    core_data <- tibble::tibble(
      message_index = res$message_index,
      provider = res$config_info$provider,
      model = res$config_info$model,
      response_text = if (res$success) as.character(res$response) else NA_character_,
      success = res$success,
      error_message = if (!res$success) res$error else NA_character_
    )
    params_data <- stats::setNames(as.list(rep(NA, length(all_model_param_names))), all_model_param_names)
    if (length(res$config_params) > 0) {
      for(p_name in names(res$config_params)) {
        if(p_name %in% all_model_param_names) {
          params_data[[p_name]] <- res$config_params[[p_name]]
        }
      }
    }
    params_data <- lapply(params_data, function(x) if(is.null(x)) NA else x)
    dplyr::bind_cols(core_data, tibble::as_tibble(params_data))
  })

  result_df <- dplyr::bind_rows(result_df_list)

  if (verbose) {
    successful_calls <- sum(result_df$success, na.rm = TRUE)
    message(sprintf("Message broadcast completed: %d/%d calls successful", successful_calls, nrow(result_df)))
  }

  return(result_df)
}

#' Mode 3: Model Comparison - Multiple Configs, Fixed Message
#'
#' Compares different configurations (models, providers, settings) using the same message.
#' Perfect for benchmarking across different models or providers.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param configs_list A list of llm_config objects to compare.
#' @param messages List of message objects (same for all configs).
#' @param tries Integer. Number of retries for each call. Default is 10.
#' @param wait_seconds Numeric. Initial wait time (seconds) before retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 2.
#' @param verbose Logical. If TRUE, prints processing information.
#' @param json Logical. If TRUE, returns raw JSON responses.
#' @param memoize Logical. If TRUE, enables caching for identical requests.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress tracking.
#'
#' @return A tibble with columns: config_index, provider, model, response_text, success, error_message, plus all model parameters as additional columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Compare different models
#'   config1 <- llm_config(provider = "openai", model = "gpt-4o-mini",
#'                         api_key = Sys.getenv("OPENAI_API_KEY"))
#'   config2 <- llm_config(provider = "openai", model = "gpt-3.5-turbo",
#'                         api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#'   configs_list <- list(config1, config2)
#'   messages <- list(list(role = "user", content = "Explain quantum computing"))
#'
#'   # setup paralle Environment
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'
#'   results <- call_llm_compare(configs_list, messages)
#'
#'   # Reset to sequential
#'   reset_llm_parallel(verbose = TRUE)
#' }
call_llm_compare <- function(configs_list,
                             messages,
                             tries = 10,
                             wait_seconds = 2,
                             backoff_factor = 2,
                             verbose = FALSE,
                             json = FALSE,
                             memoize = FALSE,
                             max_workers = NULL,
                             progress = FALSE) {

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

  if (length(configs_list) == 0) {
    warning("No configs provided. Returning empty tibble.")
    return(tibble::tibble(
      config_index = integer(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  if (is.null(max_workers)) {
    if (requireNamespace("future", quietly = TRUE)) {
      max_workers <- min(future::availableCores() - 1, length(configs_list))
      max_workers <- max(1, max_workers)
    } else {
      max_workers <- 1
    }
  }

  current_plan <- future::plan()
  if (verbose) {
    message(sprintf("Setting up parallel execution with %d workers using plan: %s", max_workers, class(current_plan)[1]))
  }
  on.exit(future::plan(current_plan), add = TRUE)
  if (!inherits(current_plan, "FutureStrategy") || inherits(current_plan, "sequential")) {
    future::plan(future::multisession, workers = max_workers)
  }

  if (verbose) {
    message(sprintf("Comparing %d different configurations", length(configs_list)))
  }

  compare_worker <- function(i_val) {
    current_config <- configs_list[[i_val]]

    tryCatch({
      result_content <- call_llm_robust(
        config = current_config,
        messages = messages,
        tries = tries,
        wait_seconds = wait_seconds,
        backoff_factor = backoff_factor,
        verbose = FALSE,
        json = json,
        memoize = memoize
      )
      list(
        config_index = i_val,
        config_info = list(provider = current_config$provider, model = current_config$model),
        config_params = current_config$model_params,
        response = result_content,
        success = TRUE,
        error = NA_character_
      )
    }, error = function(e) {
      list(
        config_index = i_val,
        config_info = list(provider = current_config$provider, model = current_config$model),
        config_params = current_config$model_params,
        response = NA_character_,
        success = FALSE,
        error = conditionMessage(e)
      )
    })
  }

  if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = length(configs_list))
      results <- future.apply::future_lapply(seq_along(configs_list), function(k) {
        res <- compare_worker(k)
        p()
        res
      }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE)
    })
  } else {
    results <- future.apply::future_lapply(seq_along(configs_list), function(k) {
      compare_worker(k)
    }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE)
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required for result formatting. Please install it with: install.packages('dplyr')")
  }

  all_model_param_names <- unique(unlist(lapply(results, function(r) names(r$config_params))))

  result_df_list <- lapply(results, function(res) {
    core_data <- tibble::tibble(
      config_index = res$config_index,
      provider = res$config_info$provider,
      model = res$config_info$model,
      response_text = if (res$success) as.character(res$response) else NA_character_,
      success = res$success,
      error_message = if (!res$success) res$error else NA_character_
    )
    params_data <- stats::setNames(as.list(rep(NA, length(all_model_param_names))), all_model_param_names)
    if (length(res$config_params) > 0) {
      for(p_name in names(res$config_params)) {
        if(p_name %in% all_model_param_names) {
          params_data[[p_name]] <- res$config_params[[p_name]]
        }
      }
    }
    params_data <- lapply(params_data, function(x) if(is.null(x)) NA else x)
    dplyr::bind_cols(core_data, tibble::as_tibble(params_data))
  })

  result_df <- dplyr::bind_rows(result_df_list)

  if (verbose) {
    successful_calls <- sum(result_df$success, na.rm = TRUE)
    message(sprintf("Model comparison completed: %d/%d configs successful", successful_calls, nrow(result_df)))
  }

  return(result_df)
}

#' Mode 4: Parallel Processing - List of Config-Message Pairs
#'
#' Processes a list where each element contains both a config and message pair.
#' Maximum flexibility for complex workflows with different configs and messages.
#' This function requires setting up the parallel environment using `setup_llm_parallel`.
#'
#' @param config_message_pairs A list where each element is a list with 'config' and 'messages' elements.
#' @param tries Integer. Number of retries for each call. Default is 10.
#' @param wait_seconds Numeric. Initial wait time (seconds) before retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 2.
#' @param verbose Logical. If TRUE, prints progress and debug information.
#' @param json Logical. If TRUE, returns raw JSON responses.
#' @param memoize Logical. If TRUE, enables caching for identical requests.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress bar.
#'
#' @return A tibble with columns: pair_index, provider, model, response_text, success, error_message, plus all model parameters as additional columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Full flexibility with different configs and messages
#'   config1 <- llm_config(provider = "openai", model = "gpt-4o-mini",
#'                         api_key = Sys.getenv("OPENAI_API_KEY"))
#'   config2 <- llm_config(provider = "openai", model = "gpt-3.5-turbo",
#'                         api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#'   pairs <- list(
#'     list(config = config1, messages = list(list(role = "user", content = "What is AI?"))),
#'     list(config = config2, messages = list(list(role = "user", content = "Explain ML")))
#'   )
#'
#'   # setup paralle Environment
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'
#'   results <- call_llm_par(pairs)
#'
#'   # Reset to sequential
#'   reset_llm_parallel(verbose = TRUE)
#' }
call_llm_par <- function(config_message_pairs,
                         tries = 10,
                         wait_seconds = 2,
                         backoff_factor = 2,
                         verbose = FALSE,
                         json = FALSE,
                         memoize = FALSE,
                         max_workers = NULL,
                         progress = FALSE) {

  if (!requireNamespace("future", quietly = TRUE)) {
    stop("The 'future' package is required for parallel processing. Please install it with: install.packages('future')")
  }
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("The 'future.apply' package is required for parallel processing. Please install it with: install.packages('future.apply')")
  }
  if (progress && !requireNamespace("progressr", quietly = TRUE)) {
    warning("The 'progressr' package is not available. Progress tracking will be disabled.")
    progress <- FALSE
  }

  if (length(config_message_pairs) == 0) {
    warning("No config-message pairs provided. Returning empty tibble.")
    return(tibble::tibble(
      pair_index = integer(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  # Validate input structure
  for (i in seq_along(config_message_pairs)) {
    pair <- config_message_pairs[[i]]
    if (!is.list(pair) || !("config" %in% names(pair)) || !("messages" %in% names(pair))) {
      stop(sprintf("Element %d of config_message_pairs must be a list with 'config' and 'messages' named elements.", i))
    }
    if (!inherits(pair$config, "llm_config")) {
      stop(sprintf("Element %d 'config' is not an llm_config object.", i))
    }
  }

  if (is.null(max_workers)) {
    if (requireNamespace("future", quietly = TRUE)) {
      max_workers <- min(future::availableCores() - 1, length(config_message_pairs))
      max_workers <- max(1, max_workers)
    } else {
      max_workers <- 1
    }
  }

  current_plan <- future::plan()
  if (verbose) {
    message(sprintf("Setting up parallel execution with %d workers using plan: %s", max_workers, class(current_plan)[1]))
  }
  on.exit(future::plan(current_plan), add = TRUE)
  if (!inherits(current_plan, "FutureStrategy") || inherits(current_plan, "sequential")) {
    future::plan(future::multisession, workers = max_workers)
  }

  if (verbose) {
    message(sprintf("Processing %d config-message pairs", length(config_message_pairs)))
  }

  par_worker <- function(i_val) {
    pair <- config_message_pairs[[i_val]]
    current_config <- pair$config
    current_messages <- pair$messages

    tryCatch({
      result_content <- call_llm_robust(
        config = current_config,
        messages = current_messages,
        tries = tries,
        wait_seconds = wait_seconds,
        backoff_factor = backoff_factor,
        verbose = FALSE,
        json = json,
        memoize = memoize
      )
      list(
        pair_index = i_val,
        config_info = list(provider = current_config$provider, model = current_config$model),
        config_params = current_config$model_params,
        response = result_content,
        success = TRUE,
        error = NA_character_
      )
    }, error = function(e) {
      list(
        pair_index = i_val,
        config_info = list(provider = current_config$provider, model = current_config$model),
        config_params = current_config$model_params,
        response = NA_character_,
        success = FALSE,
        error = conditionMessage(e)
      )
    })
  }

  if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = length(config_message_pairs))
      results <- future.apply::future_lapply(seq_along(config_message_pairs), function(k) {
        res <- par_worker(k)
        p()
        res
      }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE)
    })
  } else {
    results <- future.apply::future_lapply(seq_along(config_message_pairs), function(k) {
      par_worker(k)
    }, future.seed = TRUE, future.packages = "LLMR", future.globals = TRUE)
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required for result formatting. Please install it with: install.packages('dplyr')")
  }

  all_model_param_names <- unique(unlist(lapply(results, function(r) names(r$config_params))))

  result_df_list <- lapply(results, function(res) {
    core_data <- tibble::tibble(
      pair_index = res$pair_index,
      provider = res$config_info$provider,
      model = res$config_info$model,
      response_text = if (res$success) as.character(res$response) else NA_character_,
      success = res$success,
      error_message = if (!res$success) res$error else NA_character_
    )
    params_data <- stats::setNames(as.list(rep(NA, length(all_model_param_names))), all_model_param_names)
    if (length(res$config_params) > 0) {
      for(p_name in names(res$config_params)) {
        if(p_name %in% all_model_param_names) {
          params_data[[p_name]] <- res$config_params[[p_name]]
        }
      }
    }
    params_data <- lapply(params_data, function(x) if(is.null(x)) NA else x) # Convert NULL to NA
    dplyr::bind_cols(core_data, tibble::as_tibble(params_data))
  })

  result_df <- dplyr::bind_rows(result_df_list)

  if (verbose) {
    successful_calls <- sum(result_df$success, na.rm = TRUE)
    message(sprintf("Parallel processing completed: %d/%d pairs successful", successful_calls, nrow(result_df)))
  }

  return(result_df)
}


#' Call LLM API with Experimental Metadata
#'
#' An enhanced version of call_llm_par() that allows attaching metadata to each experiment.
#' This eliminates the need to manually track experiment indices and join metadata afterward.
#'
#' @param experiments A list where each element represents one experiment. Each element
#'   must contain 'config' and 'messages', and can contain additional named elements
#'   that will be included as metadata columns in the results.
#' @param tries Integer. Number of retries for each call. Default is 10.
#' @param wait_seconds Numeric. Initial wait time (seconds) before retry. Default is 2.
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 2.
#' @param verbose Logical. If TRUE, prints progress and debug information.
#' @param json Logical. If TRUE, returns raw JSON responses.
#' @param memoize Logical. If TRUE, enables caching for identical requests.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress bar.
#'
#' @return A tibble with API results plus any metadata columns from the experiments.
#'   The 'pair_index' column shows the position in the input list for debugging.
#'
#' @details
#' This function accepts experiments in two formats:
#'
#' \strong{Simple format} (backward compatible with call_llm_par):
#' \preformatted{
#' list(config = my_config, messages = my_messages)
#' }
#'
#' \strong{Enhanced format with metadata:}
#' \preformatted{
#' list(config = my_config, messages = my_messages,
#'      condition = "treatment", repetition = 1, model_type = "large")
#' }
#'
#' Any named elements beyond 'config' and 'messages' are treated as metadata
#' and included as columns in the results tibble. This eliminates the need to
#' manually track indices and join metadata afterward.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Enhanced usage with metadata
#'   experiments <- list(
#'     list(config = config1, messages = messages_control,
#'          condition = "control", repetition = 1, model_size = "small"),
#'     list(config = config1, messages = messages_treatment,
#'          condition = "treatment", repetition = 1, model_size = "small"),
#'     list(config = config2, messages = messages_control,
#'          condition = "control", repetition = 1, model_size = "large")
#'   )
#'
#'   setup_llm_parallel(workers = 4)
#'   results <- call_llm_experiments(experiments, progress = TRUE)
#'   reset_llm_parallel()
#'
#'   # Results include metadata columns automatically - no manual joining needed
#'   print(results)
#'
#'   # Direct analysis with metadata
#'   results %>% group_by(condition, model_size) %>%
#'     summarise(mean_rating = mean(as.numeric(response_text), na.rm = TRUE))
#' }
call_llm_experiments <- function(experiments,
                                 tries = 10,
                                 wait_seconds = 2,
                                 backoff_factor = 2,
                                 verbose = FALSE,
                                 json = FALSE,
                                 memoize = FALSE,
                                 max_workers = NULL,
                                 progress = FALSE) {

  if (length(experiments) == 0) {
    warning("No experiments provided. Returning empty tibble.")
    return(tibble::tibble(
      pair_index = integer(0),
      provider = character(0),
      model = character(0),
      response_text = character(0),
      success = logical(0),
      error_message = character(0)
    ))
  }

  # Validate input and extract config-message pairs and metadata
  config_message_pairs <- vector("list", length(experiments))
  metadata_list <- vector("list", length(experiments))

  for (i in seq_along(experiments)) {
    exp <- experiments[[i]]

    # Validate required elements
    if (!is.list(exp) || !("config" %in% names(exp)) || !("messages" %in% names(exp))) {
      stop(sprintf("Element %d of experiments must be a list with 'config' and 'messages' named elements.", i))
    }
    if (!inherits(exp$config, "llm_config")) {
      stop(sprintf("Element %d 'config' is not an llm_config object.", i))
    }

    # Extract config and messages for API call
    config_message_pairs[[i]] <- list(config = exp$config, messages = exp$messages)

    # Extract any additional elements as metadata (preserve original order)
    other_elements <- exp[!names(exp) %in% c("config", "messages")]
    metadata_list[[i]] <- c(list(experiment_index = i), other_elements)
  }

  if (verbose) {
    n_with_metadata <- sum(lengths(metadata_list) > 1)  # More than just experiment_index
    message(sprintf("Processing %d experiments (%d with metadata)", length(experiments), n_with_metadata))
  }

  # Call the existing parallel function
  api_results <- call_llm_par(
    config_message_pairs = config_message_pairs,
    tries = tries,
    wait_seconds = wait_seconds,
    backoff_factor = backoff_factor,
    verbose = verbose,
    json = json,
    memoize = memoize,
    max_workers = max_workers,
    progress = progress
  )

  # Merge with metadata if any exists beyond just the index
  has_metadata <- length(metadata_list) > 0 && any(lengths(metadata_list) > 1)

  if (has_metadata) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("The 'dplyr' package is required. Please install it with: install.packages('dplyr')")
    }

    # Convert metadata list to dataframe
    metadata_df <- dplyr::bind_rows(metadata_list)

    # Join with API results
    results <- dplyr::left_join(api_results, metadata_df, by = c("pair_index" = "experiment_index"))

    # Reorder columns: metadata first, then API columns, keeping pair_index for debugging
    metadata_cols <- setdiff(names(metadata_df), "experiment_index")
    api_cols <- setdiff(names(api_results), "pair_index")
    results <- results[, c("pair_index", metadata_cols, api_cols)]

  } else {
    results <- api_results
  }

  if (verbose) {
    successful_calls <- sum(results$success, na.rm = TRUE)
    message(sprintf("Experiment processing completed: %d/%d experiments successful", successful_calls, nrow(results)))
  }

  return(results)
}


#' Build Factorial Experiment Design
#'
#' Creates a list of experiments for factorial designs where you want to test
#' all combinations of configs, messages, and repetitions with automatic metadata.
#'
#' @param configs List of llm_config objects to test
#' @param messages_list List of message lists to test (each element is a message list for one condition)
#' @param repetitions Integer. Number of repetitions per combination. Default is 1.
#' @param config_labels Character vector of labels for configs. If NULL, uses config info.
#' @param message_labels Character vector of labels for message sets. If NULL, uses indices.
#'
#' @return A list of experiments suitable for call_llm_experiments(), with metadata
#'   columns for config_label, message_label, and repetition automatically attached.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Factorial design: 3 configs × 2 message conditions × 10 reps = 60 experiments
#'   configs <- list(gpt4_config, claude_config, llama_config)
#'   messages_list <- list(control_messages, treatment_messages)
#'
#'   experiments <- build_factorial_experiments(
#'     configs = configs,
#'     messages_list = messages_list,
#'     repetitions = 10,
#'     config_labels = c("gpt4", "claude", "llama"),
#'     message_labels = c("control", "treatment")
#'   )
#'
#'   # Metadata automatically included: config_label, message_label, repetition
#'   results <- call_llm_experiments(experiments, progress = TRUE)
#' }
build_factorial_experiments <- function(configs,
                                        messages_list,
                                        repetitions = 1,
                                        config_labels = NULL,
                                        message_labels = NULL) {

  # Validate inputs
  if (length(configs) == 0 || length(messages_list) == 0) {
    stop("Both configs and messages_list must have at least one element")
  }

  # Create config labels if not provided
  if (is.null(config_labels)) {
    config_labels <- sapply(configs, function(cfg) {
      paste(cfg$provider, cfg$model, sep = "_")
    })
  } else if (length(config_labels) != length(configs)) {
    stop("config_labels must have the same length as configs")
  }

  # Create message labels if not provided
  if (is.null(message_labels)) {
    message_labels <- paste0("messages_", seq_along(messages_list))
  } else if (length(message_labels) != length(messages_list)) {
    stop("message_labels must have the same length as messages_list")
  }

  # Build all combinations
  experiments <- list()

  for (i in seq_along(configs)) {
    for (j in seq_along(messages_list)) {
      for (rep in seq_len(repetitions)) {
        experiments <- append(experiments, list(list(
          config = configs[[i]],
          messages = messages_list[[j]],
          config_label = config_labels[i],
          message_label = message_labels[j],
          repetition = rep
        )))
      }
    }
  }

  message(sprintf("Built %d experiments: %d configs × %d message sets × %d repetitions",
                  length(experiments), length(configs), length(messages_list), repetitions))

  return(experiments)
}



#' Setup Parallel Environment for LLM Processing
#'
#' Convenience function to set up the future plan for optimal LLM parallel processing.
#' Automatically detects system capabilities and sets appropriate defaults.
#'
#' @param strategy Character. The future strategy to use. Options: "multisession", "multicore", "sequential".
#'                If NULL (default), automatically chooses "multisession".
#' @param workers Integer. Number of workers to use. If NULL, auto-detects optimal number (availableCores - 1, capped at 8).
#' @param verbose Logical. If TRUE, prints setup information.
#'
#' @return Invisibly returns the previous future plan.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Automatic setup
#'   old_plan <- setup_llm_parallel()
#'
#'   # Manual setup with specific workers
#'   setup_llm_parallel(workers = 4, verbose = TRUE)
#'
#'   # Force sequential processing for debugging
#'   setup_llm_parallel(strategy = "sequential")
#'
#'   # Restore old plan if needed
#'   future::plan(old_plan)
#' }
setup_llm_parallel <- function(strategy = NULL, workers = NULL, verbose = FALSE) {

  if (!requireNamespace("future", quietly = TRUE)) {
    stop("The 'future' package is required. Please install it with: install.packages('future')")
  }

  current_plan <- future::plan() # Store current plan to return

  if (is.null(strategy)) {
    strategy <- "multisession" # Default to multisession for broad compatibility
  }

  if (is.null(workers)) {
    available_cores <- future::availableCores()
    workers <- max(1, available_cores - 1) # Leave one core free
    workers <- min(workers, 8) # Cap at a reasonable maximum for API calls
  } else {
    workers <- max(1, as.integer(workers)) # Ensure positive integer
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
    message(sprintf("Parallel environment set to: %s with %d workers.", class(future::plan())[1], future::nbrOfWorkers()))
  }

  invisible(current_plan)
}

#' Reset Parallel Environment
#'
#' Resets the future plan to sequential processing.
#'
#' @param verbose Logical. If TRUE, prints reset information.
#'
#' @return Invisibly returns the future plan that was in place before resetting to sequential (often the default sequential plan).
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

  previous_plan <- future::plan(future::sequential) # Set to sequential and get the one before that

  if (verbose) {
    message("Parallel environment reset complete. Previous plan was: ", class(previous_plan)[1])
  }

  invisible(previous_plan)
}
