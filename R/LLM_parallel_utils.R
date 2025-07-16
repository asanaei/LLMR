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
  if (is.null(sys)) user else c(system = sys, user = user)
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
    param_cols_list <- lapply(all_model_param_names, function(p_name) {
      sapply(results_df[[config_col]], function(cfg) {
        val <- cfg$model_params[[p_name]]
        if (is.null(val)) NA else val
      })
    })
    names(param_cols_list) <- all_model_param_names
    #params_df <- tibble::as_tibble(param_cols_list)
    #results_df <- dplyr::bind_cols(results_df, params_df)

    params_df <- tibble::as_tibble(param_cols_list)
    # ------------------------------------------------------------------
    # avoid duplicate names coming from the input tibble
    # ------------------------------------------------------------------
    dup_params <- intersect(names(params_df), names(results_df))
    if (length(dup_params) > 0) {
      # If both columns exist but differ, keep the one already in results_df
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
#'   config <- llm_config(provider = "openai", model = "gpt-4.1-nano",
#'                        api_key = Sys.getenv("OPENAI_API_KEY"))
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
#'   config <- llm_config(provider = "openai", model = "gpt-4.1-nano",
#'                        api_key = Sys.getenv("OPENAI_API_KEY"))
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
#'   config1 <- llm_config(provider = "openai", model = "gpt-4o-mini",
#'                         api_key = Sys.getenv("OPENAI_API_KEY"))
#'   config2 <- llm_config(provider = "openai", model = "gpt-4.1-nano",
#'                         api_key = Sys.getenv("OPENAI_API_KEY"))
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
  results_raw <- call_llm_par(experiments, ...)
  results_final <- results_raw
  results_final$config <- NULL
  results_final
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
#' @param backoff_factor Numeric. Multiplier for wait time after each failure. Default is 2.
#' @param verbose Logical. If TRUE, prints progress and debug information.
#' @param memoize Logical. If TRUE, enables caching for identical requests.
#' @param max_workers Integer. Maximum number of parallel workers. If NULL, auto-detects.
#' @param progress Logical. If TRUE, shows progress bar.
#' @param json_output Deprecated. Raw JSON string is always included as raw_response_json.
#'                  This parameter is kept for backward compatibility but has no effect.
#'
#' @return A tibble containing all original columns from experiments (metadata, config, messages),
#'   plus new columns: response_text, raw_response_json (the raw JSON string from the API),
#'   success, error_message, duration (in seconds).
#'

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
#' cfg1 <- llm_config("openai", "gpt-4.1-nano", Sys.getenv("OPENAI_API_KEY"))
#' cfg2 <- llm_config("groq", "llama-3.3-70b-versatile", Sys.getenv("GROQ_API_KEY"))
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
    message("Note: The 'json_output' parameter in call_llm_par is deprecated. Raw JSON string is always included as 'raw_response_json'.")
  }

  # Package checks
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
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required. Please install it with: install.packages('tibble')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required. Please install it with: install.packages('dplyr')")
  }

  # Input validation
  if (!is.data.frame(experiments)) {
    stop("experiments must be a tibble/data.frame")
  }
  if (!all(c("config", "messages") %in% names(experiments))) {
    stop("experiments must have 'config' and 'messages' columns")
  }
  if (nrow(experiments) == 0) {
    warning("No experiments provided. Returning empty input tibble with result columns.")
    return(dplyr::bind_cols(experiments, tibble::tibble(
      response_text = character(0),
      raw_response_json = character(0),
      success = logical(0),
      error_message = character(0)
    )))
  }

  # Validate configs
  for (i in seq_len(nrow(experiments))) {
    if (!inherits(experiments$config[[i]], "llm_config")) {
      stop(sprintf("Row %d 'config' is not an llm_config object.", i))
    }
  }

  # Setup workers
  if (is.null(max_workers)) {
    max_workers <- min(future::availableCores(omit = 1L), nrow(experiments))
    max_workers <- max(1, max_workers)
  }

  current_plan <- future::plan()
  if (verbose) {
    message(sprintf("Setting up parallel execution with %d workers using plan: %s",
                    max_workers, class(current_plan)[1]))
  }
  on.exit(future::plan(current_plan), add = TRUE)
  if (!inherits(current_plan, "FutureStrategy") || inherits(current_plan, "sequential")) {
    future::plan(future::multisession, workers = max_workers)
  }

  if (verbose) {
    n_metadata_cols <- ncol(experiments) - 2
    message(sprintf("Processing %d experiments with %d user metadata columns",
                    nrow(experiments), n_metadata_cols))
  }

  # Worker function
  par_worker <- function(i_val) {
    start_time <- Sys.time()                 # begin timer
    current_config <- experiments$config[[i_val]]
    current_messages <- experiments$messages[[i_val]]
    raw_json_str <- NA_character_

    tryCatch({
      # Always call with json=TRUE to get attributes for raw_json
      result_content <- call_llm_robust(
        config = current_config,
        messages = current_messages,
        tries = tries,
        wait_seconds = wait_seconds,
        backoff_factor = backoff_factor,
        verbose = FALSE,
        json = TRUE, # Force TRUE to get raw_json attribute
        memoize = memoize
      )

      # Extract raw JSON from attributes
      raw_json_str <- attr(result_content, "raw_json") %||% NA_character_

      list(
        row_index = i_val,
        response_text = as.character(result_content), # Strip attributes
        raw_response_json = raw_json_str,
        success = TRUE,
        error_message = NA_character_,
        duration = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
    }, error = function(e) {
      list(
        row_index = i_val,
        response_text = NA_character_,
        raw_response_json = raw_json_str,
        success = FALSE,
        error_message = conditionMessage(e),
        duration = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
    })
  }

  # Execute in parallel
  if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(experiments))
      api_call_results_list <- future.apply::future_lapply(
        seq_len(nrow(experiments)),
        function(k) {
          res <- par_worker(k)
          p()
          res
        },
        future.seed = TRUE,
        future.packages = "LLMR",
        future.globals = TRUE
      )
    })
  } else {
    api_call_results_list <- future.apply::future_lapply(
      seq_len(nrow(experiments)),
      par_worker,
      future.seed = TRUE,
      future.packages = "LLMR",
      future.globals = TRUE
    )
  }

  # Convert results to dataframe
  api_results_df <- dplyr::bind_rows(api_call_results_list)

  # Prepare output with all original columns plus results
  output_df <- experiments
  output_df$response_text <- NA_character_
  output_df$raw_response_json <- NA_character_
  output_df$success <- NA
  output_df$error_message <- NA_character_
  output_df$duration   <- NA_real_


  # Fill in results by row index
  output_df$response_text[api_results_df$row_index] <- as.character(api_results_df$response_text)
  output_df$raw_response_json[api_results_df$row_index] <- as.character(api_results_df$raw_response_json)
  output_df$success[api_results_df$row_index] <- as.logical(api_results_df$success)
  output_df$error_message[api_results_df$row_index] <- as.character(api_results_df$error_message)
  output_df$duration  [api_results_df$row_index] <- as.numeric(api_results_df$duration)

  if (verbose) {
    successful_calls <- sum(output_df$success, na.rm = TRUE)
    message(sprintf("Parallel processing completed: %d/%d experiments successful",
                    successful_calls, nrow(output_df)))
  }

  if (simplify) {
    output_df <- .unnest_config_to_cols(output_df, config_col = "config")
  }

  return(output_df)
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
#' @param system_prompts  Optional character vector of system messages
#'                        (recycled against user_prompts).
#' @param system_prompt_labels Optional labels for the system prompts.
#'
#' @return A tibble with columns: config (list-column), messages (list-column),
#'   config_label, message_label, and repetition. Ready for use with call_llm_par().
#' @export
#'
#' @examples
#' \dontrun{
#'   # Factorial design: 3 configs x 2 message conditions x 10 reps = 60 experiments
#'   configs <- list(gpt4_config, claude_config, llama_config)
#'   messages <- list("Control prompt", "Treatment prompt")
#'
#'   experiments <- build_factorial_experiments(
#'     configs = configs,
#'     messages = messages,
#'     repetitions = 10,
#'     config_labels = c("gpt4", "claude", "llama"),
#'     message_labels = c("control", "treatment")
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
#' @param strategy Character. The future strategy to use. Options: "multisession", "multicore", "sequential".
#'                If NULL (default), automatically chooses "multisession".
#' @param workers Integer. Number of workers to use. If NULL, auto-detects optimal number
#'                (availableCores - 1, capped at 8).
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
