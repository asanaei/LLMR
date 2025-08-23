## json_mode.R
## Structured output and schema utilities (provider-agnostic)
## --------------------------------------------------------------------------

# internal: pick top-level scalar properties from a JSON Schema
.auto_scalar_fields_from_schema <- function(schema) {
  if (is.null(schema) || !is.list(schema)) return(character(0))
  props <- schema$properties
  if (is.null(props) || !length(props)) return(character(0))
  keep_type <- function(p) {
    tp <- tryCatch(p$type, error = function(...) NULL)
    if (is.null(tp)) return(FALSE)
    tp %in% c("string","number","integer","boolean")
  }
  nm <- names(props)
  if (is.null(nm)) return(character(0))
  nm[vapply(props, keep_type, logical(1))]
}

# internal: pick all top-level properties from a JSON Schema not just scalars
.auto_fields_from_schema <- function(schema) {
  if (is.null(schema) || !is.list(schema)) return(character(0))
  props <- schema$properties
  if (is.null(props) || !length(props)) return(character(0))
  nm <- names(props)
  if (is.null(nm)) return(character(0))
  nm
}




#' Enable Structured Output (Provider-Agnostic)
#'
#' Turn on structured output for a model configuration. Supports OpenAI‑compatible
#' providers (OpenAI, Groq, Together, x.ai, DeepSeek), Anthropic, and Gemini.
#'
#' @param config An [llm_config] object.
#' @param schema A named list representing a JSON Schema.
#' If `NULL`, OpenAI-compatible providers enforce a JSON object; Gemini switches
#' to JSON mime type; Anthropic only injects a tool when a schema is supplied.
#' @param name Character. Schema/tool name for providers requiring one. Default "llmr_schema".
#' @param method One of c("auto","json_mode","tool_call"). "auto" chooses the best
#'   per provider. You rarely need to change this.
#' @param strict Logical. Request strict validation when supported (OpenAI-compatible).
#' @return Modified `llm_config`.
#' @export
enable_structured_output <- function(config,
                              schema = NULL,
                                     name = "llmr_schema",
                                     method = c("auto","json_mode","tool_call"),
                              strict = TRUE) {
  stopifnot(inherits(config, "llm_config"))
  method <- match.arg(method)

  mp <- config$model_params %||% list()
  prov <- config$provider
  is_openai_compat <- prov %in% c("openai","groq","together","xai","deepseek")

  if (is_openai_compat) {
    # Prefer json_mode via response_format for structured output
    if (is.null(schema)) {
      mp$response_format <- list(type = "json_object")
    } else {
      mp$response_format <- list(
        type = "json_schema",
        json_schema = list(
          name   = as.character(name %||% "llmr_schema"),
          schema = schema,
          strict = isTRUE(strict)
        )
      )
      mp$json_schema <- schema
    }
  } else if (identical(prov, "anthropic")) {
    if (!is.null(schema)) {
      tool <- list(
        name         = as.character(name %||% "llmr_schema"),
        description  = "Return a JSON object that validates against the given schema.",
        input_schema = schema
      )
      # Do not drop user tools; append ours and force tool_choice to our schema tool
      mp$tools <- append(mp$tools %||% list(), list(tool))
      mp$tool_choice <- list(type = "tool", name = tool$name)
      mp$json_schema <- schema
    }
  } else if (identical(prov, "gemini")) {
    # Ask for JSON back
    mp$response_mime_type <- "application/json"
    if (!is.null(schema) && isTRUE(mp$gemini_enable_response_schema)) {
      mp$response_schema <- schema
      mp$json_schema <- schema
    }
  }

  config$model_params <- mp
  config
}

#' Disable Structured Output (clean provider toggles)
#'
#' Removes response_format/response_schema/response_mime_type and schema tool if present.
#' Keeps user tools intact.
#' @param config llm_config
#' @export
disable_structured_output <- function(config) {
  stopifnot(inherits(config, "llm_config"))
  mp <- config$model_params %||% list()

  # Remove OpenAI/Groq/Together/x.ai/DeepSeek response_format/json_schema fields
  mp$response_format <- NULL
  mp$json_schema <- NULL

  # Gemini toggles
  mp$response_mime_type <- NULL
  mp$response_schema <- NULL

  # Anthropic: remove only our schema tool (by name), keep user tools
  if (!is.null(mp$tools) && length(mp$tools)) {
    keep <- vapply(mp$tools, function(t) {
      nm <- t$name %||% ""
      # keep if not our default name
      !identical(nm, "llmr_schema")
    }, logical(1))
    mp$tools <- mp$tools[keep]
  }
  if (!is.null(mp$tool_choice) && identical(mp$tool_choice$type, "tool") &&
      identical(mp$tool_choice$name %||% "", "llmr_schema")) {
    mp$tool_choice <- NULL
  }

  config$model_params <- mp
  config
}

# internal: recursively convert integer vectors to double
.coerce_int_to_double <- function(x) {
  if (is.integer(x)) return(as.numeric(x))
  if (is.list(x)) return(lapply(x, .coerce_int_to_double))
  x
}

.strip_code_fences <- function(s) {
  s <- trimws(s)
  if (grepl("^```", s) && grepl("```\n?$|```\r?$|```\r\n?$", s)) {
    s <- sub("^```[a-zA-Z0-9_-]*\\s*\\n?", "", s)
    s <- sub("\\n?```\\s*$", "", s)
  }
  s
}

.largest_balanced_segment <- function(s, pattern = c("[{}]", "[\\[\\]]")) {
  # returns substring for the larger of the two bracket types if available
  best <- NULL
  best_len <- -1L
  for (pat in pattern) {
    locs <- gregexpr(pat, s)[[1]]
    if (locs[1] == -1) next
    syms <- substring(s, locs, locs)
    open <- if (pat == "[{}]") "{" else "["
    close <- if (pat == "[{}]") "}" else "]"
    depth <- 0L; start <- NA_integer_
    for (i in seq_along(syms)) {
      ch <- syms[i]
      if (ch == open) {
        if (depth == 0L) start <- locs[i]
        depth <- depth + 1L
      } else if (ch == close) {
        depth <- depth - 1L
        if (depth == 0L && !is.na(start)) {
          cand <- c(start, locs[i])
          clen <- cand[2] - cand[1] + 1L
          if (clen > best_len) {
            best <- cand
            best_len <- clen
          }
          start <- NA_integer_
        }
      }
    }
  }
  if (is.null(best)) return(NULL)
  substr(s, best[1], best[2])
}

#' Parse structured output emitted by an LLM
#'
#' Robustly parses an LLM's structured output (JSON). Works on character scalars
#' or an [llmr_response]. Strips code fences first, then tries strict parsing,
#' then attempts to extract the largest balanced \{...\} or \[...\].
#'
#'
#' The return contract is list-or-NULL; scalar-only JSON is treated as failure.
#'
#' Numerics are coerced to double for stability.
#'
#' @param x Character or [llmr_response].
#' @param strict_only If TRUE, do not attempt recovery via substring extraction.
#' @param simplify Logical passed to jsonlite::fromJSON (`simplifyVector = FALSE` when FALSE).
#' @return A parsed R object (list), or NULL on failure.
#' @export
llm_parse_structured <- function(x, strict_only = FALSE, simplify = FALSE) {
  if (inherits(x, "llmr_response")) x <- x$text %||% ""
  if (!is.character(x) || length(x) == 0) return(NULL)
  s <- as.character(x[[1]])
  if (is.na(s) || !nzchar(s)) return(NULL)

  s0 <- .strip_code_fences(s)
  out <- tryCatch(jsonlite::fromJSON(s0, simplifyVector = simplify), error = function(e) NULL)
  # Enforce contract: return list or NULL. Scalars are treated as failure.
  if (!is.null(out) && !is.list(out)) out <- NULL
  if (!is.null(out) || isTRUE(strict_only)) return(.coerce_int_to_double(out))

  seg <- .largest_balanced_segment(s0, pattern = c("[{}]", "[\\[\\]]"))
  if (is.null(seg)) return(NULL)
  out <- tryCatch(jsonlite::fromJSON(seg, simplifyVector = simplify), error = function(e) NULL)
  # Enforce list-or-NULL on recovery path too
  if (!is.null(out) && !is.list(out)) out <- NULL
  .coerce_int_to_double(out)
}

.extract_path <- function(obj, path) {
  if (is.null(obj)) return(NULL)
  if (!nzchar(path)) return(NULL)

  # JSON Pointer: /a/b/0/c
  if (substr(path, 1, 1) == "/") {
    parts <- strsplit(substr(path, 2, nchar(path)), "/", fixed = TRUE)[[1]]
    cur <- obj
    for (p in parts) {
      if (identical(p, "")) next
      idx <- suppressWarnings(as.integer(p))
      if (!is.na(idx)) {
        idx <- idx + 1L # JSON pointer uses 0-based
        if (is.list(cur) && length(cur) >= idx) {
          cur <- cur[[idx]]
        } else return(NULL)
      } else {
        # Safe name lookup
        if (is.list(cur) && (p %in% names(cur))) {
          cur <- cur[[p]]
        } else return(NULL)
      }
    }
    return(cur)
  }

  # Dot/bracket path: a.b[0].c
  tokens <- strsplit(path, "\\.", fixed = FALSE)[[1]]
  cur <- obj
  for (tok in tokens) {
    # capture bracket index if present
    m <- regmatches(tok, regexec("^(.*?)\\[(\\d+)\\]$", tok))[[1]]
    if (length(m) == 3L) {
      key <- m[2]; idx <- as.integer(m[3]) + 1L
      # step key
      if (!is.null(key) && nzchar(key)) {
        if (!(is.list(cur) && (key %in% names(cur)))) return(NULL)
        cur <- cur[[key]]
      }
      # step index
      if (!is.list(cur) || length(cur) < idx) return(NULL)
      cur <- cur[[idx]]
    } else {
      # plain key
      if (!(is.list(cur) && (tok %in% names(cur)))) return(NULL)
      cur <- cur[[tok]]
    }
  }
  cur
}

#' Parse structured fields from a column into typed vectors
#'
#' Extracts fields from a column containing structured JSON (string or list) and
#' appends them as new columns. Adds `structured_ok` (logical) and `structured_data` (list).
#'
#' - Supports nested-path extraction via dot/bracket paths (e.g., \code{a.b[0].c})
#'   or JSON Pointer (\code{/a/b/0/c}).
#' - When `allow_list = TRUE`, non-scalar values become list-columns; otherwise
#' they yield `NA` and only scalars are hoisted.
#'
#' @param .data data.frame/tibble
#' @param fields Character vector of fields or named vector (dest_name = path).
#' @param structured_col Column name to parse from. Default "response_text".
#' @param prefix Optional prefix for new columns.
#' @param allow_list Logical. If TRUE (default), non-scalar values (arrays/objects)
#'   are hoisted as list-columns instead of being dropped. If FALSE, only scalar
#'   fields are hoisted and non-scalars become NA.
#' @return `.data` with diagnostics and one new column per requested field.
#' @export
llm_parse_structured_col <- function(.data, fields, structured_col = "response_text", prefix = "", allow_list = TRUE) {
  # Always work on a data.frame and always return a tibble
  if (!is.data.frame(.data)) {
    .data <- as.data.frame(.data, stringsAsFactors = FALSE)
  }
  n <- nrow(.data)
  if (!length(fields)) fields <- character(0)

  out <- .data
  if (!structured_col %in% names(.data)) {
    out$structured_ok   <- rep(FALSE, n)
    out$structured_data <- replicate(n, NULL, simplify = FALSE)
    if (length(fields)) {
      dest <- if (is.null(names(fields))) fields else names(fields)
      for (f in dest) out[[paste0(prefix, f)]] <- rep(NA_character_, n)
    }
    return(out)
  }

  src <- .data[[structured_col]]
  parsed <- vector("list", n)
  ok     <- logical(n)
  for (i in seq_len(n)) {
    xi <- src[[i]]
    if (is.list(xi)) {
      parsed[[i]] <- xi
      ok[i] <- TRUE
    } else if (is.character(xi) && nzchar(xi[1])) {
      p <- llm_parse_structured(xi[1])
      # Accept only list results; reject scalars
      if (is.list(p)) {
        parsed[[i]] <- p
        ok[i] <- TRUE
      } else {
        parsed[i] <- list(NULL)  # do not shrink the list; [[<- NULL drops an element
        ok[i] <- FALSE
      }
    } else {
      parsed[i] <- list(NULL)    # do not shrink the list
      ok[i] <- FALSE
    }
  }
  # Defensive: ensure a one-to-one mapping (NULLs allowed); never shorter than n
  if (length(parsed) != n) length(parsed) <- n
  out$structured_ok   <- ok
  out$structured_data <- parsed

  # Field extraction and typing
  if (length(fields)) {
    src_paths <- unname(if (is.null(names(fields))) fields else fields)
    dest_names <- if (is.null(names(fields))) fields else names(fields)

    is_numish <- function(z) {
      if (!is.character(z) || length(z) != 1L) return(FALSE)
      zz <- trimws(z); if (!nzchar(zz)) return(FALSE)
      !is.na(suppressWarnings(as.numeric(zz)))
    }

    for (k in seq_along(src_paths)) {
      path <- src_paths[[k]]
      dest <- dest_names[[k]]
      raw_vals <- lapply(parsed, function(x) {
        if (is.null(x) || !is.list(x)) return(NULL)
        v <- if (grepl("^/|\\[|\\.", path)) {
          .extract_path(x, path)
        } else {
          if (path %in% names(x)) x[[path]] else NULL
        }
        v
      })

      # If we allow list-columns and any value is non-scalar, keep as list-column
      if (isTRUE(allow_list)) {
        non_scalar_present <- any(vapply(raw_vals, function(v) {
          !is.null(v) && !(is.atomic(v) && length(v) == 1L)
        }, logical(1)))
        if (non_scalar_present) {
          out[[paste0(prefix, dest)]] <- raw_vals
          next
        }
      }

      # Scalar-only hoist (or allow_list = FALSE): drop non-scalars
      vals <- lapply(raw_vals, function(v) {
        if (is.atomic(v) && length(v) == 1L) v else NULL
      })

      any_numeric <- any(vapply(vals, is.numeric, logical(1L)), na.rm = TRUE)
    if (any_numeric) {
      vals <- lapply(vals, function(v) {
        if (is.null(v)) return(NULL)
        if (is.character(v) && is_numish(v)) return(suppressWarnings(as.numeric(v)))
        v
      })
    } else {
      nonnull <- vals[!vapply(vals, is.null, logical(1))]
      if (length(nonnull) > 0L &&
          all(vapply(nonnull, function(v) is.character(v) && is_numish(v), logical(1)))) {
        vals <- lapply(vals, function(v) if (is.null(v)) NULL else suppressWarnings(as.numeric(v)))
      }
    }

    nonnull <- vals[!vapply(vals, is.null, logical(1))]
      col <- tryCatch({
        if (requireNamespace("vctrs", quietly = TRUE) && length(nonnull)) {
        coerce_scalar_like <- function(v) {
          if (is.null(v)) return(NULL)
          if (is.character(v) && length(v) == 1L) {
            vv <- trimws(v)
              if (grepl("^[-+]?(?:\\d+\\.?|\\d*\\.\\d+)$", vv)) {
                suppressWarnings(num <- as.numeric(vv)); if (!is.na(num)) return(num)
            }
              if (vv %in% c("true","false","TRUE","FALSE")) return(tolower(vv) == "true")
          }
          v
        }
        vals    <- lapply(vals, coerce_scalar_like)
        nonnull <- vals[!vapply(vals, is.null, logical(1))]
          ptype <- vctrs::vec_ptype_common(!!!nonnull)
        na_val <- vctrs::vec_cast(NA, ptype)
        casted <- lapply(vals, function(v) if (is.null(v)) na_val else vctrs::vec_cast(v, ptype))
        vctrs::vec_c(!!!casted)
      } else {
          if (length(nonnull) == 0L) {
            rep(NA_character_, n)
          } else {
            cls <- unique(vapply(nonnull, function(v) class(v)[1], character(1)))
            target <- if (all(cls %in% c("integer","numeric"))) "numeric"
            else if (all(cls %in% c("logical"))) "logical" else "character"
            NA_target <- switch(target, numeric = NA_real_, logical = NA, character = NA_character_)
            vapply(vals, function(v) {
              if (is.null(v)) return(NA_target)
              switch(target,
                       numeric  = suppressWarnings(as.numeric(v)),
                       logical  = as.logical(v),
                       character= as.character(v))
            }, FUN.VALUE = NA_target)
          }
      }
    }, error = function(...) {
      vapply(vals, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
    })

      out[[paste0(prefix, dest)]] <- col
    } # end fields loop
  }
  # Always return a tibble to play nicely with dplyr verbs
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(out)
  } else out
}

#' Validate structured JSON objects against a JSON Schema (locally)
#'
#' Adds `structured_valid` (logical) and `structured_error` (chr) by validating
#' each row's `structured_data` against `schema`. No provider calls are made.
#'
#' @param .data A data.frame with a `structured_data` list-column.
#' @param schema JSON Schema (R list)
#' @param structured_list_col Column name with parsed JSON. Default "structured_data".
#' @export
llm_validate_structured_col <- function(.data, schema, structured_list_col = "structured_data") {
  stopifnot(is.data.frame(.data))
  if (!structured_list_col %in% names(.data)) {
    warning(sprintf("Column '%s' not found; nothing to validate.", structured_list_col))
    .data$structured_valid <- NA
    .data$structured_error <- NA_character_
    return(.data)
  }
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    warning("Package 'jsonvalidate' is not installed; skipping validation.")
    .data$structured_valid <- NA
    .data$structured_error <- NA_character_
    return(.data)
  }
  sch_json <- jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null")
  validator <- jsonvalidate::json_validator(schema = sch_json, engine = "ajv")
  n <- nrow(.data); valid <- logical(n); err <- character(n)
  for (i in seq_len(n)) {
    obj <- .data[[structured_list_col]][[i]]
    if (is.null(obj)) { valid[i] <- FALSE; err[i] <- "no structured JSON"; next }
    txt <- jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null")
    ok  <- tryCatch(validator(txt, error = FALSE), error = function(e) FALSE)
    valid[i] <- isTRUE(ok)
    if (!ok) {
      err[i] <- tryCatch({ validator(txt, error = TRUE); NA_character_ }, error = function(e) conditionMessage(e))
    } else {
      err[i] <- NA_character_
    }
  }
  .data$structured_valid <- valid
  .data$structured_error <- err
  .data
}

#' Vectorized structured-output LLM
#'
#' Schema-first variant of [llm_fn()]. It enables structured output on the config,
#' calls the model via [call_llm_broadcast()], parses JSON, and optionally validates.
#'
#' @inheritParams llm_fn
#' @param .schema Optional JSON Schema list; if `NULL`, only JSON object is enforced.
#' @param .fields Optional fields to hoist from parsed JSON (supports nested paths).
#' @param .local_only If TRUE, do not send schema to the provider (parse/validate locally).
#' @param .validate_local If TRUE and `.schema` provided, validate locally.
#' @export
llm_fn_structured <- function(x,
                        prompt,
                        .config,
                        .system_prompt = NULL,
                        ...,
                        .schema = NULL,
                        .fields = NULL,
                              .local_only = FALSE,
                              .validate_local = TRUE) {
  cfg <- if (isTRUE(.local_only) || is.null(.schema)) .config
         else enable_structured_output(.config, schema = .schema)

  out  <- llm_fn(x, prompt, .config = cfg, .system_prompt = .system_prompt, ..., .return = "columns")
  # Auto-hoist all top-level properties if fields not supplied
  fields_auto <- if (is.null(.fields) && !is.null(.schema)) .auto_fields_from_schema(.schema) else .fields
  out2 <- llm_parse_structured_col(out, structured_col = "response_text", fields = fields_auto)

  if (!is.null(.schema) && isTRUE(.validate_local)) {
    out2 <- llm_validate_structured_col(out2, schema = .schema, structured_list_col = "structured_data")
  }
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out2) else out2
}

#' Data-frame mutate with structured output
#'
#' Drop-in schema-first variant of [llm_mutate()]. Produces parsed columns.
#'
#' @inheritParams llm_mutate
#' @param .schema Optional JSON Schema list; if `NULL`, only JSON object is enforced.
#' @param .fields Optional fields to hoist (supports nested paths).
#' @export
llm_mutate_structured <- function(.data,
                            output,
                            prompt = NULL,
                            .messages = NULL,
                            .config,
                            .system_prompt = NULL,
                            .before = NULL,
                            .after  = NULL,
                            .schema = NULL,
                            .fields = NULL,
                            ...) {
  out <- llm_mutate(.data,
                    {{ output }},
                    prompt = prompt,
                    .messages = .messages,
                    .config = if (is.null(.schema)) .config else enable_structured_output(.config, schema = .schema),
                    .system_prompt = .system_prompt,
                    .before = {{ .before }},
                    .after  = {{ .after }},
                    .return = "columns",
                    ...)
  fields_auto <- if (is.null(.fields) && !is.null(.schema)) .auto_fields_from_schema(.schema) else .fields
  out2 <- llm_parse_structured_col(out,
                                   structured_col = rlang::as_name(rlang::enquo(output)),
                                   fields   = fields_auto)
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out2) else out2
}

#' Parallel experiments with structured parsing
#'
#' Enables structured output on each config (if not already set), runs, then parses JSON.
#'
#' @param experiments Tibble with `config` and `messages` list-columns.
#' @param schema Optional JSON Schema list.
#' @param .fields Optional fields to hoist from parsed JSON (supports nested paths).
#' @param ... Passed to [call_llm_par()].
#' @export
call_llm_par_structured <- function(experiments, schema = NULL, .fields = NULL, ...) {
  stopifnot(is.data.frame(experiments), all(c("config","messages") %in% names(experiments)))
  ex <- experiments
  ex$config <- lapply(ex$config, function(cfg) {
    mp <- cfg$model_params %||% list()
    has_openai_rf <- !is.null(mp$response_format)
    has_gemini    <- !is.null(mp$response_mime_type) || !is.null(mp$response_schema)
    has_anthropic <- !is.null(mp$tools) && !is.null(mp$tool_choice)
    if (has_openai_rf || has_gemini || has_anthropic) return(cfg)
    enable_structured_output(cfg, schema = schema)
  })

  # Infer schema if not provided: look through config$model_params
  effective_schema <- schema
  if (is.null(effective_schema)) {
    for (cfg in ex$config) {
      mp <- cfg$model_params %||% list()
      found_schema <- mp$json_schema %||% mp$response_schema
      if (!is.null(found_schema)) {
        effective_schema <- found_schema
        break
      }
    }
  }

  # row-wise glue
  ex$messages <- lapply(seq_len(nrow(ex)), function(i) {
    msg   <- ex$messages[[i]]
    rowdf <- ex[i, setdiff(names(ex), c("config", "messages")), drop = FALSE]
    if (is.character(msg)) {
      nm  <- names(msg)
      out <- vapply(msg, function(s) as.character(glue::glue_data(rowdf, s, .na = "")), "")
      if (!is.null(nm)) names(out) <- nm
      out
    } else msg
  })

  res <- call_llm_par(ex, ...)
  # Default auto-hoist if user didn't provide fields but gave a schema (hoist all top-level props)
  fields_auto <- if (is.null(.fields) && !is.null(effective_schema)) .auto_fields_from_schema(effective_schema) else .fields
  out <- llm_parse_structured_col(res, structured_col = "response_text", fields = fields_auto, allow_list = TRUE)
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out
}


