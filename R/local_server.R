# local_server.R ----------------------------------------------------------------
# The llama.cpp server behind provider "llamacpp": one process per model,
# started on first use, listening on 127.0.0.1 with a key made for the
# session, and stopped when R exits. processx supervises each process, so a
# server does not outlive the R session even when R crashes.

.llmr_local_state <- new.env(parent = emptyenv())
.llmr_local_state$servers <- list()

# A key for one server, from the operating system's random source where there
# is one; it never touches the user's random-number stream.
.llmr_local_key <- function() {
  raw <- NULL
  if (.Platform$OS.type == "unix" && file.exists("/dev/urandom")) {
    raw <- tryCatch({
      con <- file("/dev/urandom", open = "rb", raw = TRUE)
      on.exit(close(con), add = TRUE)
      readBin(con, "raw", 20L)
    }, error = function(e) NULL)
  }
  if (length(raw) != 20L) {
    # Windows: hash several unpredictable process values; the server listens
    # only on 127.0.0.1, so the key guards against other local users.
    raw <- as.raw(strtoi(substring(digest::digest(
      paste(format(Sys.time(), "%Y%m%d%H%M%OS6"), Sys.getpid(), tempfile(),
            proc.time()[["elapsed"]], Sys.getenv("COMPUTERNAME")),
      algo = "sha1", serialize = FALSE), seq(1, 39, 2), seq(2, 40, 2)), 16L))
  }
  paste(sprintf("%02x", as.integer(raw)), collapse = "")
}

# A free TCP port on the loopback interface, found by trying to listen on it.
.llmr_free_port <- function() {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  })
  set.seed(NULL)
  for (p in sample(49152:65000, 50L)) {
    s <- tryCatch(serverSocket(p), error = function(e) NULL, warning = function(w) NULL)
    if (!is.null(s)) {
      close(s)
      return(as.integer(p))
    }
  }
  stop("No free local port was found for the llama.cpp server.", call. = FALSE)
}

# The server's environment: the session's, without the LLAMA_* variables that
# llama-server reads as settings (so a stray LLAMA_ARG_N_PREDICT cannot change
# results), plus the server's key.
.llmr_local_child_env <- function(key) {
  env <- Sys.getenv()
  env <- env[!grepl("^LLAMA_", names(env))]
  env[["LLAMA_API_KEY"]] <- key
  env
}

.llmr_local_alive <- function(srv) {
  !is.null(srv) && inherits(srv$process, "process") && isTRUE(srv$process$is_alive())
}

.llmr_local_log_tail <- function(path, n = 15L) {
  if (!file.exists(path)) return("")
  x <- readLines(path, warn = FALSE)
  paste(utils::tail(x, n), collapse = "\n")
}

#' Start, stop, and inspect local model servers
#'
#' Each installed local model runs in its own llama.cpp server process on this
#' computer. A server starts by itself on the first call that needs it, so
#' `llm_local_start()` is needed only to choose settings other than the
#' defaults, such as the context length, the number of CPU threads, or CPU
#' instead of GPU. A server listens only on `127.0.0.1`, accepts only requests
#' that carry a key generated when it starts (only its `/health` status is
#' open), and stops when R exits.
#'
#' Results depend on where the model runs. On the same model,
#' log-probabilities computed on the GPU and on the CPU differ in the first
#' decimal place (by up to about 0.2 in tests with Qwen3 0.6B), and on the CPU
#' they also depend on settings such as flash attention, which LLMR turns off.
#' Repeated requests on the same device and build give identical values. LLMR
#' records the device with every result of the token-level functions; numbers
#' meant to be compared should come from the same device and, for the CPU,
#' the same type of machine.
#'
#' @param model Name of an installed model; see [llm_local_models()].
#' @param ctx Context length in tokens; by default the model's registry
#'   value (4096 for most chat models, 8192 for embedding models).
#' @param device `"auto"` (the GPU when the installed build has one, otherwise
#'   the CPU), `"cpu"`, or `"gpu"`.
#' @param threads Number of CPU threads; by default the `llmr.local_threads`
#'   option, or else llama.cpp's own choice.
#' @param parallel Number of requests served at once. Chat models use 1 so
#'   that requests are handled in order; embedding models default to 4.
#' @param timeout Seconds to wait for the server to load the model.
#' @param server Path to a `llama-server` program to use instead of the
#'   installed one, for systems the official builds do not support.
#' @param args Further command-line arguments for `llama-server`.
#' @param dir The local model directory; see [llm_local_dir()].
#' @return `llm_local_start()` returns the server's address, invisibly.
#'   `llm_local_stop()` returns the names of the stopped servers, invisibly.
#'   `llm_local_status()` returns an `llmr_local_status` list with the
#'   installed server build, the installed models, and the running servers.
#' @seealso [llm_local_install()]
#' @examples
#' llm_local_status()
#' if (llm_local_ready("smollm2-135m")) {
#'   llm_local_start("smollm2-135m", threads = 2)
#'   llm_local_stop("smollm2-135m")
#' }
#' @export
llm_local_start <- function(model, ctx = NULL, device = c("auto", "cpu", "gpu"),
                            threads = NULL, parallel = NULL, timeout = 120,
                            server = NULL, args = character(), dir = llm_local_dir()) {
  device <- match.arg(device)
  entry <- .llmr_local_entry(model, dir)
  man <- .llmr_local_manifest_read(dir)
  if (is.null(server) && !.llmr_local_runtime_present(man, dir)) {
    stop("The llama.cpp server is not installed; run llm_local_install() first.", call. = FALSE)
  }
  if (!.llmr_local_model_present(man, model, dir)) {
    stop(sprintf("Model %s is not installed; run llm_local_install(\"%s\") first.", model, model),
         call. = FALSE)
  }
  bin <- server %||% file.path(dir, man$runtime$server)
  if (!file.exists(bin)) stop(sprintf("No llama-server program at %s.", bin), call. = FALSE)
  threads <- threads %||% getOption("llmr.local_threads")
  if (!is.null(threads)) {
    if (!is.numeric(threads) || length(threads) != 1L || is.na(threads) || threads < 1) {
      stop("`threads` must be one positive whole number.", call. = FALSE)
    }
    threads <- as.integer(threads)
  }
  ctx <- as.integer(ctx %||% entry$ctx %||% 4096L)
  embedding <- identical(entry$kind, "embedding")
  parallel <- as.integer(parallel %||% if (embedding) (entry$parallel %||% 4L) else 1L)
  gpu_build <- !is.null(server) || !identical(man$runtime$gpu %||% "none", "none")
  dev <- switch(device, auto = if (gpu_build) "gpu" else "cpu", cpu = "cpu", gpu = "gpu")
  if (dev == "gpu" && !gpu_build) {
    stop("The installed llama.cpp build has no GPU support; install with device = \"vulkan\" on Linux or Windows, or use device = \"cpu\".",
         call. = FALSE)
  }
  settings <- list(ctx = ctx, device = dev, threads = threads, parallel = parallel,
                   server = normalizePath(bin, winslash = "/"), args = args)

  cur <- .llmr_local_state$servers[[model]]
  if (.llmr_local_alive(cur)) {
    if (identical(cur$settings, settings)) return(invisible(cur$url))
    llm_local_stop(model)
  }

  port <- .llmr_free_port()
  key <- .llmr_local_key()
  model_path <- file.path(dir, man$models[[model]]$path)
  a <- c("-m", model_path, "--host", "127.0.0.1", "--port", port,
         "--no-ui", "--offline", "-fa", "off", "-np", parallel)
  a <- c(a, if (embedding) c("--embeddings", "-c", ctx * parallel, "-b", ctx, "-ub", ctx)
            else c("-c", ctx))
  a <- c(a, if (dev == "cpu") c("-dev", "none", "-ngl", "0") else c("-ngl", "999"))
  if (!is.null(threads)) a <- c(a, "-t", threads)
  a <- c(a, args)
  log_dir <- file.path(dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  old_logs <- list.files(log_dir, full.names = TRUE)
  unlink(old_logs[difftime(Sys.time(), file.mtime(old_logs), units = "days") > 7])
  log_path <- file.path(log_dir, sprintf("%s-%d.log", model, Sys.getpid()))
  proc <- processx::process$new(
    bin, as.character(a),
    env = .llmr_local_child_env(key),
    stdout = log_path, stderr = "2>&1",
    cleanup = TRUE, cleanup_tree = TRUE, supervise = TRUE
  )
  url <- sprintf("http://127.0.0.1:%d", port)
  deadline <- Sys.time() + timeout
  ready <- FALSE
  while (Sys.time() < deadline) {
    if (!proc$is_alive()) break
    st <- tryCatch(
      httr2::request(paste0(url, "/health")) |>
        httr2::req_timeout(5) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_status(),
      error = function(e) NA_integer_)
    if (isTRUE(st == 200L)) { ready <- TRUE; break }
    Sys.sleep(0.2)
  }
  if (!ready) {
    alive <- proc$is_alive()
    if (alive) proc$kill_tree()
    stop(sprintf("The llama.cpp server for %s %s. Its log (%s) ends with:\n%s",
                 model, if (alive) sprintf("did not become ready within %s seconds", timeout)
                        else "stopped while loading the model",
                 log_path, .llmr_local_log_tail(log_path)), call. = FALSE)
  }
  srv <- list(model = model, kind = entry$kind, url = url, port = port, key = key,
              process = proc, settings = settings, device = dev, ctx = ctx,
              build = if (is.null(server)) man$runtime$build else "user-supplied",
              sha256 = man$models[[model]]$sha256, log = log_path,
              started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  meta <- tryCatch(.llmr_local_request(srv, "/v1/models", method = "GET"), error = function(e) NULL)
  m1 <- if (!is.null(meta$data) && length(meta$data)) meta$data[[1]]$meta else NULL
  srv$n_vocab <- as.integer(m1$n_vocab %||% NA_integer_)
  srv$n_embd  <- as.integer(m1$n_embd %||% NA_integer_)
  srv$ftype   <- m1$ftype %||% NA_character_
  .llmr_local_state$servers[[model]] <- srv
  invisible(url)
}

#' @rdname llm_local_start
#' @export
llm_local_stop <- function(model = NULL) {
  names_ <- if (is.null(model)) names(.llmr_local_state$servers) else model
  stopped <- character()
  for (m in names_) {
    srv <- .llmr_local_state$servers[[m]]
    if (.llmr_local_alive(srv)) {
      srv$process$kill_tree()
      stopped <- c(stopped, m)
    }
    .llmr_local_state$servers[[m]] <- NULL
  }
  invisible(stopped)
}

#' @rdname llm_local_start
#' @export
llm_local_status <- function(dir = llm_local_dir()) {
  man <- tryCatch(.llmr_local_manifest_read(dir), error = function(e) list(models = list()))
  servers <- .llmr_local_state$servers
  srv_tbl <- tibble::tibble(
    model   = as.character(names(servers)),
    url     = vapply(servers, function(s) s$url, ""),
    alive   = vapply(servers, .llmr_local_alive, NA),
    device  = vapply(servers, function(s) s$device, ""),
    ctx     = vapply(servers, function(s) as.integer(s$ctx), 0L),
    pid     = vapply(servers, function(s) as.integer(s$process$get_pid()), 0L)
  )
  out <- list(
    dir = dir,
    runtime = if (.llmr_local_runtime_present(man, dir))
      sprintf("llama.cpp %s (build %s, %s)", man$runtime$release, man$runtime$build, man$runtime$asset)
      else NA_character_,
    models = names(Filter(function(m) file.exists(file.path(dir, m$path)), man$models)),
    servers = srv_tbl
  )
  structure(out, class = "llmr_local_status")
}

#' @export
print.llmr_local_status <- function(x, ...) {
  cat("Local models in ", x$dir, "\n", sep = "")
  cat("  server program: ", if (is.na(x$runtime)) "not installed" else x$runtime, "\n", sep = "")
  cat("  installed models: ", if (length(x$models)) paste(x$models, collapse = ", ") else "none", "\n", sep = "")
  if (nrow(x$servers)) {
    cat("  running servers:\n")
    for (i in seq_len(nrow(x$servers))) {
      s <- x$servers[i, ]
      cat(sprintf("    %s at %s (%s, ctx %d)%s\n", s$model, s$url, s$device, s$ctx,
                  if (isTRUE(s$alive)) "" else " [stopped]"))
    }
  } else {
    cat("  running servers: none\n")
  }
  invisible(x)
}

# The running server for a model, started with the default settings when none
# is running. Never downloads anything.
.llmr_local_ensure <- function(model, dir = llm_local_dir()) {
  srv <- .llmr_local_state$servers[[model]]
  if (.llmr_local_alive(srv)) return(srv)
  if (!llm_local_ready(model, dir)) {
    stop(sprintf("Local model %s is not installed; run llm_local_install(\"%s\").", model, model),
         call. = FALSE)
  }
  llm_local_start(model, device = getOption("llmr.local_device", "auto"), dir = dir)
  .llmr_local_state$servers[[model]]
}

# One request to a local server, with its session key. HTTP errors become the
# same typed conditions as provider errors.
.llmr_local_request <- function(srv, path, body = NULL, method = c("POST", "GET"),
                                log_kind = NULL) {
  method <- match.arg(method)
  start <- Sys.time()
  req <- httr2::request(paste0(srv$url, path)) |>
    httr2::req_headers(Authorization = paste("Bearer", srv$key)) |>
    httr2::req_timeout(getOption("llmr.timeout", 600)) |>
    httr2::req_error(is_error = function(r) FALSE)
  if (method == "POST") req <- httr2::req_body_json(req, body %||% list(), auto_unbox = TRUE, digits = NA)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    .llmr_error(sprintf("The local server for %s could not be reached (%s).", srv$model,
                        conditionMessage(resp)),
                category = "server", provider = "llamacpp", model = srv$model)
  }
  status <- httr2::resp_status(resp)
  parsed <- tryCatch(httr2::resp_body_json(resp, check_type = FALSE), error = function(e) NULL)
  if (status >= 400L) {
    msg <- parsed$error$message %||% httr2::resp_body_string(resp)
    code <- parsed$error$type %||% NA_character_
    .llmr_error(sprintf("The local server for %s returned HTTP %d: %s", srv$model, status, msg),
                category = .llmr_classify_error("llamacpp", status, code, msg),
                status_code = status, provider = "llamacpp", model = srv$model,
                code = code, response_body = parsed)
  }
  if (!is.null(log_kind)) {
    .llmr_log_event(kind = log_kind, provider = "llamacpp", model = srv$model, status = status,
                    request = req,
                    duration_s = as.numeric(difftime(Sys.time(), start, units = "secs")))
  }
  parsed
}
