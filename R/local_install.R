# local_install.R ---------------------------------------------------------------
# One-call installation of a local model: the llama.cpp server binary for the
# user's platform and the model's GGUF file, both downloaded only when the user
# asks, verified against pinned sha256 values, and recorded in a manifest so a
# second call makes no network request. Nothing here runs at install or load
# time of LLMR.

#' Directory holding local models and the llama.cpp server
#'
#' Local models, the llama.cpp server binary, server logs, and a manifest of
#' what is installed live in one directory. It is, in order of precedence, the
#' `llmr.local_dir` option, the `LLMR_LOCAL_DIR` environment variable, or the
#' user cache directory that [tools::R_user_dir()] reports for LLMR. Files
#' there can be removed with [llm_local_uninstall()].
#'
#' @return The directory path (it may not exist yet).
#' @seealso [llm_local_install()], [llm_local_uninstall()]
#' @examples
#' llm_local_dir()
#' @export
llm_local_dir <- function() {
  d <- getOption("llmr.local_dir")
  if (is.null(d) || !nzchar(d)) d <- Sys.getenv("LLMR_LOCAL_DIR", unset = "")
  if (!nzchar(d)) d <- tools::R_user_dir("LLMR", which = "cache")
  normalizePath(path.expand(d), winslash = "/", mustWork = FALSE)
}

# ---- manifest -------------------------------------------------------------------

.llmr_local_manifest_path <- function(dir) file.path(dir, "manifest.json")

.llmr_local_manifest_read <- function(dir = llm_local_dir()) {
  p <- .llmr_local_manifest_path(dir)
  if (!file.exists(p)) return(list(runtime = NULL, models = list()))
  m <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) e)
  if (inherits(m, "error")) {
    stop(sprintf("The local model manifest %s cannot be read (%s). Remove it, or run llm_local_uninstall(all = TRUE), and install again.",
                 p, conditionMessage(m)), call. = FALSE)
  }
  m$models <- m$models %||% list()
  m
}

.llmr_local_manifest_write <- function(m, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  p <- .llmr_local_manifest_path(dir)
  tmp <- paste0(p, ".tmp")
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA), tmp)
  if (!file.rename(tmp, p)) {
    file.copy(tmp, p, overwrite = TRUE); unlink(tmp)
  }
  invisible(m)
}

.llmr_local_model_present <- function(man, name, dir) {
  m <- man$models[[name]]
  if (is.null(m)) return(FALSE)
  f <- file.path(dir, m$path)
  file.exists(f) && isTRUE(file.size(f) == as.numeric(m$size))
}

.llmr_local_runtime_present <- function(man, dir) {
  r <- man$runtime
  !is.null(r) && identical(r$build, .llmr_local_runtime$build) &&
    file.exists(file.path(dir, r$server))
}

# ---- platform ------------------------------------------------------------------

# The llama.cpp asset for this machine. The server is a separate process, so an
# R running under Rosetta on Apple silicon still gets the native arm64 build
# with Metal.
.llmr_local_platform <- function(device = c("auto", "cpu", "vulkan"),
                                 sys = Sys.info()[["sysname"]],
                                 machine = Sys.info()[["machine"]],
                                 rosetta = NULL) {
  device <- match.arg(device)
  mach <- tolower(machine)
  arm <- grepl("arm64|aarch64", mach)
  if (identical(sys, "Windows")) {
    arm <- arm || identical(toupper(Sys.getenv("PROCESSOR_ARCHITECTURE")), "ARM64")
  }
  if (identical(sys, "Darwin") && !arm) {
    if (is.null(rosetta)) {
      translated <- tryCatch(suppressWarnings(system2("sysctl", c("-n", "sysctl.proc_translated"),
                                                      stdout = TRUE, stderr = FALSE)),
                             error = function(e) "")
      rosetta <- identical(trimws(translated[1] %||% ""), "1")
    }
    arm <- isTRUE(rosetta)
  }
  arch <- if (arm) "arm64" else "x64"
  key <- switch(sys,
    Darwin  = paste0("macos-", arch),
    Linux   = if (device == "vulkan") paste0("ubuntu-vulkan-", arch) else paste0("ubuntu-", arch),
    Windows = if (device == "vulkan") paste0("win-vulkan-", arch) else paste0("win-cpu-", arch),
    stop(sprintf("Local models are not available on %s; llama.cpp publishes builds for macOS, Linux, and Windows.", sys),
         call. = FALSE)
  )
  if (identical(sys, "Darwin") && device == "vulkan") {
    stop("The Vulkan build is for Linux and Windows; on macOS, `device = \"auto\"` uses Metal on Apple silicon.",
         call. = FALSE)
  }
  list(key = key, sys = sys, arch = arch, asset = .llmr_local_runtime$assets[[key]])
}

# Windows builds need the Microsoft Visual C++ runtime, which their archives do
# not include.
.llmr_local_check_msvc <- function(arch) {
  if (!identical(Sys.info()[["sysname"]], "Windows")) return(invisible(TRUE))
  sysdir <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32")
  need <- c("vcruntime140.dll", "msvcp140.dll")
  if (identical(arch, "x64")) need <- c(need, "vcruntime140_1.dll")
  missing <- need[!file.exists(file.path(sysdir, need))]
  if (length(missing)) {
    url <- if (identical(arch, "arm64")) "https://aka.ms/vs/17/release/vc_redist.arm64.exe"
           else "https://aka.ms/vs/17/release/vc_redist.x64.exe"
    stop(sprintf(paste0("The llama.cpp server needs the Microsoft Visual C++ runtime, which is not ",
                        "installed (missing %s). Install it from %s and run llm_local_install() again."),
                 paste(missing, collapse = ", "), url), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- downloads -----------------------------------------------------------------

.llmr_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# Download `url` to `dest` through a resumable partial file, then check size
# and sha256 before moving it into place. A mismatch deletes the partial file.
.llmr_local_download <- function(url, dest, sha256, size = NULL, quiet = FALSE,
                                 timeout = 3600) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(dest, ".part")
  # A partial file that is already complete (R stopped before it was checked)
  # goes straight to the checks; asking the server to resume it would only
  # return "range not satisfiable".
  complete <- file.exists(part) && !is.null(size) && !is.na(size) &&
    file.size(part) >= as.numeric(size)
  if (!complete) {
    res <- curl::multi_download(url, part, resume = TRUE, progress = !quiet,
                                timeout = timeout)
    status <- res$status_code[1]
    ok <- isTRUE(res$success[1]) && file.exists(part) &&
      (isTRUE(status %in% c(200L, 206L)) || startsWith(url, "file://"))
  } else {
    ok <- TRUE
  }
  if (!ok) {
    if (!isTRUE(status %in% c(200L, 206L))) unlink(part)
    why <- res$error[1]
    stop(sprintf("Download of %s failed%s%s. Rerun llm_local_install() to resume it.",
                 url, if (!is.na(status)) sprintf(" (HTTP %s)", status) else "",
                 if (!is.na(why) && nzchar(why)) paste0(": ", why) else ""),
         call. = FALSE)
  }
  if (!is.null(size) && !is.na(size) && file.size(part) != as.numeric(size)) {
    got <- file.size(part)
    unlink(part)
    stop(sprintf("The file from %s has %s bytes where %s were expected; it was deleted.",
                 url, format(got, big.mark = ","), format(as.numeric(size), big.mark = ",")),
         call. = FALSE)
  }
  got <- .llmr_sha256(part)
  if (!identical(got, tolower(sha256))) {
    unlink(part)
    stop(sprintf("The file from %s failed its checksum (sha256 %s, expected %s); it was deleted.",
                 url, got, sha256), call. = FALSE)
  }
  if (!file.rename(part, dest)) {
    file.copy(part, dest, overwrite = TRUE)
    unlink(part)
  }
  invisible(dest)
}

.llmr_hf_url <- function(repo, revision, file) {
  sprintf("https://huggingface.co/%s/resolve/%s/%s", repo, revision, utils::URLencode(file))
}

# Fill a custom model's revision, sha256, size, and license from the Hugging
# Face API, so the file is pinned from the first install on.
.llmr_local_resolve_spec <- function(spec) {
  need_rev <- !grepl("^[0-9a-f]{40}$", spec$revision %||% "")
  if (!need_rev && !is.null(spec$sha256) && !is.null(spec$size) && !is.null(spec$license)) {
    return(spec)
  }
  api <- sprintf("https://huggingface.co/api/models/%s/revision/%s", spec$repo,
                 utils::URLencode(spec$revision %||% "main", reserved = TRUE))
  info <- tryCatch(jsonlite::fromJSON(api, simplifyVector = FALSE), error = function(e) e)
  if (inherits(info, "error")) {
    stop(sprintf("Cannot read %s from the Hugging Face API (%s). Supply `revision`, `sha256`, `size`, and `license` to llm_local_model() to install without it.",
                 spec$repo, conditionMessage(info)), call. = FALSE)
  }
  if (isTRUE(info$gated) || is.character(info$gated)) {
    stop(sprintf("%s is gated on Hugging Face and needs an account and accepted terms; LLMR does not download gated models.",
                 spec$repo), call. = FALSE)
  }
  spec$revision <- info$sha
  sib <- Filter(function(s) identical(s$rfilename, spec$file), info$siblings %||% list())
  tree <- tryCatch(jsonlite::fromJSON(sprintf("https://huggingface.co/api/models/%s/tree/%s",
                                              spec$repo, info$sha), simplifyVector = FALSE),
                   error = function(e) list())
  entry <- Filter(function(t) identical(t$path, spec$file), tree)
  if (!length(sib) && !length(entry)) {
    stop(sprintf("%s has no file named %s at commit %s.", spec$repo, spec$file, info$sha), call. = FALSE)
  }
  lfs <- if (length(entry)) entry[[1]]$lfs else NULL
  if (is.null(spec$sha256)) {
    if (is.null(lfs$oid)) stop(sprintf("Hugging Face reports no sha256 for %s; supply `sha256`.", spec$file), call. = FALSE)
    spec$sha256 <- lfs$oid
  }
  if (is.null(spec$size)) spec$size <- lfs$size %||% entry[[1]]$size
  if (is.null(spec$license)) {
    lic <- info$cardData$license %||% NULL
    spec$license <- if (is.null(lic)) "not stated in the repository metadata" else paste(unlist(lic), collapse = ", ")
  }
  spec
}

# ---- consent -------------------------------------------------------------------

.llmr_local_consent <- function(items, accept_license) {
  if (!length(items)) return(invisible(TRUE))
  lines <- vapply(items, function(it) sprintf(
    "  %s: %s MB from %s\n    license: %s (%s)%s",
    it$label, format(round(as.numeric(it$size) / 1e6), big.mark = ","), it$source,
    it$license, it$license_url,
    if (!is.null(it$note) && nzchar(it$note %||% "")) paste0("\n    ", it$note) else ""), "")
  total <- sum(vapply(items, function(it) as.numeric(it$size), 0)) / 1e6
  msg <- paste0("llm_local_install() will download:\n", paste(lines, collapse = "\n"),
                sprintf("\nTotal: %s MB, saved under %s.", format(round(total), big.mark = ","),
                        items[[1]]$dir))
  if (isTRUE(accept_license)) {
    message(msg)
    return(invisible(TRUE))
  }
  if (!interactive()) {
    stop(paste0(msg, "\nDownloading requires agreeing to these licenses: call ",
                "llm_local_install(..., accept_license = TRUE)."), call. = FALSE)
  }
  message(msg)
  ans <- utils::askYesNo("Download these files and accept their licenses?", default = FALSE)
  if (!isTRUE(ans)) stop("Nothing was downloaded.", call. = FALSE)
  invisible(TRUE)
}

# ---- runtime -------------------------------------------------------------------

.llmr_local_find_server <- function(root) {
  hits <- list.files(root, pattern = "^llama-server(\\.exe)?$", recursive = TRUE, full.names = TRUE)
  if (!length(hits)) stop("The llama.cpp archive holds no llama-server program.", call. = FALSE)
  hits[order(nchar(hits))][1]
}

.llmr_local_unpack <- function(archive, exdir) {
  unlink(exdir, recursive = TRUE)
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  zip <- grepl("[.]zip$", archive)
  entries <- if (zip) utils::unzip(archive, list = TRUE)$Name else utils::untar(archive, list = TRUE, tar = "internal")
  if (any(startsWith(entries, "/") | grepl("(^|/)[.][.](/|$)", entries))) {
    stop("The llama.cpp archive holds paths outside its own folder; it was not unpacked.", call. = FALSE)
  }
  if (zip) {
    utils::unzip(archive, exdir = exdir)
  } else {
    utils::untar(archive, exdir = exdir, tar = "internal")
  }
  server <- .llmr_local_find_server(exdir)
  bins <- list.files(dirname(server), full.names = TRUE)
  Sys.chmod(bins[!dir.exists(bins)], mode = "0755", use_umask = FALSE)
  server
}

# Run the server once with --version, so a binary that cannot start on this
# system fails at install time with a reason, not later inside a model call.
.llmr_local_smoke <- function(server, sys) {
  res <- tryCatch(processx::run(server, "--version", error_on_status = FALSE, timeout = 60),
                  error = function(e) list(status = -1L, stdout = "", stderr = conditionMessage(e)))
  out <- paste(res$stdout, res$stderr)
  if (!identical(as.integer(res$status), 0L) || !grepl("version", out, ignore.case = TRUE)) {
    hint <- switch(sys,
      Linux = paste0(" The official Linux build needs glibc 2.34 or later (Ubuntu 22.04, Debian 12, ",
                     "RHEL 9, or newer) and OpenSSL 3; on older systems, build llama.cpp yourself and ",
                     "pass its llama-server to llm_local_start(server = )."),
      Windows = " Check that the Microsoft Visual C++ runtime is installed and that security software allows the program.",
      "")
    stop(sprintf("The downloaded llama.cpp server does not run on this machine: %s.%s",
                 trimws(substr(out, 1, 400)), hint), call. = FALSE)
  }
  invisible(TRUE)
}

#' Install a local model and the llama.cpp server
#'
#' Downloads what a local model needs and records it, so that
#' `llm_config("llamacpp", model)` works offline from then on: the llama.cpp
#' server program for this computer's platform (from the official llama.cpp
#' releases on GitHub) and the model's GGUF file (from Hugging Face). Each file
#' is pinned to an exact version and checked against its sha256 after download;
#' a file that fails the check is deleted. Before downloading, the function
#' lists every file with its size, source, and license, and asks for
#' agreement; in a non-interactive session `accept_license = TRUE` gives that
#' agreement. Files already present are not downloaded again, so a second call
#' makes no network request. Nothing is downloaded at any other time.
#'
#' The server runs on this computer, listens only on `127.0.0.1`, and requires
#' a key generated for each session. It starts on the first call that needs
#' it and stops when R exits (see [llm_local_start()]). On macOS with Apple
#' silicon it uses the GPU through Metal; elsewhere it uses the CPU, or a GPU
#' through Vulkan with `device = "vulkan"` on Linux and Windows.
#'
#' @param model A model name from [llm_local_models()], or a specification
#'   from [llm_local_model()] for another GGUF file on Hugging Face. `NULL`
#'   installs only the server.
#' @param accept_license `TRUE` to agree to the licenses of the downloaded
#'   files without being asked, as a non-interactive session requires.
#' @param device `"auto"` (Metal on Apple silicon, otherwise CPU), `"cpu"`, or
#'   `"vulkan"` (Linux and Windows).
#' @param dir Directory for the files; see [llm_local_dir()].
#' @param verify If `TRUE`, recompute the sha256 of files already installed.
#' @param force If `TRUE`, download again even when the files are present.
#' @param quiet If `TRUE`, show no download progress.
#' @param timeout Seconds allowed for each download.
#' @return The model name, invisibly.
#' @seealso [llm_local_models()], [llm_local_status()], [llm_local_uninstall()],
#'   [llm_next_token()], [llm_continue()], [llm_score()]
#' @examples
#' \dontrun{
#' llm_local_install("qwen3-0.6b")
#' cfg <- llm_config("llamacpp", "qwen3-0.6b", temperature = 0)
#' call_llm(cfg, "What is the capital of France? One word.")
#'
#' llm_local_install("bge-m3")
#' X <- get_batched_embeddings(c("budget", "\u0628\u0648\u062f\u062c\u0647"),
#'                             llm_config("llamacpp", "bge-m3"))
#' }
#' @export
llm_local_install <- function(model = "qwen3-0.6b", accept_license = FALSE,
                              device = c("auto", "cpu", "vulkan"),
                              dir = llm_local_dir(), verify = FALSE, force = FALSE,
                              quiet = !interactive(), timeout = 3600) {
  device <- match.arg(device)
  plat <- .llmr_local_platform(device)
  .llmr_local_check_msvc(plat$arch)
  dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
  man <- .llmr_local_manifest_read(dir)

  # what is wanted
  spec <- NULL
  if (inherits(model, "llmr_local_model")) {
    spec <- model
    model <- spec$name
    reg <- .llmr_local_registry_list()
    if (model %in% names(reg)) {
      stop(sprintf("\"%s\" is the name of a registry model; give the custom model another `name`.", model),
           call. = FALSE)
    }
  }
  entry <- if (is.null(model)) NULL else if (!is.null(spec)) NULL else .llmr_local_entry(model, dir)

  need_runtime <- force || !.llmr_local_runtime_present(man, dir) ||
    !identical(man$runtime$asset %||% "", plat$key)
  need_model <- !is.null(model) &&
    (force || !.llmr_local_model_present(man, model, dir))

  if (!is.null(spec) && need_model) {
    spec <- .llmr_local_resolve_spec(spec)
    entry <- unclass(spec)
  }

  # verify what is present
  if (isTRUE(verify) && !need_runtime) {
    a <- .llmr_local_runtime$assets[[man$runtime$asset]]
    arch_file <- file.path(dir, man$runtime$archive)
    if (file.exists(arch_file) && !identical(.llmr_sha256(arch_file), a$sha256)) need_runtime <- TRUE
  }
  if (isTRUE(verify) && !is.null(model) && !need_model) {
    m <- man$models[[model]]
    if (!identical(.llmr_sha256(file.path(dir, m$path)), m$sha256)) {
      warning(sprintf("The installed file for %s failed its checksum; downloading it again.", model),
              call. = FALSE)
      need_model <- TRUE
      entry <- entry %||% m$spec %||% .llmr_local_entry(model, dir)
    }
  }

  items <- list()
  if (need_runtime) {
    items[[length(items) + 1L]] <- list(
      label = sprintf("llama.cpp server %s (%s)", .llmr_local_runtime$release, plat$key),
      size = plat$asset$size, source = paste0(.llmr_local_runtime$base_url, plat$asset$file),
      license = .llmr_local_runtime$license, license_url = .llmr_local_runtime$license_url,
      note = NULL, dir = dir)
  }
  if (need_model) {
    items[[length(items) + 1L]] <- list(
      label = sprintf("model %s (%s)", model, entry$file),
      size = entry$size, source = sprintf("huggingface.co/%s at %s", entry$repo, substr(entry$revision, 1, 12)),
      license = entry$license, license_url = entry$license_url, note = entry$note, dir = dir)
  }
  if (!length(items)) {
    if (!quiet) message(sprintf("%s is installed in %s.", model %||% "The llama.cpp server", dir))
    return(invisible(model))
  }
  .llmr_local_consent(items, accept_license)
  accepted <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  if (need_runtime) {
    archive_rel <- file.path("runtime", plat$asset$file)
    archive <- file.path(dir, archive_rel)
    if (force || !file.exists(archive) || !identical(.llmr_sha256(archive), plat$asset$sha256)) {
      unlink(archive)
      .llmr_local_download(paste0(.llmr_local_runtime$base_url, plat$asset$file), archive,
                           plat$asset$sha256, plat$asset$size, quiet = quiet, timeout = timeout)
    }
    exdir_rel <- file.path("runtime", paste0(.llmr_local_runtime$build, "-", plat$key))
    exdir <- file.path(dir, exdir_rel)
    server <- .llmr_local_unpack(archive, exdir)
    server_rel <- file.path(exdir_rel, substring(server, nchar(exdir) + 2L))
    .llmr_local_smoke(server, plat$sys)
    # remove builds from earlier pins
    old <- setdiff(list.files(file.path(dir, "runtime"), full.names = TRUE),
                   c(exdir, archive))
    unlink(old, recursive = TRUE)
    man$runtime <- list(build = .llmr_local_runtime$build, release = .llmr_local_runtime$release,
                        commit = .llmr_local_runtime$commit, asset = plat$key,
                        gpu = plat$asset$gpu, archive = archive_rel,
                        server = server_rel,
                        license = .llmr_local_runtime$license, accepted_at = accepted)
    .llmr_local_manifest_write(man, dir)
  }

  if (need_model) {
    rel <- file.path("models", model, basename(entry$file))
    dest <- file.path(dir, rel)
    unlink(dest)
    .llmr_local_download(.llmr_hf_url(entry$repo, entry$revision, entry$file), dest,
                         entry$sha256, entry$size, quiet = quiet, timeout = timeout)
    stale <- setdiff(list.files(file.path(dir, "models", model), full.names = TRUE), dest)
    unlink(stale, recursive = TRUE)
    rec <- list(repo = entry$repo, revision = entry$revision, file = entry$file,
                sha256 = entry$sha256, size = entry$size, kind = entry$kind,
                license = entry$license, path = rel, accepted_at = accepted,
                custom = !is.null(spec))
    if (!is.null(spec)) rec$spec <- entry
    man$models[[model]] <- rec
    .llmr_local_manifest_write(man, dir)
  }
  if (!quiet) message(sprintf("Installed%s in %s.", if (is.null(model)) "" else paste0(" ", model), dir))
  invisible(model)
}

#' Is a local model ready to use?
#'
#' Reports, without any network request, whether the llama.cpp server and
#' (when named) a model are installed, so code can run local-model steps only
#' where [llm_local_install()] has been run.
#'
#' @param model A model name, or `NULL` to ask about the server alone.
#' @param dir The local model directory; see [llm_local_dir()].
#' @return `TRUE` or `FALSE`.
#' @examples
#' llm_local_ready("smollm2-135m")
#' @export
llm_local_ready <- function(model = NULL, dir = llm_local_dir()) {
  man <- tryCatch(.llmr_local_manifest_read(dir), error = function(e) NULL)
  if (is.null(man) || !.llmr_local_runtime_present(man, dir)) return(FALSE)
  if (is.null(model)) return(TRUE)
  .llmr_local_model_present(man, model, dir)
}

#' Remove local models or the llama.cpp server
#'
#' Deletes installed model files, the server program, or everything in the
#' local model directory, stopping any server that uses them first.
#'
#' @param models Names of the models to remove.
#' @param runtime If `TRUE`, also remove the llama.cpp server program.
#' @param all If `TRUE`, remove every model, the server program, and the logs,
#'   and the directory itself when nothing else is left in it. Only a
#'   directory holding LLMR's manifest is touched.
#' @param dir The local model directory; see [llm_local_dir()].
#' @return The names of the removed models, invisibly.
#' @seealso [llm_local_install()]
#' @examples
#' \dontrun{
#' llm_local_uninstall("qwen3-0.6b")
#' llm_local_uninstall(all = TRUE)
#' }
#' @export
llm_local_uninstall <- function(models = character(), runtime = FALSE, all = FALSE,
                                dir = llm_local_dir()) {
  if (isTRUE(all)) {
    if (!file.exists(.llmr_local_manifest_path(dir))) {
      stop(sprintf("%s holds no LLMR manifest, so it is not a local model directory; nothing was removed.", dir),
           call. = FALSE)
    }
    llm_local_stop()
    unlink(file.path(dir, c("runtime", "models", "logs")), recursive = TRUE)
    unlink(file.path(dir, c("manifest.json", "manifest.json.tmp")))
    if (!length(list.files(dir, all.files = TRUE, no.. = TRUE))) unlink(dir, recursive = TRUE)
    message(sprintf("Removed the local models and the llama.cpp server from %s.", dir))
    return(invisible(character()))
  }
  if (!length(models) && !isTRUE(runtime)) {
    stop("Name the models to remove, set `runtime = TRUE`, or set `all = TRUE`.", call. = FALSE)
  }
  man <- .llmr_local_manifest_read(dir)
  unknown <- setdiff(models, names(man$models))
  if (length(unknown)) {
    stop(sprintf("Not installed: %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  for (m in models) {
    llm_local_stop(m)
    unlink(file.path(dir, "models", m), recursive = TRUE)
    man$models[[m]] <- NULL
  }
  if (isTRUE(runtime)) {
    llm_local_stop()
    unlink(file.path(dir, "runtime"), recursive = TRUE)
    man$runtime <- NULL
  }
  .llmr_local_manifest_write(man, dir)
  invisible(models)
}
