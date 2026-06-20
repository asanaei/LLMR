# hash.R ------------------------------------------------------------------------
# The ecosystem's content hash. Downstream packages (LLMRcontent protocols,
# LLMRcontent archives) use these hashes as scientific identifiers -- a value
# cited in a paper must be reproducible on any machine, in any R version,
# forever. Hence: canonical form (classes stripped, named lists sorted,
# functions deparsed), canonical JSON, SHA-256 over the UTF-8 bytes of that
# string -- never over R's serialization of it, which varies by R version.

# Internal: canonical form. Classes are stripped recursively; named lists are
# sorted by name (construction order must not change the hash); functions
# hash by their deparsed source.
.llmr_canonical <- function(x) {
  if (is.function(x)) return(paste(deparse(x), collapse = "\n"))
  if (is.list(x)) {
    x <- unclass(x)
    nm <- names(x)
    if (!is.null(nm) && all(nzchar(nm))) x <- x[order(nm)]
    return(lapply(x, .llmr_canonical))
  }
  x
}

#' Content hash for research artifacts
#'
#' One hash convention for the whole LLMR ecosystem: prompts, codebooks,
#' coding protocols, archived requests. The object is reduced to a canonical
#' form (classes stripped, named lists sorted by name, functions replaced by
#' their deparsed source), rendered as canonical JSON, and hashed with
#' SHA-256 over the UTF-8 bytes of that string. Two consequences worth
#' stating: equal content hashes equally regardless of construction order or
#' S3 class, and the hash does not depend on R's serialization format, so a
#' value recorded in a paper today is checkable on any machine later.
#'
#' Downstream packages treat these hashes as identifiers of record
#' (`LLMRcontent::protocol_lock()`, `LLMRcontent::archive_build()`); the
#' convention is versioned by this function's documentation -- any future
#' change would be a new function, not a silent edit.
#'
#' @param x An R object: list, character, config, codebook -- anything whose
#'   canonical JSON form is well defined. Environments are not hashable.
#' @return A 64-character lowercase SHA-256 hex string.
#' @examples
#' llm_hash(list(model = "gpt-oss-20b", temperature = 0))
#' # construction order does not matter:
#' identical(llm_hash(list(a = 1, b = 2)), llm_hash(list(b = 2, a = 1)))
#' # any content change does:
#' identical(llm_hash(list(a = 1)), llm_hash(list(a = 2)))
#' @export
llm_hash <- function(x) {
  canon <- jsonlite::toJSON(.llmr_canonical(x), auto_unbox = TRUE,
                            null = "null", digits = NA)
  digest::digest(as.character(canon), algo = "sha256", serialize = FALSE)
}

# A per-session monotone counter for llm_uuid().
.llmr_uuid_state <- new.env(parent = emptyenv())
.llmr_uuid_state$n <- 0L

#' A short, sortable, process-unique identifier
#'
#' Mints a compact id without a UUID dependency, useful for tagging runs, spans,
#' or records. The id sorts in creation order within a session: it is a base-36
#' timestamp, the process id, and a monotone counter, joined by `-` (with an
#' optional prefix). It is unique within a process and practically unique across
#' processes on a host; it is not a registered UUID and makes no cross-host
#' global-uniqueness guarantee.
#'
#' @param prefix Optional short string prepended as `prefix-...`.
#' @return A character scalar.
#' @examples
#' llm_uuid()
#' llm_uuid("run")
#' @export
llm_uuid <- function(prefix = NULL) {
  .llmr_uuid_state$n <- .llmr_uuid_state$n + 1L
  ts <- format(as.numeric(Sys.time()) * 1000, scientific = FALSE)
  b36 <- function(n) {
    n <- floor(as.numeric(n))
    if (!is.finite(n) || n < 1) return("0")
    digs <- c(0:9, letters); out <- character(0)
    while (n >= 1) { out <- c(digs[(n %% 36) + 1], out); n <- floor(n / 36) }
    paste(out, collapse = "")
  }
  id <- paste(b36(ts), Sys.getpid(), .llmr_uuid_state$n, sep = "-")
  if (!is.null(prefix) && nzchar(prefix)) paste(prefix, id, sep = "-") else id
}
