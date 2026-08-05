# hash.R ------------------------------------------------------------------------
# The ecosystem's content hash. Downstream packages (LLMRcontent protocols,
# LLMRcontent archives) use these hashes as scientific identifiers -- a value
# cited in a paper must be reproducible wherever the canonical form below
# is implemented. Hence: canonical form (list classes stripped, named lists sorted,
# functions deparsed), canonical JSON, SHA-256 over the UTF-8 bytes of that
# string -- never over R's serialization of it, which varies by R version.

# Internal: canonical form. List classes are stripped recursively; named lists
# are sorted by name (construction order must not change the hash); functions
# hash by their deparsed source. Atomic vectors keep their class and hash by
# their canonical JSON rendering (a Date hashes as its date string, not as the
# underlying integer) -- changing that now would silently re-key every archive. The sort uses method = "radix", which is
# locale-independent (unlike the default, which follows LC_COLLATE); this is
# what makes the hash identical across machines. Radix order coincides with the
# C-locale ordering, so hashes recorded under the C locale are unchanged.
.llmr_canonical <- function(x) {
  if (is.function(x)) return(paste(deparse(x), collapse = "\n"))
  if (is.list(x)) {
    x <- unclass(x)
    nm <- names(x)
    if (!is.null(nm) && all(nzchar(nm))) x <- x[order(nm, method = "radix")]
    return(lapply(x, .llmr_canonical))
  }
  x
}

#' Content hash for research artifacts
#'
#' One hash convention for the whole LLMR ecosystem: prompts, codebooks,
#' coding protocols, archived requests. The object is reduced to a canonical
#' form (list classes stripped, named lists sorted by name, functions replaced
#' by their deparsed source), rendered as canonical JSON, and hashed with
#' SHA-256 over the UTF-8 bytes of that string. Two consequences worth
#' stating: equal content hashes equally regardless of construction order or
#' list class, and the hash does not depend on R's serialization format, so a
#' value recorded in a paper today is checkable later under this same
#' canonicalization (call it hash schema v1: canonical JSON, SHA-256 over its
#' UTF-8 bytes). Atomic
#' vectors hash by their canonical JSON rendering, class included: a `Date`
#' hashes as its date string, not as its unclassed integer.
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

# A per-session monotone counter for internal identifiers.
.llmr_uuid_state <- new.env(parent = emptyenv())
.llmr_uuid_state$n <- 0L

# Internal short, sortable, process-unique identifier.
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
