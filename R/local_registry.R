# local_registry.R --------------------------------------------------------------
# The pinned llama.cpp build and the registry of models that llm_local_install()
# knows by name. Every download is pinned to an exact file: the llama.cpp
# release asset by its GitHub digest, each model by its Hugging Face repository,
# commit, file name, size, and sha256. The values below were read from the
# GitHub and Hugging Face APIs; changing a pin means re-reading all of them and
# re-validating the models against the new build.

.llmr_local_runtime <- list(
  build   = "b11669",
  release = "v0.7.0",
  commit  = "620d2225cc436ab0b001e3200e590e3b286ef9ab",
  license = "MIT",
  license_url = "https://github.com/ggml-org/llama.cpp/blob/master/LICENSE",
  base_url = "https://github.com/ggml-org/llama.cpp/releases/download/b11669/",
  assets = list(
    "macos-arm64"         = list(file = "llama-b11669-bin-macos-arm64.tar.gz", size = 12191582,
                                 sha256 = "dbed55493d72bc1ba2babc8770d669c2289b5bc4ee9fb2a4dfc158d45b7d13fd",
                                 gpu = "metal"),
    "macos-x64"           = list(file = "llama-b11669-bin-macos-x64.tar.gz", size = 11677404,
                                 sha256 = "cab28238d78158109b48deb27b05d50e0dcef56275d2e76da9ca8281526c7532",
                                 gpu = "none"),
    "ubuntu-x64"          = list(file = "llama-b11669-bin-ubuntu-x64.tar.gz", size = 17851611,
                                 sha256 = "cf07c1c112d709bd67e522017471de57ed0ec8bea031d732c5422351c07f65ac",
                                 gpu = "none"),
    "ubuntu-arm64"        = list(file = "llama-b11669-bin-ubuntu-arm64.tar.gz", size = 13900160,
                                 sha256 = "9ac6fda2058d35677608a06f751a50122ddfa4ce0ddb7f084466b8e79b6d4a99",
                                 gpu = "none"),
    "win-cpu-x64"         = list(file = "llama-b11669-bin-win-cpu-x64.zip", size = 19537886,
                                 sha256 = "37b3efcf962847ca869745fd4ccf7f5e7e8dacfeff9fa3523b4ca8cdc66d632b",
                                 gpu = "none"),
    "win-cpu-arm64"       = list(file = "llama-b11669-bin-win-cpu-arm64.zip", size = 12442177,
                                 sha256 = "9cff39ffed17edcd03a7d876073f74d7b48cd43a1e5eafdd59ee094185ad871b",
                                 gpu = "none"),
    "ubuntu-vulkan-x64"   = list(file = "llama-b11669-bin-ubuntu-vulkan-x64.tar.gz", size = 30497889,
                                 sha256 = "e8709e5ba6c1b093dea7f3de57f4aba842f9a188fba6b465782d65d6fcfa4d9f",
                                 gpu = "vulkan"),
    "ubuntu-vulkan-arm64" = list(file = "llama-b11669-bin-ubuntu-vulkan-arm64.tar.gz", size = 24187561,
                                 sha256 = "d283522e479c0d532e3a04d715dd852ffb2d95925caa6d41ef2ba53a65e46a51",
                                 gpu = "vulkan"),
    "win-vulkan-x64"      = list(file = "llama-b11669-bin-win-vulkan-x64.zip", size = 32182350,
                                 sha256 = "2c07574e7bde71321cb785282b20cc821e951132149eeceeea32ddebdbf0598e",
                                 gpu = "vulkan"),
    "win-vulkan-arm64"    = list(file = "llama-b11669-bin-win-vulkan-arm64.zip", size = 24817965,
                                 sha256 = "963d3b185362cadca3de643409f916a5ce95dd0f7fd8617ba9ee5d8833ac1ef3",
                                 gpu = "vulkan")
  )
)

# EmbeddingGemma 2 task instructions, from its model card: queries and
# symmetric tasks take a "task: ... | query: " prefix; documents take
# "title: none | text: " when they have no title.
.llmr_embeddinggemma_prefixes <- c(
  similarity         = "task: sentence similarity | query: ",
  search_query       = "task: search result | query: ",
  question_answering = "task: question answering | query: ",
  fact_checking      = "task: fact checking | query: ",
  code_retrieval     = "task: code retrieval | query: ",
  classification     = "task: classification | query: ",
  clustering         = "task: clustering | query: ",
  document           = "title: none | text: "
)

.llmr_local_registry_list <- function() {
  list(
    "qwen3-0.6b" = list(
      kind = "chat", base = FALSE, ctx = 4096L,
      repo = "Qwen/Qwen3-0.6B-GGUF", revision = "23749fefcc72300e3a2ad315e1317431b06b590a",
      file = "Qwen3-0.6B-Q8_0.gguf", size = 639446688,
      sha256 = "9465e63a22add5354d9bb4b99e90117043c7124007664907259bd16d043bb031",
      license = "Apache-2.0", license_url = "https://huggingface.co/Qwen/Qwen3-0.6B/blob/main/LICENSE",
      card_url = "https://huggingface.co/Qwen/Qwen3-0.6B",
      languages = "119 languages, Persian and Arabic among them",
      note = "Qwen's official 8-bit conversion. A reasoning model: chat calls think first unless the template is told not to."
    ),
    "qwen3.5-0.8b" = list(
      kind = "chat", base = FALSE, ctx = 4096L,
      repo = "ggml-org/Qwen3.5-0.8B-GGUF", revision = "8fea620810c4afa23dd6443f999a48574c1611a3",
      file = "Qwen3.5-0.8B-Q8_0.gguf", size = 833592096,
      sha256 = "37ae482d336108d23516fa35e8e0c4126688d81018b87178a18d752a1357814f",
      license = "Apache-2.0", license_url = "https://huggingface.co/Qwen/Qwen3.5-0.8B",
      card_url = "https://huggingface.co/Qwen/Qwen3.5-0.8B",
      languages = "201 languages and dialects; markedly better Persian than qwen3-0.6b",
      note = "llama.cpp's 8-bit conversion. A hybrid recurrent model, so its numbers are reproducible only without prompt caching, which the research functions never use."
    ),
    "qwen3.5-0.8b-q4" = list(
      kind = "chat", base = FALSE, ctx = 4096L,
      repo = "ggml-org/Qwen3.5-0.8B-GGUF", revision = "8fea620810c4afa23dd6443f999a48574c1611a3",
      file = "Qwen3.5-0.8B-Q4_0.gguf", size = 563036064,
      sha256 = "57d1997790d1744fba5b40a7317df71ea5e2acee28c47e78f0cce39c0703f8cf",
      license = "Apache-2.0", license_url = "https://huggingface.co/Qwen/Qwen3.5-0.8B",
      card_url = "https://huggingface.co/Qwen/Qwen3.5-0.8B",
      languages = "201 languages and dialects; markedly better Persian than qwen3-0.6b",
      note = "The 4-bit version of qwen3.5-0.8b, smaller at some cost in fidelity to the full model. A hybrid recurrent model, reproducible only without prompt caching, which the research functions never use."
    ),
    "qwen3.5-0.8b-base" = list(
      kind = "chat", base = TRUE, ctx = 4096L,
      repo = "ggml-org/Qwen3.5-0.8B-Base-GGUF", revision = "f78a2c518573cedd5585e5e368a6e1844d2ebbda",
      file = "Qwen3.5-0.8B-Base-Q8_0.gguf", size = 833591872,
      sha256 = "02fbcf13dd07601e5801cdef73ace47426c422742fa68fd0c607a8b88eea0790",
      license = "Apache-2.0", license_url = "https://huggingface.co/Qwen/Qwen3.5-0.8B-Base",
      card_url = "https://huggingface.co/Qwen/Qwen3.5-0.8B-Base",
      languages = "201 languages and dialects",
      note = "Pretrained base model without instruction tuning; use `prompt =` rather than chat messages."
    ),
    "granite-4.0-350m" = list(
      kind = "chat", base = FALSE, ctx = 4096L,
      repo = "ibm-granite/granite-4.0-350m-GGUF", revision = "b8208a86a58427e1739265318028eb5895b74bf2",
      file = "granite-4.0-350m-Q8_0.gguf", size = 378138016,
      sha256 = "9595dafb4ed15aa02512c8ea26188744192a6f09530eae0a2747bd3ada96cd36",
      license = "Apache-2.0", license_url = "https://huggingface.co/ibm-granite/granite-4.0-350m",
      card_url = "https://huggingface.co/ibm-granite/granite-4.0-350m",
      languages = "12 languages, Arabic among them; no Persian",
      note = "IBM's official conversion; pairs with granite-4.0-350m-base."
    ),
    "granite-4.0-350m-base" = list(
      kind = "chat", base = TRUE, ctx = 4096L,
      repo = "ibm-granite/granite-4.0-350m-base-GGUF", revision = "87a04d7f2076c11968a34bc67ead869f522b1c39",
      file = "granite-4.0-350m-base-Q8_0.gguf", size = 378131328,
      sha256 = "6179b72f88991de701d11346c3598c55a33bc8a38e95e6d8af49f27aac563777",
      license = "Apache-2.0", license_url = "https://huggingface.co/ibm-granite/granite-4.0-350m-base",
      card_url = "https://huggingface.co/ibm-granite/granite-4.0-350m-base",
      languages = "12 languages, Arabic among them; no Persian",
      note = "Pretrained base model of granite-4.0-350m; use `prompt =` rather than chat messages."
    ),
    "smollm2-135m" = list(
      kind = "chat", base = FALSE, ctx = 2048L,
      repo = "bartowski/SmolLM2-135M-Instruct-GGUF", revision = "09816acd5d99df7be770d85ea30822623dab342c",
      file = "SmolLM2-135M-Instruct-Q8_0.gguf", size = 144811360,
      sha256 = "5a1395716f7913741cc51d98581b9b1228d80987a9f7d3664106742eb06bba83",
      license = "Apache-2.0", license_url = "https://huggingface.co/HuggingFaceTB/SmolLM2-135M-Instruct",
      card_url = "https://huggingface.co/HuggingFaceTB/SmolLM2-135M-Instruct",
      languages = "English",
      note = "A very small model for trying the functions and for examples; a community conversion of Hugging Face's SmolLM2."
    ),
    "smollm2-360m" = list(
      kind = "chat", base = FALSE, ctx = 2048L,
      repo = "HuggingFaceTB/SmolLM2-360M-Instruct-GGUF", revision = "593b5a2e04c8f3e4ee880263f93e0bd2901ad47f",
      file = "smollm2-360m-instruct-q8_0.gguf", size = 386404992,
      sha256 = "48ab3034d0dd401fbc721eb1df3217902fee7dab9078992d66431f09b7750201",
      license = "Apache-2.0", license_url = "https://huggingface.co/HuggingFaceTB/SmolLM2-360M-Instruct",
      card_url = "https://huggingface.co/HuggingFaceTB/SmolLM2-360M-Instruct",
      languages = "English",
      note = "Hugging Face's official 8-bit conversion; a small English model."
    ),
    "multilingual-e5-small" = list(
      kind = "embedding", base = NA, ctx = 512L, parallel = 4L,
      repo = "TwinSunsLLC/multilingual-e5-small-gguf", revision = "b6cac9615d4ecce28d7f22539b7322d695fc2886",
      file = "multilingual-e5-small-q8_0.gguf", size = 132439008,
      sha256 = "e011debc1208e31bf7b6aebee2d9fc8bd2ca11694a77ed66ac9d0c9d0a877c93",
      license = "MIT", license_url = "https://huggingface.co/intfloat/multilingual-e5-small",
      card_url = "https://huggingface.co/intfloat/multilingual-e5-small",
      languages = "94 languages, Persian and Arabic among them",
      dims = 384L, mrl = integer(),
      prefixes = c(query = "query: ", passage = "passage: "), default_task = "query",
      note = "The smallest multilingual model here (384 dimensions, 512-token inputs); a community conversion of intfloat's model. Trained with prefixes: \"query\" (the default, also for non-retrieval tasks) and \"passage\" for documents."
    ),
    "granite-embedding-311m" = list(
      kind = "embedding", base = NA, ctx = 8192L, parallel = 4L,
      repo = "mykor/granite-embedding-311m-multilingual-r2-GGUF", revision = "4413a1d4c63ed0aeda030202ad982a613a91f9ea",
      file = "granite-embedding-311M-multilingual-r2-Q8_0.gguf", size = 347018080,
      sha256 = "960116d274ff796e65713a984c4d3de52999795512c64a02bb162e231fa1f3f8",
      license = "Apache-2.0", license_url = "https://huggingface.co/ibm-granite/granite-embedding-311m-multilingual-r2",
      card_url = "https://huggingface.co/ibm-granite/granite-embedding-311m-multilingual-r2",
      languages = "52 languages, Persian and Arabic among them",
      dims = 768L, mrl = c(512L, 384L, 256L, 128L), prefixes = character(), default_task = NA_character_,
      note = "Needs no task prefix; a community conversion of IBM's model."
    ),
    "bge-m3" = list(
      kind = "embedding", base = NA, ctx = 8192L, parallel = 4L,
      repo = "ggml-org/bge-m3-Q8_0-GGUF", revision = "9eba04c5d75ba5a1595e45de734d36bef4e5cb98",
      file = "bge-m3-q8_0.gguf", size = 634553760,
      sha256 = "aa473d51f451a22f0fcf39ba3330c14bed38a385712b1113440f69df4047a173",
      license = "MIT", license_url = "https://huggingface.co/BAAI/bge-m3",
      card_url = "https://huggingface.co/BAAI/bge-m3",
      languages = "over 100 languages; strong published results for Persian and Arabic",
      dims = 1024L, mrl = integer(), prefixes = character(), default_task = NA_character_,
      note = "Needs no task prefix. 1024 dimensions."
    ),
    "embeddinggemma-2" = list(
      kind = "embedding", base = NA, ctx = 8192L, parallel = 4L,
      repo = "ggml-org/embeddinggemma-2-GGUF", revision = "bfcd298762cc34d0357ece5ebdd31791a3a374d8",
      file = "embeddinggemma-2-Q8_0.gguf", size = 309855456,
      sha256 = "2188ac1deca4b77dffefd603c2776a9d76d9d74ec01841392982ebb840b09135",
      license = "Apache-2.0, with Google's Gemma Prohibited Use Policy",
      license_url = "https://ai.google.dev/gemma/prohibited_use_policy",
      card_url = "https://huggingface.co/google/embeddinggemma-2",
      languages = "over 100 languages",
      dims = 768L, mrl = c(512L, 256L, 128L), prefixes = .llmr_embeddinggemma_prefixes,
      default_task = "similarity",
      note = "Smaller and faster than bge-m3. Trained with task prefixes, which LLMR adds from `task =` (default \"similarity\")."
    )
  )
}

# Registry entries joined with the custom models recorded in the manifest.
.llmr_local_entries <- function(dir = llm_local_dir()) {
  reg <- .llmr_local_registry_list()
  man <- .llmr_local_manifest_read(dir)
  custom <- Filter(function(m) isTRUE(m$custom), man$models %||% list())
  for (nm in names(custom)) {
    if (!nm %in% names(reg)) reg[[nm]] <- custom[[nm]]$spec
  }
  reg
}

# The entry for a model name: the pinned registry first, then custom models
# recorded in the manifest. An unreadable manifest is an error, not a silent
# fallback, since a model's prefixes and kind come from its entry. NULL for a
# name that is in neither.
.llmr_local_lookup <- function(model, dir = llm_local_dir()) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) return(NULL)
  reg <- .llmr_local_registry_list()
  if (model %in% names(reg)) return(reg[[model]])
  .llmr_local_entries(dir)[[model]]
}

.llmr_local_entry <- function(model, dir = llm_local_dir()) {
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    stop("`model` must be one model name; see llm_local_models().", call. = FALSE)
  }
  reg <- .llmr_local_entries(dir)
  e <- reg[[model]]
  if (is.null(e)) {
    stop(sprintf("Unknown local model \"%s\". Known models: %s. A model from Hugging Face can be added with llm_local_install(llm_local_model(...)).",
                 model, paste(names(reg), collapse = ", ")), call. = FALSE)
  }
  e$name <- model
  e
}

#' Models available for local installation
#'
#' Lists the models that [llm_local_install()] can download by name, with
#' their kind (`"chat"` or `"embedding"`), size, license, language coverage,
#' and whether they are installed. Every entry is pinned to one file at one
#' commit of its Hugging Face repository and verified by its sha256 after
#' download. Models added with [llm_local_model()] appear as well once
#' installed.
#'
#' @param dir The local model directory; see [llm_local_dir()].
#' @return A tibble with columns `name`, `kind`, `base` (`TRUE` for a
#'   pretrained model without instruction tuning), `size_mb`, `license`,
#'   `languages`, `installed`, `source` (repository and commit), and `note`.
#' @seealso [llm_local_install()], [llm_local_status()]
#' @examples
#' llm_local_models()
#' @export
llm_local_models <- function(dir = llm_local_dir()) {
  reg <- .llmr_local_entries(dir)
  man <- .llmr_local_manifest_read(dir)
  installed <- vapply(names(reg), function(nm) .llmr_local_model_present(man, nm, dir), logical(1))
  tibble::tibble(
    name      = names(reg),
    kind      = vapply(reg, function(e) e$kind, ""),
    base      = vapply(reg, function(e) as.logical(e$base %||% NA), NA),
    size_mb   = round(vapply(reg, function(e) as.numeric(e$size), 0) / 1e6),
    license   = vapply(reg, function(e) e$license %||% NA_character_, ""),
    languages = vapply(reg, function(e) e$languages %||% NA_character_, ""),
    installed = unname(installed),
    source    = vapply(reg, function(e) sprintf("%s@%s", e$repo, substr(e$revision, 1, 8)), ""),
    note      = vapply(reg, function(e) e$note %||% NA_character_, "")
  )
}

#' Describe a model from Hugging Face for local installation
#'
#' Builds the specification that lets [llm_local_install()] download any GGUF
#' model file from a Hugging Face repository, in addition to the models it
#' knows by name ([llm_local_models()]). The file is pinned to a commit and
#' verified by its sha256; when `revision` or `sha256` is not supplied, both
#' are read from the Hugging Face API at install time and then recorded, so
#' later sessions use exactly the same file.
#'
#' @param repo Hugging Face repository, such as `"Qwen/Qwen3-1.7B-GGUF"`.
#' @param file Name of the `.gguf` file within the repository.
#' @param name Name to refer to the model by in [llm_config()]; by default
#'   the file name without its extension, in lower case.
#' @param kind `"chat"` for a generative model or `"embedding"`.
#' @param revision A commit hash, or a branch name such as `"main"`, which is
#'   resolved to its current commit at install time.
#' @param sha256,size The expected sha256 and size in bytes of the file;
#'   read from Hugging Face when `NULL`.
#' @param license License name shown before download; read from the
#'   repository's metadata when `NULL`.
#' @param ctx Context length to start the server with.
#' @param prefixes For embedding models, a named character vector mapping
#'   task names to the prefix the model expects (see the model's card).
#' @param default_task The task used when [llm_config()] names none.
#' @param base `TRUE` for a pretrained model without instruction tuning.
#' @return An `llmr_local_model` specification.
#' @seealso [llm_local_install()]
#' @examples
#' spec <- llm_local_model("Qwen/Qwen3-1.7B-GGUF", "Qwen3-1.7B-Q8_0.gguf")
#' spec$name
#' @export
llm_local_model <- function(repo, file, name = NULL, kind = c("chat", "embedding"),
                            revision = "main", sha256 = NULL, size = NULL,
                            license = NULL, ctx = NULL, prefixes = NULL,
                            default_task = NULL, base = FALSE) {
  kind <- match.arg(kind)
  stopifnot(is.character(repo), length(repo) == 1L, grepl("^[^/]+/[^/]+$", repo),
            is.character(file), length(file) == 1L, grepl("[.]gguf$", file, ignore.case = TRUE))
  parts <- strsplit(file, "/", fixed = TRUE)[[1]]
  if (startsWith(file, "/") || any(parts %in% c("", ".", ".."))) {
    stop("`file` must be a path inside the repository, without '.' or '..' components.", call. = FALSE)
  }
  if (is.null(name)) name <- tolower(sub("[.]gguf$", "", basename(file), ignore.case = TRUE))
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", name)) {
    stop("`name` must start with a letter or digit and contain only letters, digits, '.', '_', and '-'.",
         call. = FALSE)
  }
  if (!is.null(sha256) && !grepl("^[0-9a-f]{64}$", sha256)) {
    stop("`sha256` must be 64 lower-case hexadecimal characters.", call. = FALSE)
  }
  if (!is.null(prefixes) && (!is.character(prefixes) || is.null(names(prefixes)))) {
    stop("`prefixes` must be a named character vector, task = prefix.", call. = FALSE)
  }
  structure(list(
    name = name, kind = kind, base = if (kind == "chat") isTRUE(base) else NA,
    ctx = as.integer(ctx %||% if (kind == "embedding") 8192L else 4096L),
    parallel = if (kind == "embedding") 4L else NULL,
    repo = repo, revision = revision, file = file, size = size, sha256 = sha256,
    license = license, license_url = sprintf("https://huggingface.co/%s", repo),
    card_url = sprintf("https://huggingface.co/%s", repo),
    languages = NA_character_,
    prefixes = prefixes %||% character(), default_task = default_task %||% NA_character_,
    dims = NA_integer_, mrl = integer(),
    note = "Added with llm_local_model().", custom = TRUE
  ), class = "llmr_local_model")
}

#' @export
print.llmr_local_model <- function(x, ...) {
  cat(sprintf("<llmr_local_model %s: %s/%s (%s)>\n", x$name, x$repo, x$file, x$kind))
  invisible(x)
}
