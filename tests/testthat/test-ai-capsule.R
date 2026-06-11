# The AI usage capsule (inst/ai/LLMR.md) must never drift from the real
# API: every function it mentions in call position must exist, either as an
# export of this package, an LLMR export, or a base/stats/utils function.

test_that("the AI capsule mentions only real functions", {
  path <- system.file("ai", "LLMR.md", package = "LLMR")
  expect_true(nzchar(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hits <- regmatches(txt, gregexpr(
    "(?<![$:A-Za-z0-9_.])([A-Za-z_][A-Za-z0-9_.]*)\\(", txt, perl = TRUE))[[1]]
  fns <- unique(sub("\\($", "", hits))
  known <- function(f) {
    f %in% getNamespaceExports("LLMR") ||
      f %in% getNamespaceExports("LLMR") ||
      exists(f, envir = baseenv()) ||
      f %in% getNamespaceExports("stats") ||
      f %in% getNamespaceExports("utils")
  }
  unknown <- fns[!vapply(fns, known, logical(1))]
  expect_identical(unknown, character(0))
})
