library(testthat)
library(LLMR)

# llm_preview() / llm_render_messages() — all offline, no API.

df <- tibble::tibble(text = c("alpha", "beta", "gamma"))

test_that("llm_render_messages renders without any API call", {
  m <- llm_render_messages(df, prompt = "Text: {text}")
  expect_length(m, 3L)
  expect_identical(m[[1]], "Text: alpha")     # bare, no system
  expect_null(names(m[[1]]))
})

test_that("llm_render_messages mirrors the shared renderer exactly", {
  ref <- LLMR:::.llm_build_messages_df(df, prompt = "Q {text}",
                                       .system_prompt = "S")
  got <- llm_render_messages(df, prompt = "Q {text}", .system_prompt = "S")
  expect_identical(got, ref)
})

test_that("llm_render_messages rows= subsets", {
  m <- llm_render_messages(df, prompt = "{text}", rows = c(1L, 3L))
  expect_length(m, 2L)
  expect_identical(m[[2]], "gamma")
})

test_that("llm_preview returns one row per input row with ok=TRUE on clean input", {
  p <- llm_preview(df, prompt = "Classify: {text}")
  expect_s3_class(p, "llmr_preview")
  expect_equal(nrow(p), 3L)
  expect_true(all(p$ok))
  expect_true(grepl("Classify: alpha", p$rendered_preview[[1]]))
  expect_equal(p$roles[[1]], "user")
})

test_that("llm_preview shows the batch plan (contiguous, deterministic)", {
  p <- llm_preview(df, prompt = "{text}", .rows_per_prompt = 2)
  expect_equal(p$rowpack_id, c(1L, 1L, 2L))
  expect_equal(p$rows_per_prompt, c(2L, 2L, 1L))
  expect_equal(p$rowpack_row, c(1L, 2L, 1L))
})

test_that("llm_preview Inf batch => single call", {
  p <- llm_preview(df, prompt = "{text}", .rows_per_prompt = Inf)
  expect_true(all(p$rowpack_id == 1L))
  expect_equal(p$rows_per_prompt, c(3L, 3L, 3L))
})

test_that("llm_preview rows_per_prompt=1 => no grouping (each its own call)", {
  p <- llm_preview(df, prompt = "{text}", .rows_per_prompt = 1)
  expect_equal(p$rowpack_id, 1:3)
  expect_equal(p$rows_per_prompt, c(1L, 1L, 1L))
})

test_that("llm_preview flags missing files (with and without batching)", {
  d <- tibble::tibble(img = "definitely-not-here.png", prompt = "Describe")
  p <- llm_preview(d, .messages = c(user = "{prompt}", file = "{img}"))
  expect_true(p$has_file[[1]])
  expect_false(isTRUE(p$file_ok[[1]]))
  expect_true(any(grepl("do not exist", p$issues[[1]])))
  expect_false(p$ok[[1]])
})

test_that("llm_preview flags file + .rows_per_prompt>1 as unsupported", {
  d <- tibble::tibble(img = "x.png", prompt = "Describe")
  p <- llm_preview(d, .messages = c(user = "{prompt}", file = "{img}"),
                   .rows_per_prompt = 2)
  expect_true(any(grepl("file/multimodal", p$issues[[1]])))
  expect_false(p$ok[[1]])
})

test_that("llm_preview file role kept as path string, no base64/IO", {
  d <- tibble::tibble(img = "~/some/where.png")
  m <- llm_render_messages(d, .messages = c(file = "{img}"))
  # not expanded, not encoded — just the glued template
  expect_identical(unname(m[[1]]["file"]), "~/some/where.png")
})

test_that("llm_preview flags embedding config + batching", {
  cfg <- llm_config("voyage", "voyage-3", embedding = TRUE)
  p <- llm_preview(df, prompt = "{text}", .config = cfg, .rows_per_prompt = 3)
  expect_true(any(grepl("embedding", p$issues[[1]])))
  expect_false(p$ok[[1]])
})

test_that("llm_preview flags .return=object + batching", {
  p <- llm_preview(df, prompt = "{text}", .return = "object", .rows_per_prompt = 2)
  expect_true(any(grepl("object", p$issues[[1]])))
})

test_that("llm_preview flags schema without structured", {
  p <- llm_preview(df, prompt = "{text}", .schema = list(type = "object"))
  expect_true(any(grepl(".schema is ignored", p$issues[[1]])))
})

test_that("llm_preview flags structured + tags conflict", {
  p <- llm_preview(df, prompt = "{text}", .structured = TRUE,
                   .tags = c("a", "b"))
  expect_true(any(grepl("takes precedence", p$issues[[1]])))
})

test_that("llm_preview truncates long previews at max_chars", {
  d <- tibble::tibble(text = strrep("x", 1000))
  p <- llm_preview(d, prompt = "{text}", max_chars = 50)
  expect_lte(nchar(p$rendered_preview[[1]]), 53)   # 50 + "..."
})

test_that("llm_preview rows= subsets the summary", {
  p <- llm_preview(df, prompt = "{text}", rows = c(1L, 3L))
  expect_equal(p$row, c(1L, 3L))
})

test_that("llm_preview errors without prompt or .messages", {
  expect_error(llm_preview(df), "prompt.*or.*messages|messages.*or.*prompt")
})

test_that("llm_preview prints a summary header", {
  p <- llm_preview(df, prompt = "{text}", .rows_per_prompt = 2)
  out <- capture.output(print(p))
  expect_true(any(grepl("llmr_preview", out)))
})
