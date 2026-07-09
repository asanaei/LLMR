library(testthat)
library(LLMR)

# Golden lock for the shared message renderer extracted in this release.
# The reference functions below are VERBATIM copies of the pre-refactor local
# closures (eval_messages_one_row / build_generative_messages) and the llm_fn
# inline assembly. Every assertion proves the shared helper produces
# byte-identical output (values AND names() AND list structure). If anyone
# changes rendering behaviour, these tests must fail.

# ---- reference: the OLD logic, frozen ------------------------------------

ref_eval_one_row <- function(row_df, tpl_vec) {
  roles <- names(tpl_vec)
  if (is.null(roles)) roles <- rep("user", length(tpl_vec))
  roles[is.na(roles) | roles == ""] <- "user"
  out <- vapply(
    unname(tpl_vec),
    function(s) as.character(glue::glue_data(row_df, s, .na = "")),
    FUN.VALUE = character(1)
  )
  names(out) <- roles
  out
}

ref_build <- function(df, prompt, .messages, .system_prompt,
                      roles_allowed = c("system", "user", "assistant", "file")) {
  n <- nrow(df)
  msgs <- vector("list", n)
  if (!is.null(.messages)) {
    stopifnot(is.character(.messages), length(.messages) > 0)
    if (is.null(names(.messages))) names(.messages) <- rep("user", length(.messages))
    bad <- setdiff(unique(names(.messages)), roles_allowed)
    if (length(bad)) stop(sprintf("Unsupported roles in .messages: %s",
                                  paste(bad, collapse = ", ")))
    for (i in seq_len(n)) {
      row_msg <- ref_eval_one_row(df[i, , drop = FALSE], .messages)
      if (!is.null(.system_prompt) && !"system" %in% names(row_msg)) {
        row_msg <- c(system = .system_prompt, row_msg)
      }
      msgs[[i]] <- row_msg
    }
  } else {
    if (is.null(prompt)) stop("Either 'prompt' or '.messages' must be provided.")
    user_txt <- glue::glue_data(df, prompt, .na = "")
    for (i in seq_len(n)) {
      if (is.null(.system_prompt)) {
        msgs[[i]] <- as.character(user_txt[i])
      } else {
        msgs[[i]] <- c(system = .system_prompt, user = as.character(user_txt[i]))
      }
    }
  }
  msgs
}

# llm_fn's inline assembly, frozen
ref_llm_fn_assembly <- function(user_txt, .system_prompt) {
  lapply(as.character(user_txt), function(txt) {
    if (is.null(.system_prompt)) txt else c(system = .system_prompt, user = txt)
  })
}

# ---- cases ----------------------------------------------------------------

df <- tibble::tibble(
  text = c("alpha", "beta", "gamma"),
  n    = c(1L, 2L, 3L),
  img  = c("a.png", "b.png", "c.png")
)

test_that("shared renderer == frozen build: prompt, no system", {
  got <- LLMR:::.llm_build_messages_df(df, prompt = "Classify: {text} ({n})")
  ref <- ref_build(df, prompt = "Classify: {text} ({n})", NULL, NULL)
  expect_identical(got, ref)
  # prompt-only, no system => BARE unnamed scalar (not c(user=))
  expect_null(names(got[[1]]))
  expect_identical(got[[1]], "Classify: alpha (1)")
})

test_that("shared renderer == frozen build: prompt + system", {
  got <- LLMR:::.llm_build_messages_df(df, prompt = "Q: {text}",
                                       .system_prompt = "Be terse.")
  ref <- ref_build(df, prompt = "Q: {text}", NULL, "Be terse.")
  expect_identical(got, ref)
  expect_identical(names(got[[2]]), c("system", "user"))
  expect_identical(unname(got[[2]]), c("Be terse.", "Q: beta"))
})

test_that("shared renderer == frozen build: .messages named roles", {
  m <- c(system = "Rate {n}.", user = "Item: {text}")
  got <- LLMR:::.llm_build_messages_df(df, .messages = m)
  ref <- ref_build(df, NULL, m, NULL)
  expect_identical(got, ref)
  expect_identical(names(got[[3]]), c("system", "user"))
})

test_that("shared renderer == frozen build: .messages with file role kept as path string", {
  m <- c(user = "{text}", file = "{img}")
  got <- LLMR:::.llm_build_messages_df(df, .messages = m)
  ref <- ref_build(df, NULL, m, NULL)
  expect_identical(got, ref)
  # file stays a glued path; NO base64, NO I/O
  expect_identical(unname(got[[1]]["file"]), "a.png")
  expect_identical(names(got[[1]]), c("user", "file"))
})

test_that("shared renderer == frozen build: unnamed .messages default to user (0.7.2 fix)", {
  m <- c("Bare template {text}")          # no names at all
  got <- LLMR:::.llm_build_messages_df(df, .messages = m)
  ref <- ref_build(df, NULL, m, NULL)
  expect_identical(got, ref)
  expect_identical(names(got[[1]]), "user")
  # the template text must NOT have become a role name
  expect_false("Bare template {text}" %in% names(got[[1]]))
})

test_that("shared renderer == frozen build: .messages + system prepended when absent", {
  m <- c(user = "{text}")
  got <- LLMR:::.llm_build_messages_df(df, .messages = m, .system_prompt = "S.")
  ref <- ref_build(df, NULL, m, "S.")
  expect_identical(got, ref)
  expect_identical(names(got[[1]]), c("system", "user"))
})

test_that("shared renderer == frozen build: explicit system in .messages not duplicated", {
  m <- c(system = "Inline sys", user = "{text}")
  got <- LLMR:::.llm_build_messages_df(df, .messages = m, .system_prompt = "OUTER")
  ref <- ref_build(df, NULL, m, "OUTER")
  expect_identical(got, ref)
  expect_identical(sum(names(got[[1]]) == "system"), 1L)
  expect_identical(unname(got[[1]]["system"]), "Inline sys")
})

test_that("shared renderer matches llm_fn inline assembly (data.frame path)", {
  user_txt <- glue::glue_data(df, "Classify: {text}", .na = "")
  fn_msgs  <- ref_llm_fn_assembly(user_txt, NULL)
  shared   <- LLMR:::.llm_build_messages_df(df, prompt = "Classify: {text}")
  expect_identical(shared, fn_msgs)

  fn_msgs_sys <- ref_llm_fn_assembly(user_txt, "S.")
  shared_sys  <- LLMR:::.llm_build_messages_df(df, prompt = "Classify: {text}",
                                               .system_prompt = "S.")
  expect_identical(shared_sys, fn_msgs_sys)
})

test_that("shared renderer: bad roles rejected with the historical message", {
  expect_error(
    LLMR:::.llm_build_messages_df(df, .messages = c(robot = "{text}")),
    "Unsupported roles in .messages", fixed = TRUE
  )
})

test_that("shared renderer: resolved braces survive (single glue pass, no re-eval)", {
  d2 <- tibble::tibble(text = "{1+1}")
  got <- LLMR:::.llm_build_messages_df(d2, prompt = "{text}")
  expect_identical(got[[1]], "{1+1}")   # literal, not "2"
})

test_that("constant prompt (no column ref) fills every row, not just the first", {
  # glue returns length 1 for a template with no {column}; rows 2..n must not be
  # NA (the provider rejects a null-content message). See .llm_glue_rows().
  d3 <- tibble::tibble(x = 1:3)
  got <- LLMR:::.llm_build_messages_df(d3, prompt = "classify this item")
  expect_length(got, 3L)
  expect_identical(unlist(got), rep("classify this item", 3L))
  expect_false(anyNA(unlist(got)))

  # with a system prompt the user turn is still filled on every row
  got2 <- LLMR:::.llm_build_messages_df(d3, prompt = "constant",
                                        .system_prompt = "be terse")
  expect_identical(vapply(got2, function(m) unname(m["user"]), character(1)),
                   rep("constant", 3L))
})

test_that(".llm_glue_rows recycles length-1, passes length-n through, respects n", {
  glr <- LLMR:::.llm_glue_rows
  expect_identical(glr(data.frame(x = 1:3), "hi", 3L), rep("hi", 3L))          # constant
  expect_identical(glr(data.frame(x = c("a", "b")), "{x}", 2L), c("a", "b"))   # column ref
  expect_identical(glr(list(x = c("p", "q", "r")), "hi", 3L), rep("hi", 3L))   # bare vector
  expect_identical(glr(data.frame(x = 1), "hi", 1L), "hi")                     # single row
  expect_length(glr(data.frame(x = integer(0)), "hi", 0L), 0L)                 # zero rows
})
