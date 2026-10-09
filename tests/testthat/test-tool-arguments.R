library(testthat)
library(LLMR)

# A tool call whose arguments do not parse, or that comes from a reply cut off
# by the output-token limit, is answered with an error instead of being run.

openai_resp <- function(arguments = NULL, finish = "tool", text = "",
                        tool = "purge_rows") {
  raw <- if (is.null(arguments)) {
    list(choices = list(list(message = list(role = "assistant", content = text))))
  } else {
    list(choices = list(list(message = list(
      role = "assistant", content = NULL,
      tool_calls = list(list(id = "c1", type = "function",
                             `function` = list(name = tool, arguments = arguments)))))))
  }
  structure(list(
    text = text, provider = "openai", model = "m", finish_reason = finish,
    usage = list(sent = 5L, rec = 2L, total = 7L,
                 reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01, raw = raw, raw_json = "{}"
  ), class = "llmr_response")
}

# A tool with defaults that would do the wrong thing if run without arguments.
make_purge <- function(ran) {
  llm_tool(function(table = "main", n = 100) {
    ran$calls <- c(ran$calls, paste(table, n))
    "done"
  }, name = "purge_rows", description = "Deletes rows",
  parameters = list(table = list(type = "string"), n = list(type = "number")))
}

run_loop <- function(first, tool) {
  sent <- list()
  k <- 0L
  testthat::local_mocked_bindings(
    call_llm_robust = function(cfg, convo, ...) {
      k <<- k + 1L
      sent[[k]] <<- convo
      if (k == 1L) first else openai_resp(text = "All set.", finish = "stop")
    },
    .package = "LLMR"
  )
  r <- call_llm_tools(llm_config("openai", "m"), "clean up", tools = tool)
  list(resp = r, sent = sent)
}

test_that("tool_calls() flags arguments that are not a JSON object", {
  cl <- tool_calls(openai_resp('{"table": "scratch", "n": 5'))[[1]]
  expect_false(cl$arguments_ok)
  expect_identical(cl$arguments_raw, '{"table": "scratch", "n": 5')
  expect_length(cl$arguments, 0L)

  expect_false(tool_calls(openai_resp("[1, 2]"))[[1]]$arguments_ok)

  cl <- tool_calls(openai_resp('{"table": "scratch", "n": 5}'))[[1]]
  expect_true(cl$arguments_ok)
  expect_identical(cl$arguments, list(table = "scratch", n = 5L))
})

test_that("empty, null, and absent arguments mean no arguments", {
  for (a in list("", "null", "{}")) {
    cl <- tool_calls(openai_resp(a))[[1]]
    expect_true(cl$arguments_ok)
    expect_length(cl$arguments, 0L)
  }
})

test_that("a call with unparseable arguments is not run", {
  ran <- new.env()
  out <- run_loop(openai_resp('{"table": "scratch", "n": 5'), make_purge(ran))
  expect_null(ran$calls)
  expect_identical(out$resp$tool_loop$tool_calls, 0L)
  expect_identical(out$resp$tool_history$arguments, '{"table": "scratch", "n": 5')
  tool_msg <- Filter(function(m) identical(m$role, "tool"), out$sent[[2]])[[1]]
  expect_match(tool_msg$content, "not valid JSON, so the tool was not run")
  expect_identical(tool_msg$tool_call_id, "c1")
})

test_that("calls in a reply cut off by the token limit are not run", {
  ran <- new.env()
  out <- run_loop(openai_resp('{"table": "scratch"}', finish = "length"), make_purge(ran))
  expect_null(ran$calls)
  expect_match(out$resp$tool_history$result, "cut off by the output-token limit")
})

test_that("valid calls and calls without arguments still run", {
  ran <- new.env()
  out <- run_loop(openai_resp('{"table": "scratch", "n": 5}'), make_purge(ran))
  expect_identical(ran$calls, "scratch 5")
  expect_identical(out$resp$tool_loop$tool_calls, 1L)
  expect_identical(out$resp$tool_history$arguments, '{"table":"scratch","n":5}')

  ran <- new.env()
  run_loop(openai_resp(""), make_purge(ran))
  expect_identical(ran$calls, "main 100")
})
