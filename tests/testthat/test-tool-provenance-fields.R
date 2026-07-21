library(testthat)
library(LLMR)

test_that("tool-loop provenance is exposed as fields with attribute mirrors", {
  response <- function(text, sent, rec, tool_call = FALSE) {
    raw <- if (tool_call) {
      list(choices = list(list(message = list(
        role = "assistant",
        content = NULL,
        tool_calls = list(list(
          id = "call-1",
          type = "function",
          `function` = list(name = "double", arguments = '{"x":2}')
        ))
      ))))
    } else {
      list(choices = list(list(message = list(
        role = "assistant", content = text
      ))))
    }
    LLMR:::new_llmr_response(
      text = text,
      provider = "openai",
      model = "test-model",
      finish_reason = if (tool_call) "tool" else "stop",
      usage = list(sent = sent, rec = rec, total = sent + rec,
                   reasoning = NA_integer_, cached = NA_integer_),
      raw = raw
    )
  }

  calls <- 0L
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) response("", 10L, 2L, tool_call = TRUE)
      else response("4", 5L, 1L)
    },
    .package = "LLMR"
  )

  tool <- llm_tool(
    function(x) as.numeric(x) * 2,
    name = "double",
    description = "Double a number.",
    parameters = list(x = list(type = "number"))
  )
  result <- call_llm_tools(llm_config("openai", "test-model"),
                           "Double 2.", tools = tool)

  expect_true(all(c("messages", "tool_history", "tool_loop") %in% names(result)))
  expect_identical(result$messages, attr(result, "messages"))
  expect_identical(result$tool_history, attr(result, "tool_history"))
  expect_identical(result$tool_loop, attr(result, "tool_loop"))
  expect_identical(nrow(result$tool_history), 1L)
  expect_identical(result$tool_loop$model_calls, 2L)
  expect_identical(result$tool_loop$tool_calls, 1L)
})
