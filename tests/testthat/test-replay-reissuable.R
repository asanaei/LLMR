library(testthat)
library(LLMR)

# llm_request_from_log() separates a record that identifies its request
# (`complete`) from one whose rebuilt messages can be sent again (`reissuable`).

rec <- function(messages, provider = "openai", key = "messages") {
  r <- list(provider = provider, model = "m", request = list(temperature = 0))
  r$request[[key]] <- messages
  r
}

test_that("text-only records are complete and reissuable", {
  r <- llm_request_from_log(rec(list(list(role = "system", content = "Be brief."),
                                     list(role = "user", content = "Hi"))))
  expect_true(r$complete)
  expect_true(r$reissuable)
  expect_identical(r$messages, c(system = "Be brief.", user = "Hi"))
  g <- llm_request_from_log(rec(list(list(role = "user", parts = list(list(text = "Hi"))),
                                     list(role = "model", parts = list(list(text = "Hello")))),
                                provider = "gemini", key = "contents"))
  expect_true(g$reissuable)
})

test_that("an attachment keeps the hash identity but is not reissuable", {
  img <- list(list(role = "user", content = list(
    list(type = "text", text = "Describe this."),
    list(type = "image_url", image_url = list(url = "data:image/png;base64,iVBORw0KGgo=")))))
  r <- llm_request_from_log(rec(img))
  expect_true(r$complete)
  expect_false(r$reissuable)
})

test_that("tool exchanges and lossy roles are not reissuable", {
  oa <- list(list(role = "user", content = "Weather?"),
             list(role = "assistant", content = "Checking.",
                  tool_calls = list(list(id = "c1", type = "function",
                                         `function` = list(name = "w", arguments = "{}")))),
             list(role = "tool", tool_call_id = "c1", content = "Sunny"))
  expect_false(llm_request_from_log(rec(oa), on_unsupported = "quiet")$reissuable)
  an <- list(list(role = "user", content = list(
    list(type = "tool_result", tool_use_id = "t1", content = "Sunny"),
    list(type = "text", text = "Thanks"))))
  expect_false(llm_request_from_log(rec(an, provider = "anthropic"),
                                    on_unsupported = "quiet")$reissuable)
  dev <- list(list(role = "developer", content = "Rules."), list(role = "user", content = "Hi"))
  expect_false(llm_request_from_log(rec(dev))$reissuable)
})

test_that("text-only request hashes are unchanged", {
  r <- rec(list(list(role = "user", content = "Hi")))
  rr <- llm_request_from_log(r)
  expect_identical(llm_request_hash(config = rr$config, messages = rr$messages),
                   llm_request_hash(config = rr$config, messages = c(user = "Hi")))
})
