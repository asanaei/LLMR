test_that("diagnostics and report are S3 generics with helpful defaults", {
  expect_true(is.function(diagnostics))
  expect_true(is.function(report))

  expect_true(any(grepl('UseMethod("diagnostics")', deparse(body(diagnostics)),
                        fixed = TRUE)))
  expect_true(any(grepl('UseMethod("report")', deparse(body(report)),
                        fixed = TRUE)))

  expect_error(diagnostics(list(a = 1)),
               "No diagnostics\\(\\) method for objects of class <list>")
  expect_error(report(list(a = 1)),
               "No report\\(\\) method for objects of class <list>")
})

test_that("registered diagnostics and report methods dispatch", {
  registerS3method("diagnostics", "llmr_test_result",
                   function(x, ...) list(surface = "diagnostics", value = x$value),
                   envir = environment(diagnostics))
  registerS3method("report", "llmr_test_result",
                   function(x, ...) list(surface = "report", value = x$value),
                   envir = environment(report))

  x <- structure(list(value = 42L), class = "llmr_test_result")
  expect_identical(diagnostics(x), list(surface = "diagnostics", value = 42L))
  expect_identical(report(x), list(surface = "report", value = 42L))
})
