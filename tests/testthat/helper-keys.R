skip_if_no_env <- function(var) {
  testthat::skip_if(!nzchar(Sys.getenv(var)), paste("Requires", var))
}


