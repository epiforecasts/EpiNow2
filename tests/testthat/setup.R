library("data.table")
library("lifecycle")

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  files <- c(
    "convolve.stan", "gaussian_process.stan", "pmfs.stan",
    "observation_model.stan", "secondary.stan",
    "rt.stan", "infections.stan", "delays.stan", "generated_quantities.stan", 
    "helpers.stan"
  )
  if (!(tolower(Sys.info()[["sysname"]]) %in% "windows")) {
    suppressMessages(
      expose_stan_fns(files,
        target_dir = system.file("stan/functions", package = "EpiNow2")
      )
    )
  }
}

if (requireNamespace("future", quietly = TRUE)) {
  withr::defer(future::plan("sequential"), teardown_env())
}

# Disable progressr output during tests
if (requireNamespace("progressr", quietly = TRUE)) {
  # Use void handler for silent progress (no output at all)
  progressr::handlers("void")
}

# Test categorisation helpers -----------------------------------------------

#' Check if integration tests should be run
#'
#' Integration tests are slow MCMC-based tests. By default, these are skipped
#' to speed up test runs. Set EPINOW2_SKIP_INTEGRATION=false to run them.
#'
#' @return Logical indicating whether to run integration tests
integration_test <- function() {
  skip_integration <- Sys.getenv("EPINOW2_SKIP_INTEGRATION", "true")
  !isTRUE(as.logical(skip_integration))
}

#' Check if full test suite should be run
#'
#' Full tests include all integration tests and are typically run on a schedule
#' rather than on every commit. Set EPINOW2_FULL_TESTS=true to run them.
#'
#' @return Logical indicating whether to run full test suite
full_tests <- function() {
  isTRUE(as.logical(Sys.getenv("EPINOW2_FULL_TESTS", "false")))
}
