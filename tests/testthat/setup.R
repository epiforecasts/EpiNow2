library("data.table")
library("lifecycle")

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  files <- c(
    "convolve.stan", "gaussian_process.stan", "pmfs.stan",
    "observation_model.stan", "secondary.stan",
    "rt.stan", "infections.stan", "delays.stan", "generated_quantities.stan"
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
