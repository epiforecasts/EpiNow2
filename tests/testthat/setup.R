if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  files <- c(
    "convolve.stan", "pmfs.stan", "observation_model.stan", "secondary.stan",
    "rt.stan", "infections.stan"
  )
  if (!(tolower(Sys.info()[["sysname"]]) %in% "windows")) {
   suppressMessages(
      expose_stan_fns(files,
        target_dir = system.file("stan/functions", package = "EpiNow2")
      )
   )
  }
}

withr::defer(future::plan("sequential"), teardown_env())
