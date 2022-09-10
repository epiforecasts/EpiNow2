if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  files <- c(
    "pmfs.stan", "convolve.stan", "observation_model.stan", "secondary.stan",
    "rt.stan", "infections.stan"
  )
  suppressMessages(
  expose_stan_fns(files,
      target_dir = system.file("stan/functions", package = "EpiNow2")
    )
  )
}