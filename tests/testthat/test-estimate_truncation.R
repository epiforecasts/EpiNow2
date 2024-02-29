# Setup for testing -------------------------------------------------------
skip_on_cran()
futile.logger::flog.threshold("FATAL")

# set number of cores to use
old_opts <- options()
options(mc.cores = ifelse(interactive(), 4, 1))

test_that("estimate_truncation can return values from simulated data and plot
           them", {
  # fit model to example data
  est <- estimate_truncation(example_truncated,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  expect_equal(
    names(est),
    c("dist", "obs", "last_obs", "cmf", "data", "fit")
  )
  expect_s3_class(est$dist, "dist_spec")
  expect_error(plot(est), NA)
})

test_that("estimate_truncation can return values from simulated data with the
           cmdstanr backend", {
  # fit model to example data
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    est <- estimate_truncation(example_truncated,
      verbose = FALSE, chains = 2, iter = 1000, warmup = 250,
      stan = stan_opts(backend = "cmdstanr")
  ))))
  expect_equal(
    names(est),
    c("dist", "obs", "last_obs", "cmf", "data", "fit")
  )
  expect_s3_class(est$dist, "dist_spec")
  expect_error(plot(est), NA)
})

test_that("deprecated arguments are recognised", {
  options(warn = 2)
  expect_error(estimate_truncation(example_truncated,
    verbose = FALSE, trunc_max = 10
  ), "deprecated")
  expect_error(estimate_truncation(example_truncated,
    verbose = FALSE, max_truncation = 10
  ), "deprecated")
  expect_error(estimate_truncation(example_truncated,
    verbose = FALSE, trunc_dist = "lognormal"
  ), "deprecated")
})

options(old_opts)
