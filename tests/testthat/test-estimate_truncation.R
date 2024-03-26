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

test_that("estimate_truncation works with filter_leading_zeros set", {
  skip_on_os("windows")
  # Modify the first three rows of the first dataset to have zero cases
  # and fit the model with filter_leading_zeros = TRUE. This should
  # be the same as fitting the model to the original dataset because the
  # earlier dataset is corrected to be the same as the final dataset.
  modified_data <- data.table::copy(example_truncated)
  modified_data[[1]][1:3, confirm := 0]
  modified_data_fit <- estimate_truncation(
    modified_data,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250,
    stan = stan_opts(backend = "cmdstanr"),
    filter_leading_zeros = TRUE
  )
  # fit model to original dataset
  original_data_fit <- estimate_truncation(
    example_truncated,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250,
    stan = stan_opts(backend = "cmdstanr")
  )
  expect_named(
    modified_data_fit,
    c("dist", "obs", "last_obs", "cmf", "data", "fit")
  )
  # Compare the results of the two fits
  expect_equal(
    original_data_fit$dist$dist,
    modified_data_fit$dist$dist
  )
  expect_equal(
    original_data_fit$data$obs_dist,
    modified_data_fit$data$obs_dist
  )
})

test_that("estimate_truncation works with zero_threshold set", {
  skip_on_os("windows")
  # fit model to a modified version of example_data with zero leading cases
  # but with filter_leading_zeros = TRUE
  modified_data <- example_truncated
  modified_data <- purrr::map(modified_data, function(x) x[sample(1:10, 6), confirm := 0])
  out <- estimate_truncation(modified_data,
                             verbose = FALSE, chains = 2, iter = 1000, warmup = 250,
                             stan = stan_opts(backend = "cmdstanr"),
                             zero_threshold = 1
  )
  expect_named(out, c("dist", "obs", "last_obs", "cmf", "data", "fit"))
  expect_s3_class(out$dist, "dist_spec")
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
