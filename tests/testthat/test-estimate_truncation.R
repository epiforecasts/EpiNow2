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
    c("fit", "args", "observations", ".internal")
  )
  expect_s3_class(est, "estimate_truncation")
  expect_s3_class(est, "epinowfit")
  expect_s3_class(summary(est), "dist_spec")
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
    )
  )))
  expect_equal(
    names(est),
    c("fit", "args", "observations", ".internal")
  )
  expect_s3_class(est, "estimate_truncation")
  expect_s3_class(est, "epinowfit")
  expect_s3_class(summary(est), "dist_spec")
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
  modified_data <- lapply(modified_data, filter_leading_zeros)
  modified_data_fit <- estimate_truncation(
    modified_data,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  # fit model to original dataset
  original_data_fit <- estimate_truncation(
    example_truncated,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  expect_named(
    modified_data_fit,
    c("fit", "args", "observations", ".internal")
  )
  # Compare the results of the two fits
  expect_equal(
    summary(original_data_fit)$dist,
    summary(modified_data_fit)$dist
  )
  expect_equal(
    original_data_fit$args$obs_dist,
    modified_data_fit$args$obs_dist
  )
})

test_that("estimate_truncation works with zero_threshold set", {
  skip_on_os("windows")
  # fit model to a modified version of example_data with zero leading cases
  # but with filter_leading_zeros = TRUE
  modified_data <- example_truncated
  modified_data <- purrr::map(modified_data, function(x) x[sample(1:10, 6), confirm := 0])
  modified_data <- lapply(modified_data, apply_zero_threshold, threshold = 1)
  out <- estimate_truncation(modified_data,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  expect_named(out, c("fit", "args", "observations", ".internal"))
  expect_s3_class(out, "estimate_truncation")
  expect_s3_class(out, "epinowfit")
  expect_s3_class(summary(out), "dist_spec")
})

options(old_opts)
