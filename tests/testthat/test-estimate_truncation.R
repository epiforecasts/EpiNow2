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
    )
  )))
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
  modified_data <- lapply(modified_data, apply_zero_threshold, threshold = 1)
  out <- estimate_truncation(modified_data,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  expect_named(out, c("dist", "obs", "last_obs", "cmf", "data", "fit"))
  expect_s3_class(out$dist, "dist_spec")
})

test_that("estimate_truncation warns when truncation PMF exceeds data length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  # Use short truncated example data (first 5 rows)
  trunc_data <- lapply(example_truncated, function(x) x[1:5])
  
  # Long truncation PMF
  long_trunc <- NonParametric(rep(0.1/9, 10))  # length 10
  
  expect_warning(
      estimate_truncation(
        data = trunc_data,
        truncation = trunc_opts(long_trunc),
        stan = stan_opts(
          chains = 1,
          warmup = 20,
          samples = 20
        ),
        verbose = FALSE
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("estimate_truncation runs without PMF warnings when lengths are appropriate", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  # Use full truncated example data
  trunc_data <- example_truncated
  
  # Short truncation PMF that won't trigger warnings
  short_trunc <- NonParametric(c(0.3, 0.4, 0.2, 0.1))  # length 4
  
  # Should not produce PMF length warnings
  expect_no_warning(
      estimate_truncation(
        data = trunc_data,
        truncation = trunc_opts(short_trunc),
        stan = stan_opts(
          chains = 1,
          warmup = 20,
          samples = 20
        ),
        verbose = FALSE
      )
  )
})

test_that("estimate_truncation warns for combined truncation and delay PMFs exceeding data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  # Short truncated data
  trunc_data <- lapply(example_truncated, function(x) x[1:7])
  
  # Two medium PMFs that combine to exceed data length
  trunc_pmf <- NonParametric(rep(0.25, 4))  # length 4
  delay_pmf <- NonParametric(rep(0.2, 5))  # length 5
  
  expect_warning(
      estimate_truncation(
        data = trunc_data,
        truncation = trunc_opts(trunc_pmf + delay_pmf),
        stan = stan_opts(
          chains = 1,
          warmup = 20,
          samples = 20
        ),
        verbose = FALSE
    ),
    "The combined non-parametric delays PMF is longer"
  )
})

options(old_opts)
