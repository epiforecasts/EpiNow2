# Test convert_to_logmean and convert_to_logsd functions

test_that("convert_to_logmean produces correct values", {
  # For a lognormal distribution, if X ~ LogNormal(mu, sigma)

  # then E[X] = exp(mu + sigma^2/2)
  # and Var[X] = (exp(sigma^2) - 1) * exp(2*mu + sigma^2)
  #
  # convert_to_logmean converts mean and sd to mu (logmean)

  mean_val <- 5
  sd_val <- 2

  logmean <- convert_to_logmean(mean_val, sd_val)
  logsd <- convert_to_logsd(mean_val, sd_val)

  # Verify by computing expected value from log parameters
  expected_mean <- exp(logmean + logsd^2 / 2)
  expect_equal(expected_mean, mean_val, tolerance = 1e-10)
})

test_that("convert_to_logsd produces correct values", {
  mean_val <- 5
  sd_val <- 2

  logmean <- convert_to_logmean(mean_val, sd_val)
  logsd <- convert_to_logsd(mean_val, sd_val)

  # Verify by computing variance from log parameters
  expected_var <- (exp(logsd^2) - 1) * exp(2 * logmean + logsd^2)
  expect_equal(sqrt(expected_var), sd_val, tolerance = 1e-10)
})

test_that("convert_to_logmean handles different parameter combinations", {
  # Small mean and sd
  expect_true(is.finite(convert_to_logmean(1, 0.5)))

  # Large mean and sd
  expect_true(is.finite(convert_to_logmean(100, 50)))

  # Very small sd relative to mean
  expect_true(is.finite(convert_to_logmean(10, 0.1)))
})

test_that("convert_to_logsd handles different parameter combinations", {
  # Small mean and sd
  expect_true(is.finite(convert_to_logsd(1, 0.5)))

  # Large mean and sd
  expect_true(is.finite(convert_to_logsd(100, 50)))

  # Very small sd relative to mean
  expect_true(is.finite(convert_to_logsd(10, 0.1)))
})

test_that("convert_to_logmean and convert_to_logsd are consistent", {
  # Generate samples from lognormal and verify moments
  set.seed(42)
  mean_val <- 3
  sd_val <- 1.5

  logmean <- convert_to_logmean(mean_val, sd_val)
  logsd <- convert_to_logsd(mean_val, sd_val)

  # Generate many samples
  samples <- rlnorm(100000, meanlog = logmean, sdlog = logsd)

  # Check sample mean and sd are close to target

  expect_equal(mean(samples), mean_val, tolerance = 0.05)
  expect_equal(sd(samples), sd_val, tolerance = 0.05)
})

test_that("convert_to_logmean handles vectorised input", {
  means <- c(1, 2, 5, 10)
  sds <- c(0.5, 1, 2, 3)

  logmeans <- convert_to_logmean(means, sds)

  expect_length(logmeans, 4)
  expect_true(all(is.finite(logmeans)))
})

test_that("convert_to_logsd handles vectorised input", {
  means <- c(1, 2, 5, 10)
  sds <- c(0.5, 1, 2, 3)

  logsds <- convert_to_logsd(means, sds)

  expect_length(logsds, 4)
  expect_true(all(is.finite(logsds)))
  expect_true(all(logsds > 0))
})

test_that("convert_to_logsd always returns positive values", {
  # logsd should always be positive
  expect_gt(convert_to_logsd(1, 0.1), 0)
  expect_gt(convert_to_logsd(10, 5), 0)
  expect_gt(convert_to_logsd(100, 10), 0)
})
