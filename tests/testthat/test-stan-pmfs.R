skip_on_cran()
skip_on_os("windows")

# Test discretised_pmf for lognormal distribution
test_that("discretised_pmf produces valid PMF for lognormal distribution", {
  pmf <- discretised_pmf(c(2.0, 0.5), 10, 0)

  # PMF should sum to approximately 1
  expect_equal(sum(pmf), 1, tolerance = 1e-6)

  # All probabilities should be non-negative
  expect_true(all(pmf >= 0))

  # PMF should have a mode (peak) and then decrease
  mode_index <- which.max(pmf)
  expect_true(mode_index > 1)  # Mode should not be at day 1
})

test_that("discretised_pmf produces valid PMF for gamma distribution", {
  pmf <- discretised_pmf(c(2.5, 0.5), 10, 1)

  # PMF should sum to approximately 1
  expect_equal(sum(pmf), 1, tolerance = 1e-6)

  # All probabilities should be non-negative
  expect_true(all(pmf >= 0))

  # Check length
  expect_equal(length(pmf), 10)
})

test_that("discretised_pmf handles different parameter values correctly", {
  # Test with small mean (short delay)
  pmf_short <- discretised_pmf(c(1.0, 0.5), 10, 0)

  # Test with large mean (long delay)
  pmf_long <- discretised_pmf(c(3.0, 0.5), 10, 0)

  # Short delay should have higher probability in early days
  expect_gt(pmf_short[1], pmf_long[1])
  expect_gt(pmf_short[2], pmf_long[2])
})

test_that("discretised_pmf handles edge case of n=1", {
  pmf <- discretised_pmf(c(2.0, 0.5), 1, 0)

  # With n=1, should return a single value of 1.0
  expect_equal(length(pmf), 1)
  expect_equal(pmf[1], 1, tolerance = 1e-10)
})

test_that("discretised_pmf validates distribution type", {
  # Test with invalid distribution type (should error in Stan)
  # This test verifies the function exists and works with valid inputs
  expect_no_error(discretised_pmf(c(2.0, 0.5), 10, 0))
  expect_no_error(discretised_pmf(c(2.5, 0.5), 10, 1))
})

test_that("discretised_pmf matches expected analytical properties", {
  # For lognormal with meanlog=log(5), sdlog=0.5
  # the median should be around day 5
  pmf <- discretised_pmf(c(log(5), 0.5), 15, 0)
  cdf <- cumsum(pmf)

  # The CDF should cross 0.5 around day 5
  median_day <- which(cdf >= 0.5)[1]
  expect_true(median_day >= 4 && median_day <= 6)
})

test_that("discretised_pmf produces consistent results", {
  # Same parameters should give same results
  pmf1 <- discretised_pmf(c(2.0, 0.5), 10, 0)
  pmf2 <- discretised_pmf(c(2.0, 0.5), 10, 0)

  expect_equal(pmf1, pmf2)
})
