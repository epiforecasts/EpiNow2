# Test discretised_lognormal_pmf and discretised_gamma_pmf functions
# These are internal functions so we use ::: accessor

test_that("discretised_lognormal_pmf produces valid PMF", {
  pmf <- EpiNow2:::discretised_lognormal_pmf(
    meanlog = 1.5, sdlog = 0.5, max_d = 10
  )

  # PMF should sum to 1
  expect_equal(sum(pmf), 1, tolerance = 1e-6)

  # All probabilities should be non-negative
  expect_true(all(pmf >= 0))

  # Should have correct length
  expect_length(pmf, 11)  # 0 to max_d inclusive
})

test_that("discretised_lognormal_pmf handles different parameters", {
  # Short delay (small meanlog)
  pmf_short <- EpiNow2:::discretised_lognormal_pmf(
    meanlog = 0.5, sdlog = 0.3, max_d = 10
  )

  # Long delay (large meanlog)
  pmf_long <- EpiNow2:::discretised_lognormal_pmf(
    meanlog = 2.5, sdlog = 0.3, max_d = 10
  )

  # Both should be valid PMFs
  expect_equal(sum(pmf_short), 1, tolerance = 1e-6)
  expect_equal(sum(pmf_long), 1, tolerance = 1e-6)

  # Short delay should have more mass early
  expect_gt(pmf_short[1], pmf_long[1])
})

test_that("discretised_lognormal_pmf reverse option works", {
  pmf_forward <- EpiNow2:::discretised_lognormal_pmf(
    meanlog = 1.5, sdlog = 0.5, max_d = 10, reverse = FALSE
  )
  pmf_reverse <- EpiNow2:::discretised_lognormal_pmf(
    meanlog = 1.5, sdlog = 0.5, max_d = 10, reverse = TRUE
  )

  # Reversed should be the reverse of forward
  expect_equal(pmf_reverse, rev(pmf_forward))
})

test_that("discretised_gamma_pmf produces valid PMF", {
  pmf <- EpiNow2:::discretised_gamma_pmf(mean = 5, sd = 2, max_d = 15)

  # PMF should sum to 1
  expect_equal(sum(pmf), 1, tolerance = 1e-6)

  # All probabilities should be non-negative
  expect_true(all(pmf >= 0))

  # Should have correct length
  expect_length(pmf, 16)  # 0 to max_d inclusive
})

test_that("discretised_gamma_pmf handles different parameters", {
  # Short delay (small mean)
  pmf_short <- EpiNow2:::discretised_gamma_pmf(mean = 2, sd = 1, max_d = 15)

  # Long delay (large mean)
  pmf_long <- EpiNow2:::discretised_gamma_pmf(mean = 8, sd = 2, max_d = 15)

  # Both should be valid PMFs
  expect_equal(sum(pmf_short), 1, tolerance = 1e-6)
  expect_equal(sum(pmf_long), 1, tolerance = 1e-6)

  # Short delay should have more mass early
  expect_gt(sum(pmf_short[1:5]), sum(pmf_long[1:5]))
})

test_that("discretised_gamma_pmf zero_pad option works", {
  pmf_no_pad <- EpiNow2:::discretised_gamma_pmf(
    mean = 3, sd = 1, max_d = 10, zero_pad = 0
  )
  pmf_pad_3 <- EpiNow2:::discretised_gamma_pmf(
    mean = 3, sd = 1, max_d = 10, zero_pad = 3
  )

  # Padded version should have 3 leading zeros
  expect_equal(pmf_pad_3[1:3], c(0, 0, 0))

  # Padded version should be longer
  expect_equal(length(pmf_pad_3), length(pmf_no_pad) + 3)
})

test_that("discretised_gamma_pmf reverse option works", {
  pmf_forward <- EpiNow2:::discretised_gamma_pmf(
    mean = 5, sd = 2, max_d = 10, reverse = FALSE
  )
  pmf_reverse <- EpiNow2:::discretised_gamma_pmf(
    mean = 5, sd = 2, max_d = 10, reverse = TRUE
  )

  # Reversed should be the reverse of forward
  expect_equal(pmf_reverse, rev(pmf_forward))
})

test_that("discretised_gamma_pmf zero_pad and reverse work together", {
  pmf <- EpiNow2:::discretised_gamma_pmf(
    mean = 3, sd = 1, max_d = 8, zero_pad = 2, reverse = TRUE
  )

  # Last two elements should be zeros (reverse of leading zeros)
  expect_equal(pmf[length(pmf) - 1], 0)
  expect_equal(pmf[length(pmf)], 0)
})

test_that("discretised_lognormal_pmf_conv produces valid output", {
  x <- rep(100, 10)
  result <- EpiNow2:::discretised_lognormal_pmf_conv(
    x, meanlog = 1.5, sdlog = 0.5
  )

  # Should return a single numeric value
  expect_length(result, 1)
  expect_true(is.finite(result))

  # Convolution result should be positive for positive inputs
  expect_gt(result, 0)
})

test_that("discretised_lognormal_pmf_conv with uniform input", {
  # With uniform input and PMF summing to 1, should return the input value
  x <- rep(50, 15)
  result <- EpiNow2:::discretised_lognormal_pmf_conv(
    x, meanlog = 1.5, sdlog = 0.5
  )

  # Should be close to input value since PMF sums to 1
  expect_equal(result, 50, tolerance = 1)
})
