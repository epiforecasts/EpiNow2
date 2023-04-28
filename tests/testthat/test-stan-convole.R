skip_on_cran()
skip_on_os("windows")

test_that("convolve can combine two pmfs as expected", {
  expect_equal(
    convolve_pmf(c(0.1, 0.2, 0.7), c(0.1, 0.2, 0.7), 6),
    c(0.01, 0.04, 0.18, 0.28, 0.49, 0.00),
    tolerance = 0.01
  )
  expect_equal(
    sum(convolve_pmf(c(0.05, 0.55, 0.4), c(0.1, 0.2, 0.7), 6)), 1
  )
})

test_that("convolve performs the same as a numerical convolution", {
  set.seed(123)
  # Sample and analytical PMFs for two Poisson distributions
  x <- rpois(10000, 3)
  xpmf <- dpois(0:20, 3)
  y <- rpois(10000, 5)
  ypmf <- dpois(0:20, 5)
  # Add sampled Poisson distributions up to get combined distribution
  z <- x + y
  # Analytical convolution of PMFs
  conv_pmf <- convolve_pmf(xpmf, ypmf, 42)
  conv_cdf <- cumsum(conv_pmf)
  # Empirical convolution of PMFs
  cdf <- ecdf(z)(0:41)
  # Test analytical and numerical convolutions are similar with a small error
  # allowed
  expect_lte(sum(abs(conv_cdf - cdf)), 0.1)
})

test_that("convolve_dot_product can combine vectors as we expect", {
  expect_equal(
    convolve_dot_product(c(0.1, 0.2, 0.7), rev(c(0.1, 0.2, 0.7)), 3),
    c(0.01, 0.04, 0.18),
    tolerance = 0.01
  )
  expect_equal(
    convolve_dot_product(
      seq_len(10), rev(c(0.1, 0.4, 0.3, 0.2)), 10
    ),
    c(0.1, 0.6, 1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 7.4, 8.4)
  )
  x <- seq_len(10)
  x[2:10] <- x[1:9] / 2
  x[1] <- 0
  expect_equal(
    convolve_dot_product(
      seq_len(10), rev(c(0, 0.5, 0, 0)), 10
    ),
    x
  )
})
