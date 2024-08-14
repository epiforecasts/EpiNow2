skip_on_cran()
skip_on_os("windows")

# Test calc_conv_indices_xlen function
test_that("calc_conv_indices_xlen calculates correct indices", {
  expect_equal(calc_conv_indices_xlen(1, 5, 3), c(1, 1, 3, 3))
  expect_equal(calc_conv_indices_xlen(3, 5, 3), c(1, 3, 1, 3))
  expect_equal(calc_conv_indices_xlen(5, 5, 3), c(3, 5, 1, 3))
})

# Test calc_conv_indices_len function
test_that("calc_conv_indices_len calculates correct indices", {
  expect_equal(calc_conv_indices_len(6, 5, 3), c(4, 5, 1, 2))
  expect_equal(calc_conv_indices_len(7, 5, 3), c(5, 5, 1, 1))
  expect_equal(calc_conv_indices_len(8, 5, 3), c(6, 5, 1, 0))
})

test_that("convolve_with_rev_pmf can combine two pmfs as expected", {
  expect_equal(
    convolve_with_rev_pmf(c(0.1, 0.2, 0.7), rev(c(0.1, 0.2, 0.7)), 5),
    c(0.01, 0.04, 0.18, 0.28, 0.49),
    tolerance = 0.01
  )
  expect_equal(
    sum(convolve_with_rev_pmf(
      c(0.05, 0.55, 0.4), rev(c(0.1, 0.2, 0.7)), 5
    )), 1
  )
})

test_that("convolve_with_rev_pmf performs the same as a numerical convolution", {
  # Sample and analytical PMFs for two Poisson distributions
  x <- rpois(10000, 3)
  xpmf <- dpois(0:20, 3)
  y <- rpois(10000, 5)
  ypmf <- dpois(0:20, 5)
  # Add sampled Poisson distributions up to get combined distribution
  z <- x + y
  # Analytical convolution of PMFs
  conv_pmf <- convolve_with_rev_pmf(xpmf, rev(ypmf), 41)
  conv_cdf <- cumsum(conv_pmf)
  # Empirical convolution of PMFs
  cdf <- ecdf(z)(0:40)
  # Test analytical and numerical convolutions are similar with a small error
  # allowed
  expect_lte(sum(abs(conv_cdf - cdf)), 0.1)
})

test_that("convolve_with_rev_pmf can combine vectors as we expect", {
  expect_equal(
    convolve_with_rev_pmf(c(0.1, 0.2, 0.7), rev(c(0.1, 0.2, 0.7)), 3),
    c(0.01, 0.04, 0.18),
    tolerance = 0.01
  )
  expect_equal(
    convolve_with_rev_pmf(
      seq_len(10), rev(c(0.1, 0.4, 0.3, 0.2)), 10
    ),
    c(0.1, 0.6, 1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 7.4, 8.4)
  )
  x <- seq_len(10)
  x[2:10] <- x[1:9] / 2
  x[1] <- 0
  expect_equal(
    convolve_with_rev_pmf(
      seq_len(10), rev(c(0, 0.5, 0, 0)), 10
    ),
    x
  )
})

test_that("convolve_dot_product can combine two vectors where x > y and len = x", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3)
  expect_equal(
    convolve_with_rev_pmf(x, rev(y), 5),
    c(1, 4, 10, 16, 22)
  )
})
