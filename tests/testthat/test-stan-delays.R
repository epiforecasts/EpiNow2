skip_on_cran()
skip_on_os("windows")

# Test get_delay_rev_pmf with simple parametric delay
test_that("get_delay_rev_pmf works with single parametric delay", {
  # Simple setup: one lognormal delay
  delay_id <- 1L
  len <- 10L
  delay_types_p <- array(1L)  # Parametric
  delay_types_id <- array(1L)
  delay_types_groups <- array(c(1L, 2L))
  delay_max <- array(8L)
  delay_np_pmf <- numeric(0)
  delay_np_pmf_groups <- array(1L)
  delay_params <- c(log(3), 0.5)  # meanlog, sdlog
  delay_params_groups <- array(c(1L, 3L))
  delay_dist <- array(0L)  # Lognormal

  pmf <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, left_truncate = 0L, reverse_pmf = 1L, cumulative = 0L
  )

  # Should return a valid PMF
  expect_equal(length(pmf), len)
  expect_true(all(pmf >= 0))
  # Sum should be close to 1 (may not be exact due to truncation)
  expect_gt(sum(pmf), 0.9)
})

# Test get_delay_rev_pmf with reversed vs non-reversed
test_that("get_delay_rev_pmf reverses correctly", {
  delay_id <- 1L
  len <- 10L
  delay_types_p <- array(1L)
  delay_types_id <- array(1L)
  delay_types_groups <- array(c(1L, 2L))
  delay_max <- array(8L)
  delay_np_pmf <- numeric(0)
  delay_np_pmf_groups <- array(1L)
  delay_params <- c(log(2), 0.3)
  delay_params_groups <- array(c(1L, 3L))
  delay_dist <- array(0L)

  # Get PMF without reversal
  pmf_normal <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, 0L, reverse_pmf = 0L, 0L
  )

  # Get PMF with reversal
  pmf_reversed <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, 0L, reverse_pmf = 1L, 0L
  )

  # Reversed should be reverse of normal
  expect_equal(pmf_reversed, rev(pmf_normal))
})

# Test get_delay_rev_pmf with cumulative option
test_that("get_delay_rev_pmf produces cumulative PMF correctly", {
  delay_id <- 1L
  len <- 10L
  delay_types_p <- array(1L)
  delay_types_id <- array(1L)
  delay_types_groups <- array(c(1L, 2L))
  delay_max <- array(8L)
  delay_np_pmf <- numeric(0)
  delay_np_pmf_groups <- array(1L)
  delay_params <- c(log(4), 0.6)
  delay_params_groups <- array(c(1L, 3L))
  delay_dist <- array(0L)

  # Get daily PMF
  pmf_daily <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, 0L, 0L, cumulative = 0L
  )

  # Get cumulative PMF
  pmf_cumulative <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, 0L, 0L, cumulative = 1L
  )

  # Cumulative should be cumsum of daily
  expect_equal(pmf_cumulative, cumsum(pmf_daily), tolerance = 1e-10)

  # Last value of cumulative should be close to 1
  expect_gt(pmf_cumulative[len], 0.9)
})

# Test get_delay_rev_pmf with gamma distribution
test_that("get_delay_rev_pmf works with gamma distribution", {
  delay_id <- 1L
  len <- 12L
  delay_types_p <- array(1L)
  delay_types_id <- array(1L)
  delay_types_groups <- array(c(1L, 2L))
  delay_max <- array(10L)
  delay_np_pmf <- numeric(0)
  delay_np_pmf_groups <- array(1L)
  delay_params <- c(2.5, 0.5)  # shape, rate for gamma
  delay_params_groups <- array(c(1L, 3L))
  delay_dist <- array(1L)  # Gamma distribution

  pmf <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, 0L, 1L, 0L
  )

  # Should return a valid PMF
  expect_equal(length(pmf), len)
  expect_true(all(pmf >= 0))
  expect_gt(sum(pmf), 0.9)
})

# Test get_delay_rev_pmf with non-parametric delay
test_that("get_delay_rev_pmf works with non-parametric delay", {
  delay_id <- 1L
  len <- 6L
  delay_types_p <- array(0L)  # Non-parametric
  delay_types_id <- array(1L)
  delay_types_groups <- array(c(1L, 2L))
  delay_max <- array(0L)  # Not used for non-parametric
  # Provide a simple PMF
  delay_np_pmf <- c(0.1, 0.3, 0.4, 0.2)
  delay_np_pmf_groups <- array(c(1L, 5L))
  delay_params <- numeric(0)  # Not used for non-parametric
  delay_params_groups <- array(1L)
  delay_dist <- array(0L)

  pmf <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, 0L, 1L, 0L
  )

  # Should return the reversed non-parametric PMF padded at the beginning
  expect_equal(length(pmf), len)
  # First two elements should be zero (padding)
  expect_equal(pmf[1:2], c(0, 0))
  # Remaining elements contain the reversed PMF
  expect_equal(sum(pmf[3:6]), 1, tolerance = 1e-10)
  expect_equal(pmf[3:6], rev(delay_np_pmf), tolerance = 1e-10)
})

# Test get_delay_rev_pmf with left truncation
test_that("get_delay_rev_pmf handles left truncation", {
  delay_id <- 1L
  len <- 10L
  delay_types_p <- array(1L)
  delay_types_id <- array(1L)
  delay_types_groups <- array(c(1L, 2L))
  delay_max <- array(8L)
  delay_np_pmf <- numeric(0)
  delay_np_pmf_groups <- array(1L)
  delay_params <- c(log(3), 0.5)
  delay_params_groups <- array(c(1L, 3L))
  delay_dist <- array(0L)
  left_truncate <- 2L

  pmf <- get_delay_rev_pmf(
    delay_id, len, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups,
    delay_dist, left_truncate, 1L, 0L
  )

  # First left_truncate elements should be close to zero
  expect_true(all(pmf[1:left_truncate] < 0.05))

  # Total PMF should sum to approximately 1
  expect_equal(sum(pmf), 1, tolerance = 0.05)

  # Most mass should be in later elements
  expect_gt(sum(pmf[(left_truncate + 1):len]), 0.9)
})
