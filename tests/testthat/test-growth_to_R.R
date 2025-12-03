# Test growth_to_R and R_to_growth functions

test_that("growth_to_R produces expected output for known values", {

  # With r = 0, R should equal 1 (no growth)
  expect_equal(growth_to_R(0, gamma_mean = 4, gamma_sd = 1), 1)


  # Positive growth rate should give R > 1

  R_positive <- growth_to_R(0.1, gamma_mean = 4, gamma_sd = 1)
  expect_gt(R_positive, 1)


  # Negative growth rate should give R < 1
  R_negative <- growth_to_R(-0.1, gamma_mean = 4, gamma_sd = 1)
  expect_lt(R_negative, 1)
})

test_that("growth_to_R handles different generation time parameters", {
  # Same growth rate with different generation times
  R_short <- growth_to_R(0.1, gamma_mean = 2, gamma_sd = 0.5)
  R_long <- growth_to_R(0.1, gamma_mean = 6, gamma_sd = 1.5)


  # Longer generation time should give higher R for same growth rate

  expect_gt(R_long, R_short)
})

test_that("R_to_growth produces expected output for known values", {
  # With R = 1, growth rate should equal 0

  expect_equal(R_to_growth(1, gamma_mean = 4, gamma_sd = 1), 0)

  # R > 1 should give positive growth rate
  r_positive <- R_to_growth(1.5, gamma_mean = 4, gamma_sd = 1)
  expect_gt(r_positive, 0)

  # R < 1 should give negative growth rate
  r_negative <- R_to_growth(0.8, gamma_mean = 4, gamma_sd = 1)
  expect_lt(r_negative, 0)
})

test_that("growth_to_R and R_to_growth are inverses", {
  gamma_mean <- 4
  gamma_sd <- 1


  # Test round-trip: r -> R -> r

  r_values <- c(-0.2, -0.1, 0, 0.1, 0.2, 0.3)
  for (r in r_values) {
    R <- growth_to_R(r, gamma_mean, gamma_sd)
    r_back <- R_to_growth(R, gamma_mean, gamma_sd)
    expect_equal(r_back, r, tolerance = 1e-10)
  }

  # Test round-trip: R -> r -> R

  R_values <- c(0.5, 0.8, 1.0, 1.2, 1.5, 2.0)
  for (R in R_values) {
    r <- R_to_growth(R, gamma_mean, gamma_sd)
    R_back <- growth_to_R(r, gamma_mean, gamma_sd)
    expect_equal(R_back, R, tolerance = 1e-10)
  }
})

test_that("growth_to_R handles vectorised input", {
  r_vec <- c(-0.1, 0, 0.1, 0.2)
  R_vec <- growth_to_R(r_vec, gamma_mean = 4, gamma_sd = 1)

  expect_length(R_vec, 4)
  expect_true(all(is.finite(R_vec)))
  # Should be monotonically increasing

  expect_true(all(diff(R_vec) > 0))
})

test_that("R_to_growth handles vectorised input", {
  R_vec <- c(0.5, 1.0, 1.5, 2.0)
  r_vec <- R_to_growth(R_vec, gamma_mean = 4, gamma_sd = 1)

  expect_length(r_vec, 4)
  expect_true(all(is.finite(r_vec)))
  # Should be monotonically increasing
  expect_true(all(diff(r_vec) > 0))
})
