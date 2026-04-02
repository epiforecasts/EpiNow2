skip_on_cran()
skip_on_os("windows")

# test update_infectiousness
test_that("update_infectiousness works as expected with default settings", {
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 10), 5, 10),
    1
  )
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10),
    0.5
  )
  expect_error(update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10, 10))
})

pmf <- discretised_pmf(c(2.25, 0.75), 15, 2, 0)
gt_rev_pmf <- get_delay_rev_pmf(
  1L, 15L, array(0L), array(1L),
  array(c(1L, 2L)), array(15L), pmf,
  array(c(1L, 16L)), numeric(0), 1L, 0L,
  1L, 1L, 0L
)

# test generate infections
test_that("generate_infections works as expected", {
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_rev_pmf, log(1000), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(1000, 10), 952, 945, 933, 921, 908, 896, 883, 871, 859, 847)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, gt_rev_pmf, log(20), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(20, 10), 19, 21, 21, 21, 22, 22, 22, 23, 23, 23)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, gt_rev_pmf, log(100), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(100, 10), 95, 104, 105, 106, 108, 110, 112, 114, 115, 117)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 4, gt_rev_pmf, log(500), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(500, 4), 381, 399, 399, 394, 389, 384, 378, 373, 368, 363)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 4, gt_rev_pmf, log(500), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(500, 4), 381, 439, 447, 455, 462, 470, 478, 486, 494, 502)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 1, gt_rev_pmf, log(40), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(40, 8, 11, 12, 12, 12, 12, 11, 11, 11, 11)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 1, gt_rev_pmf, log(100), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(100, 20, 31, 33, 34, 35, 35, 36, 36, 37, 38)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_rev_pmf, log(1000), 100000, 2, 1.0, 4, 0, 0, 1), 0),
    c(rep(1000, 10), 947, 939, 926, 912, 898, 884, 871, 857, 844, 831)
  )
})

test_that("generate_infections respects pop_floor with population adjustment", {
  # Test with higher pop_floor to verify floor behavior
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_rev_pmf, log(1000), 100000, 2, 10.0, 4, 0, 0, 1), 0),
    c(rep(1000, 10), 947, 939, 926, 912, 898, 884, 871, 857, 844, 831)
  )

  # Test with very small population where floor matters
  result <- generate_infections(c(1, rep(1.5, 9)), 10, gt_rev_pmf, log(100), 500, 2, 50.0, 4, 0, 0, 1)
  # With pop_floor = 50, susceptible population should never go below 50
  # This allows infections to continue even when pop - cum_infections < 50
  expect_true(all(result >= 0))
  expect_true(all(is.finite(result)))
})

# test deconvolve_infections
test_that("deconvolve_infections with fixed mode returns shifted cases", {
  shifted_cases <- c(10, 20, 30, 40, 50)
  noise <- numeric(0)  # Noise not used in fixed mode
  result <- deconvolve_infections(shifted_cases, noise, fixed = 1, prior = 0)

  # With fixed = 1, should return shifted_cases + small offset
  expect_equal(result, shifted_cases + 1e-5, tolerance = 1e-6)
})

test_that("deconvolve_infections with prior=0 applies noise only", {
  shifted_cases <- rep(100, 10)
  noise <- rep(0.1, 10)  # Small positive noise

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 0)

  # Should be close to exp(noise) since prior=0
  expected <- 1e-5 + exp(noise)
  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("deconvolve_infections with prior=1 scales cases by noise", {
  shifted_cases <- c(10, 20, 30, 40, 50)
  noise <- rep(0, 5)  # Zero noise for simple test

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 1)

  # With zero noise and prior=1, should be cases * exp(0) = cases
  expect_equal(result, shifted_cases + 1e-5, tolerance = 1e-6)
})

test_that("deconvolve_infections with prior=2 implements random walk", {
  shifted_cases <- c(100, 110, 120, 130, 140)
  noise <- c(0, 0.1, -0.05, 0.05, 0.1)

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 2)

  # First infection based on first case
  expect_equal(result[1], 1e-5 + shifted_cases[1] * exp(noise[1]), tolerance = 1e-6)

  # Subsequent infections follow random walk
  for (i in 2:5) {
    expect_equal(result[i], result[i - 1] * exp(noise[i]) + 1e-5, tolerance = 1e-6)
  }
})

test_that("deconvolve_infections handles different noise levels", {
  shifted_cases <- rep(50, 10)
  noise_high <- rep(0.5, 10)
  noise_low <- rep(0.1, 10)

  result_high <- deconvolve_infections(shifted_cases, noise_high, fixed = 0, prior = 1)
  result_low <- deconvolve_infections(shifted_cases, noise_low, fixed = 0, prior = 1)

  # Higher noise should produce higher infections
  expect_true(all(result_high > result_low))
})

test_that("deconvolve_infections always returns positive values", {
  shifted_cases <- c(1, 5, 10, 20, 50)
  noise <- c(-1, -0.5, 0, 0.5, 1)

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 1)

  # Should all be positive due to 1e-5 offset and exponential transform
  expect_true(all(result > 0))
})
