skip_on_cran()
skip_on_os("windows")

test_that("calculate_growth_infections works as expected", {
  skip_on_cran()
  expect_equal(calculate_growth_infections(rep(1, 5), 0), rep(0, 4))
  expect_equal(
    round(calculate_growth_infections(1:5, 1), 2), c(0.41, 0.29, 0.22)
  )
  expect_equal(
    round(calculate_growth_infections(exp(0.4 * 1:5), 1), 2), rep(0.4, 3)
  )

  expect_no_error(calculate_growth_infections(1:5, 0))
  expect_no_error(calculate_growth_infections(1:5, 1))
  expect_no_error(calculate_growth_infections(1:5, 2))
  expect_no_error(calculate_growth_infections(1:5, 3))
  # from here on there are not enough modeled data points (less than 2)
  expect_error(calculate_growth_infections(1:5, 4))
  expect_error(calculate_growth_infections(1:5, 5))
  expect_error(calculate_growth_infections(1:5, 6))
})

test_that("calculate_growth_infness works as expected", {
  skip_on_cran()
  gt_rev_pmf <- rev(c(0.5, 0.3, 0.2))
  mean_gt <- round(sum(gt_rev_pmf * seq_along(gt_rev_pmf)))

  expect_equal(
    calculate_growth_infness(rep(1, 7), 0, gt_rev_pmf),
    c(rep(0, 6 - mean_gt), rep(NaN, mean_gt))
  )

  seeding <- 1
  infness <- stable_convolve(1:7, gt_rev_pmf)
  growth <- diff(log(infness))[(1 + mean_gt + seeding):(length(infness) - 1 - mean_gt)]
  expect_equal(
    round(calculate_growth_infness(1:7, seeding, gt_rev_pmf), 2),
    c(round(growth, 2), rep(NaN, mean_gt))
  )

  seeding <- 2
  infness <- stable_convolve(exp(0.4 * 1:7), gt_rev_pmf)
  growth <- diff(log(infness))[(1 + mean_gt + seeding):(length(infness) - 1 - mean_gt)]
  expect_equal(
    round(calculate_growth_infness(exp(0.4 * 1:7), seeding, gt_rev_pmf), 2),
    c(round(growth, 2), rep(NaN, mean_gt))
  )

  expect_no_error(calculate_growth_infness(1:7, 0, gt_rev_pmf))
  expect_no_error(calculate_growth_infness(1:7, 1, gt_rev_pmf))
  expect_no_error(calculate_growth_infness(1:7, 2, gt_rev_pmf))
  expect_no_error(calculate_growth_infness(1:7, 3, gt_rev_pmf))
  # from here on there are not enough data points (less than 2 + mean_gt)
  expect_error(calculate_growth_infness(1:7, 4, gt_rev_pmf))
  expect_error(calculate_growth_infness(1:7, 5, gt_rev_pmf))
  expect_error(calculate_growth_infness(1:7, 6, gt_rev_pmf))
  expect_error(calculate_growth_infness(1:7, 7, gt_rev_pmf))
})

test_that("calculate_growth selects the right method", {
  skip_on_cran()

  gt_rev_pmf <- rev(c(0.5, 0.3, 0.2))
  mean_gt <- round(sum(gt_rev_pmf * seq_along(gt_rev_pmf)))

  seeding <- 2
  infness <- stable_convolve(1:7, gt_rev_pmf)
  growth <- diff(log(infness))[(1 + mean_gt + seeding):(length(infness) - 1 - mean_gt)]

  expect_equal(
    round(calculate_growth(1:7, seeding, gt_rev_pmf, 0), 2),
    c(0.29, 0.22, 0.18, 0.15)
  )

  expect_equal(
    round(calculate_growth(1:7, seeding, gt_rev_pmf, 1), 2),
    c(round(growth, 2), rep(NaN, mean_gt))
  )
})

# generation time with no mass at lag 0 and mass on lags 1 to 3
gt_pmf <- c(0, 0.5, 0.3, 0.2)
gt_rev <- rev(gt_pmf)

test_that("calculate_Rt returns one per observed time with constant infections", {
  infections <- rep(1000, 10)
  rt <- calculate_Rt(infections, 3, gt_rev, 0)
  expect_length(rt, 7)
  expect_equal(rt, rep(1, 7), tolerance = 1e-6)
})

test_that("calculate_Rt recovers Rt implied by exponential growth", {
  r <- 0.1
  infections <- 1000 * exp(r * 1:12)
  # Euler-Lotka relation between growth rate and reproduction number
  expected <- 1 / sum(gt_pmf * exp(-r * (seq_along(gt_pmf) - 1)))
  rt <- calculate_Rt(infections, 3, gt_rev, 0)
  expect_equal(rt, rep(expected, 9), tolerance = 1e-6)
  expect_gt(expected, 1)
  expect_lt(calculate_Rt(rev(infections), 3, gt_rev, 0)[9], 1)
})

test_that("calculate_Rt smooths with a centred window truncated at the edges", {
  infections <- c(10, 12, 15, 20, 18, 14, 11, 9, 8, 7) * 100
  raw <- calculate_Rt(infections, 3, gt_rev, 0)
  n <- length(raw)
  for (smooth in 1:2) {
    expected <- vapply(seq_len(n), function(s) {
      mean(raw[max(1, s - smooth):min(n, s + smooth)])
    }, numeric(1))
    expect_equal(calculate_Rt(infections, 3, gt_rev, smooth), expected)
  }
})

test_that("calculate_growth errors for an unknown growth method", {
  expect_error(
    calculate_growth(1:7, 1, gt_rev, 2), "growth_method must be 0"
  )
})
