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
    calculate_growth_infness(rep(1, 7), 1, gt_rev_pmf),
    c(rep(0, 6 - mean_gt), rep(NaN, mean_gt))
    )
  
  seeding <- 2
  infness <- stable_convolve(1:7, gt_rev_pmf)
  growth <- diff(log(infness))[(mean_gt+seeding):(length(infness)-1-mean_gt)]
  expect_equal(
    round(calculate_growth_infness(1:7, seeding, gt_rev_pmf), 2),
    c(round(growth, 2), rep(NaN, mean_gt))
  )
  
  seeding <- 3
  infness <- stable_convolve(exp(0.4 * 1:7), gt_rev_pmf)
  growth <- diff(log(infness))[(mean_gt+seeding):(length(infness)-1-mean_gt)]
  expect_equal(
    round(calculate_growth_infness(exp(0.4 * 1:7), seeding, gt_rev_pmf), 2),
    c(round(growth, 2), rep(NaN, mean_gt))
  )
  
  expect_error(calculate_growth_infness(1:5, 6, gt_rev_pmf))
})

test_that("calculate_growth selects the right method", {
  skip_on_cran()
  
  gt_rev_pmf <- rev(c(0.5, 0.3, 0.2))
  mean_gt <- round(sum(gt_rev_pmf * seq_along(gt_rev_pmf)))
  
  seeding <- 2
  infness <- stable_convolve(1:7, gt_rev_pmf)
  growth <- diff(log(infness))[(mean_gt+seeding):(length(infness)-1-mean_gt)]
  
  expect_equal(
    round(calculate_growth(1:7, seeding, gt_rev_pmf, 0), 2),
    c(0.29, 0.22, 0.18, 0.15)
  )
  
  expect_equal(
    round(calculate_growth(1:7, seeding, gt_rev_pmf, 1), 2),
    c(round(growth, 2), rep(NaN, mean_gt))
  )
})