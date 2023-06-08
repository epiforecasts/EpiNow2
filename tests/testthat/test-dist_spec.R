library(testthat)

test_that("dist_spec returns correct output for fixed lognormal distribution", {
  result <- dist_spec(mean = 5, sd = 1, max = 20, distribution = "lognormal")
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
  expect_equal(dim(result$dist), 0)
  expect_equal(dim(result$max), 0)
  expect_equal(result$fixed, array(1))
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.10, 0.11, 0.12)
  )
})

test_that("dist_spec returns correct output for uncertain gamma distribution", {
  result <- dist_spec(
    mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20,
    distribution = "gamma"
  )
  expect_equal(result$mean_mean, array(3L))
  expect_equal(result$sd_mean, array(2))
  expect_equal(result$mean_sd, array(0.5))
  expect_equal(result$sd_sd, array(0.5))
  expect_equal(result$dist, array(1))
  expect_equal(result$max, array(20))
  expect_equal(result$fixed, array(0L))
})

test_that("dist_spec returns correct output for fixed distribution", {
  result <- dist_spec(
    mean = 5, mean_sd = 3, sd = 1, max = 20, distribution = "lognormal",
    fixed = TRUE
  )
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
  expect_equal(result$fixed, array(1L))
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.10, 0.11, 0.12)
  )
})

test_that("dist_spec returns error when both pmf and distributional parameters are specified", {
  expect_error(dist_spec(mean = 5, sd = 1, max = 20, distribution = "lognormal", pmf = c(0.1, 0.2, 0.3, 0.4)), 
               "Distributional parameters or a pmf can be specified, but not both.")
})

test_that("dist_spec returns error when mean is missing but other distributional parameters are given", {
  expect_error(dist_spec(sd = 1, max = 20, distribution = "lognormal"), 
               "If any distributional parameters are given then so must the mean.")
})

test_that("dist_spec returns error when maximum of parametric distributions is not specified", {
  expect_error(dist_spec(mean = 5, sd = 1, distribution = "lognormal"), 
               "Maximum of parametric distributions must be specified.")
})

test_that("+.dist_spec returns correct output for sum of two distributions", {
  lognormal <- dist_spec(mean = 5, sd = 1, max = 20, distribution = "lognormal")
  gamma <- dist_spec(mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20, distribution = "gamma")
  result <- lognormal + gamma
  expect_equal(result$mean_mean, array(3))
  expect_equal(result$sd_mean, array(2))
  expect_equal(result$mean_sd, array(0.5))
  expect_equal(result$sd_sd, array(0.5))
  expect_equal(result$n, 2)
  expect_equal(result$n_p, 1)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_max, 20)
  expect_equal(result$np_pmf_length, array(20))
})

test_that("+.dist_spec returns correct output for sum of two fixed distributions", {
  lognormal <- dist_spec(mean = 5, sd = 1, max = 20, distribution = "lognormal", fixed = TRUE)
  gamma <- dist_spec(mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20, distribution = "gamma", fixed = TRUE)
  result <- lognormal + gamma
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
  expect_equal(result$n, 1)
  expect_equal(result$n_p, 0)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_max, 39)
  expect_equal(result$np_pmf_length, 39)
})

test_that("+.dist_spec returns correct output for sum of two nonparametric distributions", {
  lognormal <- dist_spec(pmf = c(0.1, 0.2, 0.3, 0.4))
  gamma <- dist_spec(pmf = c(0.1, 0.2, 0.3, 0.4))
  result <- lognormal + gamma
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
  expect_equal(result$n, 1)
  expect_equal(result$n_p, 0)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_max, 7)
  expect_equal(result$np_pmf_length, 7)
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.01, 0.04, 0.10, 0.20, 0.25, 0.24, 0.16)
  )
})

test_that("mean.dist_spec returns correct output for fixed lognormal distribution", {
  lognormal <- dist_spec(
    mean = convert_to_logmean(3, 1), sd = convert_to_logsd(3, 1),
    max = 20, distribution = "lognormal"
  )
  result <- mean.dist_spec(lognormal)
  expect_equal(result, 2.49, tolerance = 0.01) # here we can see the bias from 
  # using this kind of discretisation approach
})

test_that("mean.dist_spec returns correct output for uncertain gamma distribution", {
  gamma <- dist_spec(mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20, distribution = "gamma")
  result <- mean.dist_spec(gamma)
  expect_equal(result, 3)
})

test_that("mean.dist_spec returns correct output for sum of two distributions", {
  lognormal <- dist_spec(mean = 1, sd = 1, max = 20, distribution = "lognormal")
  gamma <- dist_spec(mean = 3, sd = 2, max = 20, distribution = "gamma")
  result <- mean.dist_spec(lognormal + gamma)
  expect_equal(result, c(5.85), tolerance = 0.001)
})
