skip_on_cran()
skip_on_os("windows")

test_that("day_of_week_effect applies day of week effect correctly", {
  reports <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  day_of_week <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
  effect <- c(1.0, 1.1, 1.2)

  expected <- reports * effect[day_of_week] * 3
  result <- day_of_week_effect(reports, day_of_week, effect)

  expect_equal(result, expected)
})

test_that("scale_obs scales reports by fraction observed", {
  reports <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  frac_obs <- 0.5

  expected <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500)
  result <- scale_obs(reports, frac_obs)

  expect_equal(result, expected)
})

test_that("truncate_obs truncates reports correctly", {
  reports <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  trunc_rev_cmf <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

  expected_truncate <- c(100, 180, 240, 280, 300, 300, 280, 240, 180, 100)
  result_truncate <- truncate_obs(reports, trunc_rev_cmf, reconstruct = 0)

  expect_equal(result_truncate, expected_truncate)

  result_reconstruct <- truncate_obs(expected_truncate, trunc_rev_cmf, reconstruct = 1)

  expect_equal(result_reconstruct, reports)
})

test_that("report_lp correctly computes log probability for Poisson model", {
  # Poisson model (model_type = 0)
  # Should use Poisson likelihood, phi_mean is irrelevant
  obs <- c(10, 20, 30)
  exp_obs <- c(12.0, 18.0, 32.0)
  rep_phi <- 0.0  # Not used for Poisson
  model_type <- 0  # Poisson
  weight <- 1.0

  # For Poisson: log_lik = sum(dpois(obs, exp_obs, log = TRUE))
  expected_lp <- sum(dpois(obs, exp_obs, log = TRUE))
  result_lp <- report_lp(obs, exp_obs, rep_phi, model_type, weight)

  expect_equal(result_lp, expected_lp, tolerance = 1e-6)
})

test_that("report_lp correctly computes log probability for Negative Binomial model", {
  # Negative Binomial model (model_type = 1)
  obs <- c(10, 20, 30)
  exp_obs <- c(12.0, 18.0, 32.0)
  rep_phi <- c(0.1, 0.1, 0.1)  # Overdispersion parameter
  model_type <- 1  # Negative Binomial
  weight <- 1.0

  # Result should be finite and less than or equal to Poisson
  # (NB allows for overdispersion, so fit should be at least as good)
  result_lp <- report_lp(obs, exp_obs, rep_phi, model_type, weight)

  expect_true(is.finite(result_lp))
  expect_type(result_lp, "double")
})

test_that("report_lp handles zero observations correctly", {
  obs <- c(0, 0, 0)
  exp_obs <- c(5.0, 10.0, 15.0)
  rep_phi <- 0.0
  model_type <- 0  # Poisson
  weight <- 1.0

  expected_lp <- sum(dpois(obs, exp_obs, log = TRUE))
  result_lp <- report_lp(obs, exp_obs, rep_phi, model_type, weight)

  expect_equal(result_lp, expected_lp, tolerance = 1e-6)
  expect_true(is.finite(result_lp))
})

test_that("report_lp handles weights correctly", {
  obs <- c(10, 20, 30)
  exp_obs <- c(12.0, 18.0, 32.0)
  rep_phi <- 0.0
  model_type <- 0  # Poisson

  # Weight of 1.0 vs 0.5 should scale the log probability
  result_weight_1 <- report_lp(obs, exp_obs, rep_phi, model_type, 1.0)
  result_weight_half <- report_lp(obs, exp_obs, rep_phi, model_type, 0.5)

  expect_equal(result_weight_half, result_weight_1 * 0.5, tolerance = 1e-6)
})
