skip_on_cran()
skip_on_os("windows")

cases <- c(0L, 3L, 12L, 40L)
expected_reports <- c(0.5, 4, 10, 45)

test_that("report_log_lik matches R densities for each model type", {
  overdispersion <- 0.5
  expect_equal(
    report_log_lik(cases, expected_reports, overdispersion, 0, 1),
    dpois(cases, expected_reports, log = TRUE)
  )
  expect_equal(
    report_log_lik(cases, expected_reports, overdispersion, 1, 1),
    dnbinom(
      cases, size = 1 / overdispersion^2, mu = expected_reports, log = TRUE
    )
  )
})

test_that("report_log_lik scales each contribution by the weight", {
  for (model_type in 0:1) {
    unweighted <- report_log_lik(cases, expected_reports, 0.5, model_type, 1)
    expect_equal(
      report_log_lik(cases, expected_reports, 0.5, model_type, 0.25),
      0.25 * unweighted
    )
  }
})

test_that("day_of_week_effect keeps weekly totals when effects sum to one", {
  reports <- rep(70, 14)
  day_of_week <- rep(1:7, 2)
  effect <- c(0.2, 0.2, 0.15, 0.15, 0.1, 0.1, 0.1)
  out <- day_of_week_effect(reports, day_of_week, effect)
  expect_equal(sum(out[1:7]), sum(reports[1:7]))
  expect_equal(out[1:7], out[8:14])
  expect_equal(
    day_of_week_effect(reports, day_of_week, rep(1 / 7, 7)), reports
  )
})

test_that("neg_binomial_2_safe_rng handles extreme parameters", {
  # tiny means always give zero
  expect_equal(neg_binomial_2_safe_rng(1e-9, 1), 0)
  # very large means are capped rather than overflowing
  for (phi in c(10, 1e5)) {
    draw <- neg_binomial_2_safe_rng(1e12, phi)
    expect_gt(draw, 0.99e8)
    expect_lt(draw, 1.01e8)
  }
})

test_that("neg_binomial_2_safe_rng has negative binomial moments", {
  mu <- 10
  phi <- 2
  n <- 20000
  draws <- replicate(n, neg_binomial_2_safe_rng(mu, phi))
  expect_true(all(draws >= 0 & draws == round(draws)))
  nb_var <- mu + mu^2 / phi
  # generous limits so the check is not sensitive to the random draws
  expect_lt(abs(mean(draws) - mu), 6 * sqrt(nb_var / n))
  expect_equal(var(draws), nb_var, tolerance = 0.15)
  # large phi falls back to a Poisson with variance equal to the mean
  pois_draws <- replicate(n, neg_binomial_2_safe_rng(mu, 2e4))
  expect_equal(var(pois_draws), mu, tolerance = 0.1)
})

test_that("report_rng returns one count per report with the model's spread", {
  reports <- c(0, 5, 50)
  expect_length(report_rng(reports, 0.5, 1), 3)
  expect_equal(report_rng(reports, 0.5, 1)[1], 0)
  draws <- replicate(4000, report_rng(reports, 0.5, 0))
  # Poisson model: variance equal to the mean
  expect_equal(apply(draws[2:3, ], 1, var), reports[2:3], tolerance = 0.15)
  nb_draws <- replicate(4000, report_rng(reports, 0.5, 1))
  # negative binomial model: variance mu + mu^2 * overdispersion^2
  expect_equal(
    apply(nb_draws[2:3, ], 1, var), reports[2:3] + reports[2:3]^2 * 0.25,
    tolerance = 0.15
  )
})
