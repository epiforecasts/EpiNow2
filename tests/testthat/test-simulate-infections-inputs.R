skip_on_cran()

# Input checks for simulate_infections() and forecast_infections(). These
# error before any simulation is run.

R <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), length.out = 14, by = "day"),
  R = c(rep(1.2, 7), rep(0.8, 7))
)

test_that("simulate_infections errors for a numeric population", {
  lifecycle::expect_defunct(
    simulate_infections(
      R = R, initial_infections = 100,
      generation_time = gt_opts(Fixed(1)), pop = 1e6
    )
  )
})

test_that("simulate_infections errors for pop_period 'all' without pop", {
  expect_error(
    simulate_infections(
      R = R, initial_infections = 100,
      generation_time = gt_opts(Fixed(1)), pop_period = "all"
    ),
    "pop is fixed at 0"
  )
})

test_that("simulate_infections errors for estimated nonparametric delays", {
  np_delay <- NonParametric(
    Dirichlet(prior = c(0.2, 0.5, 0.3), concentration = 10)
  )
  expect_error(
    simulate_infections(
      R = R, initial_infections = 100,
      generation_time = gt_opts(Fixed(1)),
      delays = delay_opts(np_delay),
      obs = obs_opts(family = "poisson")
    ),
    "estimated nonparametric delays"
  )
})

test_that("simulate_infections errors for bad argument specifications", {
  sim <- function(...) {
    simulate_infections(
      R = R, generation_time = gt_opts(Fixed(1)),
      obs = obs_opts(family = "poisson"), ...
    )
  }
  expect_error(sim(initial_infections = -1), "initial_infections")
  expect_error(
    sim(initial_infections = 100, seeding_time = 0), "seeding_time"
  )
  expect_error(sim(initial_infections = 100, pop_floor = -1), "pop_floor")
  expect_error(
    sim(initial_infections = 100, growth_method = "linear"), "growth_method"
  )
  expect_error(
    sim(initial_infections = 100, delays = list()), "delay_opts"
  )
})

test_that("forecast_infections errors for bad 'R' specifications", {
  fixtures <- get_test_fixtures()
  est <- fixtures$estimate_infections
  expect_error(
    forecast_infections(est, R = "a"),
    "R must either be a <numeric> vector or a <data.frame>"
  )
  expect_error(
    forecast_infections(est, R = c(1, -1)),
    "R must either be a <numeric> vector or a <data.frame>"
  )
  expect_error(
    forecast_infections(est, R = data.frame(date = Sys.Date(), R = 1)),
    "value"
  )
  expect_error(
    forecast_infections(est, R = data.frame(date = Sys.Date(), value = -1)),
    "R\\$value"
  )
})

test_that("forecast_infections errors for bad argument specifications", {
  fixtures <- get_test_fixtures()
  est <- fixtures$estimate_infections
  expect_error(forecast_infections(list(fit = NULL)), "estimate_infections")
  expect_error(forecast_infections(est, samples = 0), "samples")
  expect_error(forecast_infections(est, batch_size = 1), "batch_size")
  expect_error(forecast_infections(est, verbose = "yes"), "verbose")
})

test_that("forecast_infections simulates all samples in one batch", {
  skip("Known bug, see #1587")
  fixtures <- get_test_fixtures()
  sims <- forecast_infections(
    fixtures$estimate_infections, samples = 10, batch_size = NULL
  )
  expect_equal(max(sims$samples$sample), 10)
})

test_that("forecast_infections uses Rt samples and resamples the posterior", {
  fixtures <- get_test_fixtures()
  est <- fixtures$estimate_infections
  R_dates <- summary(est, type = "parameters", param = "R")$date
  n_draws <- max(get_samples(est)$sample)
  # more Rt samples than the posterior has, so posterior draws are resampled
  R_samples <- data.table::CJ(sample = seq_len(n_draws + 5), date = R_dates)
  R_samples[, value := 0.9]
  sims <- forecast_infections(est, R = R_samples)
  expect_s3_class(sims, "forecast_infections")
  expect_equal(max(sims$samples$sample), n_draws + 5)
  R_sims <- sims$summarised[variable == "R"]
  expect_equal(R_sims$date, R_dates)
  expect_equal(R_sims$median, rep(0.9, length(R_dates)))
})

test_that("forecast_infections resamples with a numeric Rt vector", {
  skip("Known bug, see #1588")
  fixtures <- get_test_fixtures()
  est <- fixtures$estimate_infections
  n_R <- nrow(summary(est, type = "parameters", param = "R"))
  n_draws <- max(get_samples(est)$sample)
  sims <- forecast_infections(est, R = rep(0.9, n_R), samples = n_draws + 5)
  expect_equal(max(sims$samples$sample), n_draws + 5)
})
