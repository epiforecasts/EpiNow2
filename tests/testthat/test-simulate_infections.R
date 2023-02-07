skip_on_cran()
# Setup for testing -------------------------------------------------------
futile.logger::flog.threshold("FATAL")
reported_cases <- EpiNow2::example_confirmed[1:50]
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", max_value = 10)
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer", max_value = 10)
reporting_delay <- dist_spec(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1, max = 10
)

library(data.table)
out <- suppressWarnings(estimate_infections(reported_cases,
  generation_time = generation_time_opts(generation_time),
  delays = delay_opts(reporting_delay),
  gp = NULL, rt = rt_opts(rw = 14),
  stan = stan_opts(
    chains = 2, warmup = 100, samples = 100,
    control = list(adapt_delta = 0.9)
  ),
  verbose = FALSE
))


test_that("simulate_infections works to simulate a passed in estimate_infections object", {
  sims <- simulate_infections(out)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with an adjusted Rt", {
  R <- c(rep(NA_real_, 40), rep(0.5, 9))
  sims <- simulate_infections(out, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 9), rep(0.5, 9))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with a short adjusted Rt", {
  R <- c(rep(NA_real_, 40), rep(0.5, 10))
  sims <- simulate_infections(out, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 9), rep(0.5, 9))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with a long adjusted Rt", {
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- simulate_infections(out, R)
  sims10 <- simulate_infections(out, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
})

test_that("simulate infections can be run with a limited number of samples", {
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- simulate_infections(out, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
  expect_equal(max(sims$samples$sample), 10)
})

test_that("simulate infections fails as expected", {
  expect_error(simulate_infections())
  expect_error(simulate_infections(out[-"fit"]))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with an adjusted Rt in data frame", {
  R <- c(rep(1.4, 32), rep(0.5, 17))
  R_dt <- data.frame(date = summary(out, type = "parameters", param = "R")$date, value = R)
  sims_dt <- simulate_infections(out, R_dt)
  expect_equal(names(sims_dt), c("samples", "summarised", "observations"))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with samples of Rt in a data frame", {
  R_samples <- summary(out, type = "samples", param = "R")
  R_samples <- R_samples[, .(date, sample, value)][sample <= 1000]
  R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
  sims_sample <- simulate_infections(out, R_samples)
  expect_equal(names(sims_sample), c("samples", "summarised", "observations"))
})
