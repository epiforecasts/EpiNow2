context("simulate_infections")

# Setup for testing -------------------------------------------------------
futile.logger::flog.threshold("FATAL")
reported_cases <- EpiNow2::example_confirmed[1:50]
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", max_value = 10)
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer", max_value = 10)
reporting_delay <- list(
  mean = convert_to_logmean(3, 1), mean_sd = 0.1,
  sd = convert_to_logsd(3, 1), sd_sd = 0.1, max = 10
)

if (!testthat:::on_cran()) {
  library(data.table)
  out <- suppressWarnings(estimate_infections(reported_cases,
    generation_time = generation_time,
    delays = delay_opts(reporting_delay),
    gp = NULL, rt = rt_opts(rw = 14),
    stan = stan_opts(
      chains = 2, warmup = 100, samples = 100,
      control = list(adapt_delta = 0.9)
    )
  ))
}

test_that("simulate_infections works to simulate a passed in estimate_infections object", {
  skip_on_cran()
  sims <- simulate_infections(out)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with an adjusted Rt", {
  skip_on_cran()
  R <- c(rep(NA_real_, 40), rep(0.5, 17))
  sims <- simulate_infections(out, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with a short adjusted Rt", {
  skip_on_cran()
  R <- c(rep(NA_real_, 40), rep(0.5, 10))
  sims <- simulate_infections(out, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with a long adjusted Rt", {
  skip_on_cran()
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- simulate_infections(out, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("simulate infections fails as expected", {
  skip_on_cran()
  expect_error(simulate_infections())
  expect_error(simualate_infections(out, R = rep(1, 10)))
  expect_error(simulate_infections(out[-"fit"]))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with an adjusted Rt in data frame", {
  skip_on_cran()
  R <- c(rep(NA_real_, 40), rep(0.5, 17))
  R_dt <- data.frame(date = summary(out, type = "parameters", param = "R")$date, value = R)
  sims_dt <- simulate_infections(out, R_dt)
  expect_equal(names(sims_dt), c("samples", "summarised", "observations"))
})

test_that("simulate_infections works to simulate a passed in estimate_infections object with samples of Rt in a data frame", {
  skip_on_cran()
  R_samples <- summary(out, type = "samples", param = "R")
  R_samples <- R_samples[,.(date, sample, value)][sample <= 1000]
  R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
  sims_sample <- simulate_infections(out, R_samples)
  expect_equal(names(sims_sample), c("samples", "summarised", "observations"))
})

