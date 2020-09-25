context("estimate_infections")



## Errors are handled correctly
test_that("estimate_infections fails as expected when given a very short timeout", {
  skip_on_cran()
  reported_cases <- EpiNow2::example_confirmed[1:50]

  # Add a dummy breakpoint (used only when optionally estimating breakpoints)
  reported_cases <- reported_cases[, breakpoint := data.table::fifelse(date == as.Date("2020-03-16"),
                                                                       1, 0)]
  # Set up example generation time
  generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                          mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                          sd = EpiNow2::covid_generation_times[1, ]$sd,
                          sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                          max = 30)
  # Set delays between infection and case report
  # (any number of delays can be specifed here)
  incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                            mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                            sd = EpiNow2::covid_incubation_period[1, ]$sd,
                            sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                            max = 30)

  reporting_delay <- list(mean = log(5),
                          mean_sd = log(2),
                          sd = log(2),
                          sd_sd = log(1.5),
                          max = 30)

  expect_error(estimate_infections(reported_cases, family = "negbin",
                                   generation_time = generation_time,
                                   delays = list(incubation_period, reporting_delay),
                                   samples = 500, warmup = 200, verbose = FALSE,
                                   chains = 2, future = TRUE, max_execution_time = 10))
  expect_error(estimate_infections(reported_cases, family = "negbin",
                                   generation_time = generation_time,
                                   delays = list(incubation_period, reporting_delay),
                                   samples = 500, warmup = 200, verbose = FALSE,
                                   chains = 2, future = FALSE, max_execution_time = 10))
})
