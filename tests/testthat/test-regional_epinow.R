context("regional_epinow")

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)

reporting_delay <- list(mean = log(10),
                        mean_sd = log(2),
                        sd = log(2),
                        sd_sd = log(1.1),
                        max = 30)

## Uses example case vector
cases <- EpiNow2::example_confirmed[1:40]

cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]))




## Errors are handled correctly
test_that("estimate_infections fails as expected when given a very short timeout", {
  skip_on_cran()
  expect_error(regional_epinow(reported_cases = cases,
                           generation_time = generation_time,
                           delays = list(incubation_period, reporting_delay),
                           adapt_delta = 0.9, samples = 2000, warmup = 500,
                           cores = 1, max_execution_time = 10))
  expect_error(regional_epinow(reported_cases = cases,
                               generation_time = generation_time,
                               delays = list(incubation_period, reporting_delay),
                               adapt_delta = 0.9, samples = 2000, warmup = 500, future = TRUE,
                               cores = 1, max_execution_time = 10))

})
