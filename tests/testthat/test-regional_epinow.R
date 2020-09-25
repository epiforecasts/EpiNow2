context("regional_epinow")

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

reporting_delay <- list(mean = log(3),
                        mean_sd = log(1.1),
                        sd = log(2),
                        sd_sd = log(1.1),
                        max = 30)

## Uses example case vector
cases <- EpiNow2::example_confirmed[1:20]

cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]))


df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}


test_that("regional_epinow produces expected output when run with default settings", {
  skip_on_cran()
  out <- suppressWarnings(regional_epinow(reported_cases = cases, generation_time = generation_time,
                                 delays = list(reporting_delay),
                                 gp = list(basis_prop = 0.1, boundary_scale = 2,
                                           lengthscale_mean = 20, lengthscale_sd = 2),
                                 samples = 200, warmup = 100, cores = 1, chains = 2,
                                 verbose = FALSE))
  expect_equal(names(out$regional), c("realland", "testland"))
  expect_equal(names(out$summary), c("latest_date", "results", "summarised_results", "summary_plot",
                                      "summarised_measures", "reported_cases", "high_plots", "plots"))
  expect_equal(names(out$regional$realland), c("estimates", "estimated_reported_cases", "summary", "plots", "timing"))
  expect_type(out$regional$realland$timing, "double")
  df_non_zero(out$regional$realland$estimates$samples)
  df_non_zero(out$regional$realland$estimates$summarised)
  df_non_zero(out$regional$realland$estimated_reported_cases$samples)
  df_non_zero(out$regional$realland$estimated_reported_cases$summarised)
  df_non_zero(out$regional$realland$summary)
  expect_equal(names(out$regional$realland$plots), c("infections", "reports", "reff", "growth_rate","summary"))
})

test_that("regional_epinow fails as expected when given a very short timeout", {
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
