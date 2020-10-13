context("regional_epinow")

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", max_value = 5)
reporting_delay <- list(mean = log(3), mean_sd = 0.1,
                        sd = log(2), sd_sd = 0.1, max = 5)


futile.logger::flog.threshold("ERROR")

## Uses example case vector
cases <- EpiNow2::example_confirmed[1:30]
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
                                 samples = 100, stan_args = list(warmup = 100, cores = 1, chains = 2),
                                 logs = NULL))
  expect_equal(names(out$regional), c("testland", "realland"))
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
  expect_error(regional_epinow(reported_cases = cases, generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              stan_args = list(warmup = 100, cores = 1, chains = 2),
                              max_execution_time = 1, logs = NULL))
  expect_error(regional_epinow(reported_cases = cases, generation_time = generation_time,
                               delays = list(incubation_period, reporting_delay),
                               stan_args = list(warmup = 100, cores = 1, chains = 2),
                               max_execution_time = 1, future = TRUE, logs = NULL))

})
