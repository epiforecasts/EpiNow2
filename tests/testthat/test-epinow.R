context("epinow")

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 15)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 15)

reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(3), 1))
reporting_delay$max <- 15

reported_cases <- EpiNow2::example_confirmed[1:40]

df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}

test_that("epinow produces expected output when run with default settings", {
  skip_on_cran()
  out <- suppressWarnings(epinow(reported_cases = reported_cases, generation_time = generation_time,
                delays = list(incubation_period, reporting_delay),
                gp = list(basis_prop = 0.1, boundary_scale = 2,
                          lengthscale_mean = 20, lengthscale_sd = 2),
                samples = 200, warmup = 100, cores = 1, chains = 2,
                verbose = FALSE))
  
  expect_equal(names(out), c("estimates", "estimated_reported_cases", "summary", "plots"))
  df_non_zero(out$estimates$samples)
  df_non_zero(out$estimates$summarised)
  df_non_zero(out$estimated_reported_cases$samples)
  df_non_zero(out$estimated_reported_cases$summarised)
  df_non_zero(out$summary)
  expect_equal(names(out$plots), c("infections", "reports", "reff", "growth_rate","summary"))
})



test_that("epinow fails as expected when given a short timeout", {
  skip_on_cran()
  expect_error(epinow(reported_cases = reported_cases, generation_time = generation_time,
                delays = list(incubation_period, reporting_delay),
                gp = list(basis_prop = 0.1, boundary_scale = 2,
                          lengthscale_mean = 20, lengthscale_sd = 2),
                samples = 500, warmup = 200, cores = 1, chains = 2,
                verbose = FALSE, max_execution_time = 10))
})
