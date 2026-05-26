library("EpiNow2")

reported_cases <- example_confirmed[1:60]

fixed_generation_time <- fix_parameters(example_generation_time)
fixed_incubation_period <- fix_parameters(example_incubation_period)
fixed_reporting_delay <- fix_parameters(example_reporting_delay)

delays <- delay_opts(example_incubation_period + example_reporting_delay)
fixed_delays <- delay_opts(fixed_incubation_period + fixed_reporting_delay)

# Data for estimate_truncation benchmark
truncated_cases <- example_truncated

# Data for estimate_secondary benchmark
secondary_cases <- data.table::as.data.table(example_confirmed[1:60])
secondary_cases[, primary := confirm]
secondary_cases[, scaling := 0.4]
secondary_cases[, meanlog := 1.8]
secondary_cases[, sdlog := 0.5]
secondary_cases <- convolve_and_scale(secondary_cases, type = "incidence")

# Data for estimate_dist benchmark (interval-censored linelist)
if (requireNamespace("primarycensored", quietly = TRUE)) {
  set.seed(12345)
  dist_n <- 200
  dist_D <- 30
  dist_pdate_lwr <- as.Date("2023-01-01") +
    sample(0:59, dist_n, replace = TRUE)
  dist_delays <- primarycensored::rprimarycensored(
    n = dist_n, rdist = rlnorm,
    meanlog = 1.5, sdlog = 0.7,
    pwindow = 1, D = dist_D
  )
  dist_linelist <- data.frame(
    pdate_lwr = dist_pdate_lwr,
    sdate_lwr = dist_pdate_lwr + dist_delays,
    obs_date = dist_pdate_lwr + dist_D
  )
}
