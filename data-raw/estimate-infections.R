library("EpiNow2")
library("here")

options(mc.cores = 4)

# get example case counts
reported_cases <- example_confirmed[1:60]

# set up example generation time
generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", fixed = TRUE
)
# set delays between infection and case report
incubation_period <- get_incubation_period(
  disease = "SARS-CoV-2", source = "lauer", fixed = TRUE
)
# delays between infection and case report, with uncertainty
incubation_period_uncertain <- get_incubation_period(
  disease = "SARS-CoV-2", source = "lauer"
)
reporting_delay <- dist_spec(
  mean = convert_to_logmean(2, 1), mean_sd = 0,
  sd = convert_to_logsd(2, 1), sd_sd = 0, max = 10
)

# default settings but assuming that delays are fixed rather than uncertain
example_estimate_infections <- estimate_infections(reported_cases,
  generation_time = generation_time_opts(generation_time),
  delays = delay_opts(incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
  stan = stan_opts(samples = 200, control = list(adapt_delta = 0.95))
)

cases <- example_confirmed[1:60]
cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]
))

example_regional_epinow <- regional_epinow(
  reported_cases = cases,
  generation_time = generation_time_opts(generation_time),
  delays = delay_opts(incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
  stan = stan_opts(samples = 200, control = list(adapt_delta = 0.95))
)

saveRDS(
  example_estimate_infections,
  here("inst", "extdata", "example_estimate_infections.rds"),
  compress = "xz"
)
saveRDS(
  example_regional_epinow,
  here("inst", "extdata", "example_regional_epinow.rds"),
  compress = "xz"
)