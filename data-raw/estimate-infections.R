library("EpiNow2")
library("here")

options(mc.cores = 4)

set.seed(12345)

# get example case counts
reported_cases <- example_confirmed[1:60]

#' # use example distributions
reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10L)

example_estimate_infections <- estimate_infections(reported_cases,
  generation_time = gt_opts(example_generation_time),
  delays = delay_opts(example_incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
  stan = stan_opts(samples = 200, control = list(adapt_delta = 0.95))
)

cases <- example_confirmed[1:60]
cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]
))

example_regional_epinow <- regional_epinow(
  generation_time = gt_opts(example_generation_time),
  data = cases,
  delays = delay_opts(example_incubation_period + reporting_delay),
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
