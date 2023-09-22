library("EpiNow2")

reported_cases <- example_confirmed[1:60]

fixed_generation_time <- fix_dist(example_generation_time)
fixed_incubation_period <- fix_dist(example_incubation_period)
fixed_reporting_delay <- fix_dist(example_reporting_delay)

delays <- delay_opts(example_incubation_period + example_reporting_delay)
fixed_delays <- delay_opts(fixed_incubation_period + fixed_reporting_delay)
