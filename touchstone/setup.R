library("EpiNow2")

reported_cases <- example_confirmed[1:60]

# set up example generation time
generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", fixed = TRUE
)

# set delays between infection and case report
incubation_period <- get_incubation_period(
  disease = "SARS-CoV-2", source = "lauer"
)

reporting_delay <- list(
  mean = convert_to_logmean(2, 1), mean_sd = 0,
  sd = convert_to_logsd(2, 1), sd_sd = 0, max = 10
)

if ("dist_spec" %in% class(generation_time)) {
  generation_time <- generation_time_opts(generation_time)
  reporting_delay <- do.call(dist_spec, reporting_delay)
  delays <- delay_opts(c(incubation_period, reporting_delay))
} else {
  delays <- delay_opts(incubation_period, reporting_delay)
}
