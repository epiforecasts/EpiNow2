generation_time <- EpiNow2::get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", max_value = 10
)
incubation_period <- EpiNow2::get_incubation_period(
  disease = "SARS-CoV-2", source = "lauer", max_value = 10
)

cases <- EpiNow2::example_confirmed[1:30]
cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]
))

# static model
fit <- regional_epinow(
  cases,
  generation_time,
  delays = delay_opts(incubation_period),
  stan = stan_opts(chains = 2, warmup = 200, samples = 1000),
  output = "region",
  gp = NULL
)

saveRDS(fit, "tests/testthat/test-models/regional_epinow/static.rds")
saveRDS(cases, "tests/testthat/test-models/regional_epinow/cases.rds")