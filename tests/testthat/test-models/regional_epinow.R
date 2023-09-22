#' get example delays
cases <- EpiNow2::example_confirmed[1:30]
cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]
))

# static model
fit <- regional_epinow(
  cases,
  example_generation_time,
  delays = delay_opts(example_incubation_period),
  stan = stan_opts(chains = 2, warmup = 200, samples = 1000),
  output = "region",
  gp = NULL
)

saveRDS(fit, "tests/testthat/test-models/regional_epinow/static.rds")
saveRDS(cases, "tests/testthat/test-models/regional_epinow/cases.rds")
