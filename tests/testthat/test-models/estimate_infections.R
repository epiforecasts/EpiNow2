
reported_cases <- EpiNow2::example_confirmed[1:30]
generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", max_value = 10
)
incubation_period <- get_incubation_period(
  disease = "SARS-CoV-2", source = "lauer", max_value = 10
)

# static model
fit <- estimate_infections(
  reported_cases,
  generation_time,
  delays = delay_opts(incubation_period),
  stan = stan_opts(chains = 2, warmup = 200, samples = 1000),
  gp = NULL
)

saveRDS(fit, "tests/testthat/test-models/estimate_infections/static.rds")
