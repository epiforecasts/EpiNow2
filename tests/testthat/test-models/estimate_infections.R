# static model
fit <- estimate_infections(
  reported_cases,
  generation_time_opts(example_generation_time),
  delays = delay_opts(example_incubation_period),
  stan = stan_opts(chains = 2, warmup = 200, samples = 1000),
  gp = NULL
)

saveRDS(fit, "tests/testthat/test-models/estimate_infections/static.rds")
