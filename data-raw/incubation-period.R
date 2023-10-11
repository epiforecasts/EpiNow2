library(EpiNow2)

## COVID-19 incubation period from Lauer et al., https://doi.org/10.7326/M20-0504

example_incubation_period <- dist_spec(
  mean = 1.621,
  mean_sd = 0.0640,
  sd = 0.418,
  sd_sd = 0.0691,
  dist = "lognormal",
  max = 14L
)

usethis::use_data(example_incubation_period, overwrite = TRUE)
