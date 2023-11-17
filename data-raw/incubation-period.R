library(EpiNow2)

## COVID-19 incubation period from Lauer et al.,
## https://doi.org/10.7326/M20-0504

example_incubation_period <- lognormal(
  meanlog = normal(1.621, 0.0640),
  sdlog = normal(0.418, 0.0691),
  max = 14L
)

usethis::use_data(example_incubation_period, overwrite = TRUE)
