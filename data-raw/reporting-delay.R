library(EpiNow2)

## example reporting delay

example_reporting_delay <- lognormal(
  meanlog = normal(0.6, 0.06), sdlog = normal(0.5, 0.05), max = 10L
)

usethis::use_data(example_reporting_delay, overwrite = TRUE)
