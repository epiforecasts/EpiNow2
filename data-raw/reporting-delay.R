library(EpiNow2)

## example reporting delay

example_reporting_delay <- LogNormal(
  meanlog = Normal(0.6, 0.06), sdlog = Normal(0.5, 0.05), max = 10L
)

usethis::use_data(example_reporting_delay, overwrite = TRUE)
