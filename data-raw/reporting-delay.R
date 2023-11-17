library(EpiNow2)

## example reporting delay

example_reporting_delay <- lognormal(mean = 2, sd = 1, max = 10L)

usethis::use_data(example_reporting_delay, overwrite = TRUE)
