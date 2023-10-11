library(EpiNow2)

## example reporting delay

example_reporting_delay <- dist_spec(
  mean = convert_to_logmean(2, 1),
  sd = convert_to_logsd(2, 1),
  max = 10,
  dist = "lognormal"
)

usethis::use_data(example_reporting_delay, overwrite = TRUE)
