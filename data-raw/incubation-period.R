library(data.table)

covid_incubation_period <- data.table(
  as_reported = "5.06 (log SD 0.418)",
  mean = 1.621, mean_sd = 0.0640, 
  sd = 0.418, sd_sd = 0.0691,
  dist = "lognorm",
  source = "lauer",
  url = "doi.org/10.7326/M20-0504"
)

usethis::use_data(covid_incubation_period, overwrite = TRUE)

