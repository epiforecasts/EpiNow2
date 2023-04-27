library(data.table)

incubation_periods <- data.table(
  as_reported = "5.06 (log SD 0.418)",
  mean = 1.621, mean_sd = 0.0640,
  sd = 0.418, sd_sd = 0.0691,
  dist = "lognormal",
  disease = "SARS-CoV-2",
  source = "lauer",
  url = "doi.org/10.7326/M20-0504" # nolint
)

usethis::use_data(incubation_periods, overwrite = TRUE)
