library(EpiNow2)
library(data.table)
library(here)

## We use the method outlined here:  https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257 # nolint
## to estimate the generation time based on the incubation time estimated
## here: https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported # nolint
## Code for this estimation process is available here: https://github.com/seabbs/COVID19 # nolint
## We assume that a case cannot infect another case on the day of infection.
## Load raw MCMC output
gi <- setDT(readRDS(here("data-raw", "gi.rds")))
## Check mean and standard deviation
example_generation_time <- Gamma(
  mean = Normal(median(gi$mean), sd(gi$mean)),
  sd = Normal(median(gi$sd), sd(gi$sd)),
  max = 14L
)

usethis::use_data(example_generation_time, overwrite = TRUE)
