library(data.table)
library(magrittr)

## We use the method outlined here:  https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257
## to estimate the generation time based on the incubation time estimated 
## here: https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported
## Code for this estimation process is available here: https://github.com/seabbs/COVID19
## We assume that a case cannot infect another case on the day of infection.
## Load raw MCMC output
gi <- data.table::setDT(readRDS("data-raw/gi.rds"))
## Check mean and standard deviation
covid_generation_times_summary <-
  gi[, .(mean = median(mean), mean_sd = sd(mean), 
       sd = median(sd), sd_sd = sd(sd))]

covid_generation_times <- 
  covid_generation_times_summary[, `:=`(
  as_reported = "3.64 (SD 3.08)",
  dist = "gamma",
  source = "ganyani",
  url = "https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257"
)]

covid_generation_times


usethis::use_data(covid_generation_times, overwrite = TRUE)


