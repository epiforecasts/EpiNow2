library(data.table)

## We use the method outlined here:  https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257 # nolint
## to estimate the generation time based on the incubation time estimated
## here: https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported # nolint
## Code for this estimation process is available here: https://github.com/seabbs/COVID19 # nolint
## We assume that a case cannot infect another case on the day of infection.
## Load raw MCMC output
gi <- data.table::setDT(readRDS(file.path("data-raw", "gi.rds")))
## Check mean and standard deviation
covid_generation_times_summary <-
  gi[, .(
    mean = median(mean), mean_sd = sd(mean),
    sd = median(sd), sd_sd = sd(sd)
  )]

generation_times <-
  covid_generation_times_summary[, `:=`(
    as_reported = "3.64 (SD 3.08)",
    dist = "gamma",
    disease = "SARS-CoV-2",
    source = "ganyani",
    url = "https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257" # nolint
  )]

generation_times


usethis::use_data(generation_times, overwrite = TRUE)
