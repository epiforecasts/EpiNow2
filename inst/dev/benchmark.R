devtools::load_all()
library("dplyr")
library("tibble")
library("tidyr")
library("here")

## number of times to run estimate_infections
n_iter <- 100
## number of bootstrap replicates
n_boot <- 100

## random seeds
seeds <- sample(.Machine$integer.max, n_iter)

source(here::here("touchstone", "setup.R"))
source(here::here("inst", "dev", "benchmark-functions.R"))

profiles <- list()

## generate profiles for different versions of the stan model
profiles[["main"]] <- create_profiles("estimate_infections", seeds)
profiles[["exp"]] <- create_profiles("estimate_infections_exp", seeds)
profiles[["zero"]] <- create_profiles("estimate_infections_zero", seeds)

## merge profiles from the two chains into one, round total time and only kep
## name and total_time columns; then average across chains; sort by time from
## longest to shortest
summary <- profiles |>
  dplyr::bind_rows(.id = "model") |>
  # group by model and name
  dplyr::group_by(model, name) %>%
  # generate bootstrap samples and calculate mean and standard error for each 
  # sample
  dplyr::summarise(
    bootstrap = bootci(total_time, n_boot = n_boot), .groups = "drop"
  ) |>
  tidyr::unnest(cols = c(bootstrap)) |>
  dplyr::arrange(desc(mean))

