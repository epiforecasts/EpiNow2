#!/usr/bin/env Rscript
##' Run Stan profiling benchmark for a single version
##'
##' This script runs estimate_infections multiple times and extracts
##' cmdstanr profiling information. Results are saved to a CSV file.
##'
##' Usage: Rscript benchmark-single.R <output_prefix>
##'
##' The output file will be named <output_prefix>-profiles.csv

library("data.table")
library("EpiNow2")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript benchmark-single.R <output_prefix>")
}
output_prefix <- args[1]

## Configuration
n_iter <- 15
seeds <- sample(.Machine$integer.max, n_iter)

## Setup data (same as touchstone/setup.R)
reported_cases <- example_confirmed[1:60]
delays <- delay_opts(example_incubation_period + example_reporting_delay)
fixed_generation_time <- fix_parameters(example_generation_time)

## Compile model with cmdstanr (required for profiling)
## Note: using internal function via ::: because epinow2_cmdstan_model() is not
## exported. This is necessary to get a cmdstanr model object that supports
## profiling via $profiles(). If the internal API changes, this script may need
## updating.
compiled_model <- EpiNow2:::epinow2_cmdstan_model()

message("Running ", n_iter, " iterations for profiling...")

profiles <- lapply(seq_along(seeds), function(i) {
  message("Iteration ", i, "/", n_iter)
  set.seed(seeds[i])

  fit <- suppressMessages(estimate_infections(
    data = reported_cases,
    generation_time = gt_opts(fixed_generation_time),
    delays = delays,
    rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.2)),
    stan = stan_opts(
      samples = 1000, chains = 2, cores = 2,
      object = compiled_model
    ),
    verbose = FALSE
  ))

  profile_data <- rbindlist(fit$fit$profiles(), idcol = "chain")
  profile_data[, iter := i]

  profile_data
})

all_profiles <- rbindlist(profiles)

output_file <- paste0(output_prefix, "-profiles.csv")
fwrite(all_profiles, output_file)
message("Profiles saved to: ", output_file)
