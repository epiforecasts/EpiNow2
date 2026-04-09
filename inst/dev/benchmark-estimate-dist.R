#!/usr/bin/env Rscript
##' Run Stan profiling benchmark for estimate_dist
##'
##' This script runs estimate_dist multiple times and extracts
##' cmdstanr profiling information. Results are saved to a CSV file.
##'
##' Usage: Rscript benchmark-estimate-dist.R <output_prefix>
##'
##' The output file will be named <output_prefix>-dist-profiles.csv

library("data.table")
library("EpiNow2")
library("primarycensored")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript benchmark-estimate-dist.R <output_prefix>")
}
output_prefix <- args[1]

## Configuration
n_iter <- 5
seeds <- sample(.Machine$integer.max, n_iter)

## Simulate censored delay data
set.seed(12345)
n_obs <- 400
true_meanlog <- 1.5
true_sdlog <- 0.7

delays <- rprimarycensored(
  n = n_obs, rdist = rlnorm,
  meanlog = true_meanlog, sdlog = true_sdlog,
  pwindow = 1, D = 30
)

pdate_lwr <- as.Date("2023-01-01") + sample(0:59, n_obs, TRUE)
linelist <- data.frame(
  pdate_lwr = pdate_lwr,
  sdate_lwr = pdate_lwr + delays,
  obs_date = pdate_lwr + 30L
)

## Compile model with cmdstanr (required for profiling)
compiled_model <- epinow2_cmdstan_model("estimate_dist")

message("Running ", n_iter, " iterations for profiling...")

profiles <- lapply(seq_along(seeds), function(i) {
  message("Iteration ", i, "/", n_iter)
  set.seed(seeds[i])

  fit <- suppressMessages(estimate_dist(
    data = linelist,
    dist = "lognormal",
    stan = stan_opts(
      samples = 1000, chains = 2, cores = 2,
      object = compiled_model
    ),
    verbose = FALSE
  ))

  profile_data <- rbindlist(
    fit$fit$profiles(),
    idcol = "chain"
  )
  profile_data[, iter := i]

  profile_data
})

all_profiles <- rbindlist(profiles)

output_file <- paste0(output_prefix, "-dist-profiles.csv")
fwrite(all_profiles, output_file)
message("Profiles saved to: ", output_file)
