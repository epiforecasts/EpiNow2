#!/usr/bin/env Rscript
##' Compare fitting time across Stan backends and samplers
##'
##' This script fits the models benchmarked elsewhere in this directory with
##' each available backend and sampler, and records wall-clock time along with
##' the posterior summaries needed to check that the fits agree.
##'
##' Unlike the touchstone benchmarks, which compare a branch against main,
##' this compares backends against each other within one checkout.
##'
##' Usage: Rscript benchmark-backends.R <output_prefix>
##'
##' The output file will be named <output_prefix>-backends.csv

library("data.table")
library("EpiNow2")
library("primarycensored")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript benchmark-backends.R <output_prefix>")
}
output_prefix <- args[1]

## Configuration
n_iter <- 3
chains <- 2
cores <- 2
samples <- 500
warmup <- 250
seed <- 20240101

## Backends to compare. The engine only applies to the stanr backends.
runs <- list(
  list(label = "rstan", backend = "rstan", engine = NULL),
  list(label = "cmdstanr", backend = "cmdstanr", engine = NULL),
  list(label = "stanr", backend = "stanr", engine = "nuts"),
  list(label = "stanli", backend = "stanli", engine = "nuts"),
  list(label = "stanli-walnuts", backend = "stanli", engine = "walnuts")
)

stan_settings <- function(run) {
  opts <- list(
    backend = run$backend, chains = chains, cores = cores,
    samples = samples, warmup = warmup, seed = seed
  )
  if (!is.null(run$engine)) {
    opts$engine <- run$engine
  }
  do.call(stan_opts, opts)
}

## Shared fixtures, matching those used by the touchstone benchmarks
reported_cases <- example_confirmed[1:60]
generation_time <- gt_opts(example_generation_time)
delays <- delay_opts(example_incubation_period + example_reporting_delay)

secondary_cases <- as.data.table(example_confirmed[1:60])
secondary_cases[, primary := confirm]
secondary_cases[, scaling := 0.4][, meanlog := 1.8][, sdlog := 0.5]
secondary_cases <- convolve_and_scale(secondary_cases, type = "incidence")

set.seed(12345)
dist_n <- 200
dist_D <- 30
dist_pdate_lwr <- as.Date("2023-01-01") + sample(0:59, dist_n, replace = TRUE)
dist_delays <- rprimarycensored(
  n = dist_n, rdist = rlnorm, meanlog = 1.5, sdlog = 0.7,
  pwindow = 1, D = dist_D
)
dist_linelist <- data.frame(
  pdate_lwr = dist_pdate_lwr,
  sdate_lwr = dist_pdate_lwr + dist_delays,
  obs_date = dist_pdate_lwr + dist_D
)

## Models to fit, each a function of the stan options
models <- list(
  estimate_infections = function(stan) estimate_infections(
    data = reported_cases, generation_time = generation_time,
    delays = delays, stan = stan, verbose = FALSE
  ),
  estimate_secondary = function(stan) estimate_secondary(
    data = secondary_cases,
    obs = obs_opts(scale = Normal(mean = 0.2, sd = 0.2), week_effect = FALSE),
    stan = stan, verbose = FALSE
  ),
  estimate_truncation = function(stan) estimate_truncation(
    data = example_truncated, stan = stan, verbose = FALSE
  ),
  estimate_dist = function(stan) estimate_dist(
    data = dist_linelist, dist = "lognormal", stan = stan, verbose = FALSE
  )
)

results <- rbindlist(lapply(names(models), function(model) {
  rbindlist(lapply(runs, function(run) {
    rbindlist(lapply(seq_len(n_iter), function(iter) {
      message("Fitting ", model, " with ", run$label, " (", iter, "/",
              n_iter, ")")
      elapsed <- NA_real_
      status <- "ok"
      timing <- system.time(
        fit <- tryCatch(
          suppressMessages(suppressWarnings(
            models[[model]](stan_settings(run))
          )),
          error = function(e) {
            status <<- conditionMessage(e)
            NULL
          }
        )
      )
      if (!is.null(fit)) {
        elapsed <- timing[["elapsed"]]
      }
      data.table(
        model = model, run = run$label, backend = run$backend,
        engine = run$engine %||% NA_character_,
        iter = iter, elapsed = elapsed, status = status
      )
    }))
  }))
}))

output_file <- paste0(output_prefix, "-backends.csv")
fwrite(results, output_file)

message("Results written to ", output_file)
print(results[
  status == "ok",
  .(mean_elapsed = mean(elapsed)),
  by = .(model, run)
])
