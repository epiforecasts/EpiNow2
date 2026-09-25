# Benchmark the pure Stan update_Rt() against the C++ version.
#
# Run from the package root:
#   Rscript inst/dev/benchmark-update-rt/benchmark-update-rt.R
#
# One chain, one thread, 200 warmup and 300 sampling iterations, seed 1.
# Each profile block calls the function `reps` times, so microseconds per
# call are 1e6 * total_time / autodiff_calls / reps. Each configuration is
# run three times and the median is reported.
library(cmdstanr)

bench_dir <- file.path("inst", "dev", "benchmark-update-rt")
model <- cmdstan_model(
  file.path(bench_dir, "update_rt_bench.stan"),
  include_paths = c(
    file.path("inst", "stan"), file.path("tests", "testthat", "stan")
  ),
  user_header = normalizePath(file.path("inst", "include", "epinow2.hpp")),
  dir = tempdir()
)

reps <- 20
scenario_data <- function(scenario, t) {
  weekly <- (seq_len(t) - 1) %/% 7 + 1
  gp <- scenario %in% c("a", "b", "d")
  bp <- scenario %in% c("c", "d")
  stationary <- as.integer(scenario == "b")
  list(
    t = t,
    n_centre = t - 7,
    stationary = stationary,
    gp_n = if (!gp) 0 else if (stationary) t - 7 else t - 1,
    bp_n = if (bp) ceiling(t / 7) - 1 else 0,
    bps = if (bp) weekly else rep(1, t),
    reps = reps
  )
}

scenarios <- c(
  a = "non-stationary GP", b = "stationary GP", c = "breakpoints",
  d = "non-stationary GP + breakpoints"
)
results <- list()
for (scenario in names(scenarios)) {
  for (t in c(67, 207)) {
    for (run in 1:3) {
      fit <- model$sample(
        data = scenario_data(scenario, t), chains = 1, iter_warmup = 200,
        iter_sampling = 300, seed = 1, refresh = 0, show_messages = FALSE
      )
      p <- fit$profiles()[[1]]
      results[[length(results) + 1]] <- data.frame(
        scenario = scenario, t = t, run = run, block = p$name,
        us = 1e6 * p$total_time / p$autodiff_calls / reps,
        maxdiff = max(fit$draws("maxdiff"))
      )
    }
  }
}
results <- do.call(rbind, results)
print(results)

us <- aggregate(us ~ scenario + t + block, data = results, FUN = median)
us <- reshape(us, idvar = c("scenario", "t"), timevar = "block",
              direction = "wide")
maxdiff <- aggregate(maxdiff ~ scenario + t, data = results, FUN = max)
summary <- merge(us, maxdiff)
summary$description <- scenarios[summary$scenario]
summary$speedup <- summary$us.stan / summary$us.cpp
summary <- summary[order(summary$scenario, summary$t), ]
print(summary, digits = 3, row.names = FALSE)
