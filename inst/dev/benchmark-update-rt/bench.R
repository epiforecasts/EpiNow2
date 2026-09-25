# Benchmark update_Rt() against the pre-rewrite reference.
# Run from the package root: Rscript inst/dev/benchmark-update-rt/bench.R
# Protocol: single chain and thread, 200 warmup and 300 sampling iterations,
# seed 1, 20 calls per profile block, 3 runs per configuration (median).
library(cmdstanr)
set_cmdstan_path(path.expand("~/.cmdstan/cmdstan-2.39.0"))
bench_dir <- "inst/dev/benchmark-update-rt"
source(file.path(bench_dir, "scenarios.R"))
reps <- 20
n_runs <- 3
mod <- cmdstan_model(
  file.path(bench_dir, "bench.stan"),
  include_paths = c(bench_dir, "inst/stan/functions"),
  dir = tempdir(), force_recompile = TRUE
)
res <- list()
for (t in c(67, 207)) {
  for (scenario in c("a", "b", "c", "d")) {
    data <- c(rt_scenario(t, scenario), list(reps = reps))
    for (run in seq_len(n_runs)) {
      fit <- mod$sample(
        data = data, chains = 1, threads_per_chain = NULL, seed = 1,
        iter_warmup = 200, iter_sampling = 300, refresh = 0,
        show_messages = FALSE
      )
      p <- fit$profiles()[[1]]
      res[[length(res) + 1]] <- data.frame(
        t = t, scenario = scenario, run = run, block = p$name,
        us = 1e6 * p$total_time / p$autodiff_calls / reps,
        maxdiff = max(fit$draws("maxdiff"))
      )
    }
  }
}
res <- do.call(rbind, res)
summ <- aggregate(cbind(us, maxdiff) ~ t + scenario + block, res, median)
wide <- reshape(summ[, c("t", "scenario", "block", "us")],
  idvar = c("t", "scenario"), timevar = "block", direction = "wide"
)
wide$speedup <- wide$us.current / wide$us.rewrite
wide$maxdiff <- aggregate(maxdiff ~ t + scenario, res, max)$maxdiff[
  match(paste(wide$t, wide$scenario),
    with(aggregate(maxdiff ~ t + scenario, res, max), paste(t, scenario)))
]
wide$label <- scenario_labels[wide$scenario]
print(res)
print(wide[order(wide$scenario, wide$t), ], digits = 3, row.names = FALSE)
