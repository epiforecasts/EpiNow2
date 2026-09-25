# Time the pre-rewrite reference, the plain Stan rewrite and the C++
# update_Rt() side by side in one model, with the protocol of bench.R.
# Run from the package root: Rscript inst/dev/benchmark-update-rt/bench3.R
library(cmdstanr)
set_cmdstan_path(path.expand("~/.cmdstan/cmdstan-2.39.0"))
bench_dir <- "inst/dev/benchmark-update-rt"
source(file.path(bench_dir, "scenarios.R"))
reps <- 20
n_runs <- 3
mod <- cmdstan_model(
  file.path(bench_dir, "bench3.stan"),
  include_paths = c(bench_dir, "inst/stan/functions"),
  user_header = normalizePath("inst/include/epinow2.hpp"),
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
        maxdiff_cpp = max(fit$draws("maxdiff")),
        maxdiff_rewrite = max(fit$draws("maxdiff_rewrite"))
      )
    }
  }
}
res <- do.call(rbind, res)
print(res)
summ <- aggregate(us ~ t + scenario + block, res, median)
wide <- reshape(summ, idvar = c("t", "scenario"), timevar = "block",
  direction = "wide"
)
maxdiff <- aggregate(cbind(maxdiff_rewrite, maxdiff_cpp) ~ t + scenario,
  res, max
)
wide <- merge(wide, maxdiff)
wide$label <- scenario_labels[wide$scenario]
print(wide[order(wide$scenario, wide$t),
  c("scenario", "t", "us.current", "us.rewrite", "us.cpp",
    "maxdiff_rewrite", "maxdiff_cpp", "label")
], digits = 3, row.names = FALSE)
