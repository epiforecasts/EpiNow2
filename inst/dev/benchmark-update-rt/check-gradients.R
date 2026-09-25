# Check update_Rt() matches the pre-rewrite reference in value and gradient.
# Run from the package root:
# Rscript inst/dev/benchmark-update-rt/check-gradients.R
library(cmdstanr)
set_cmdstan_path(path.expand("~/.cmdstan/cmdstan-2.39.0"))
bench_dir <- "inst/dev/benchmark-update-rt"
source(file.path(bench_dir, "scenarios.R"))
mod <- cmdstan_model(
  file.path(bench_dir, "grad.stan"),
  include_paths = c(bench_dir, "inst/stan/functions"),
  user_header = normalizePath("inst/include/epinow2.hpp"),
  dir = tempdir(), force_recompile = TRUE
)
# cmdstanr compiles the model methods without user_header, so add the
# header to the model methods code and compile them here.
methods_env <- mod$.__enclos_env__$private$model_methods_env_
code <- methods_env$hpp_code_
at <- match("#include <stan/model/model_header.hpp>", trimws(code))
methods_env$hpp_code_ <- append(
  code, paste0("#include \"", normalizePath("inst/include/epinow2.hpp"), "\""),
  at
)
cmdstanr:::expose_model_methods(methods_env)
# Benchmark scenarios plus edge cases: GP shorter than t - 1 (held forward),
# breakpoints not starting at level 1, non-unit and negative jumps,
# stationary GP with breakpoints and a centring window shorter than the GP.
edge <- list(
  ns_hold = list(t = 30, n_centre = 25, gp_n = 20, bp_n = 0, stationary = 0,
    bps = rep(1, 30)),
  ns_short_centre = list(t = 30, n_centre = 10, gp_n = 29, bp_n = 0,
    stationary = 0, bps = rep(1, 30)),
  stat_full = list(t = 30, n_centre = 23, gp_n = 30, bp_n = 0, stationary = 1,
    bps = rep(1, 30)),
  stat_bp = list(t = 30, n_centre = 23, gp_n = 23, bp_n = 4, stationary = 1,
    bps = floor(seq_len(30) / 7) + 1),
  bp_odd = list(t = 12, n_centre = 9, gp_n = 0, bp_n = 4, stationary = 0,
    bps = c(2, 2, 4, 4, 5, 3, 3, 1, 5, 5, 5, 5)),
  ns_bp_odd = list(t = 12, n_centre = 9, gp_n = 8, bp_n = 4, stationary = 0,
    bps = c(2, 2, 4, 4, 5, 3, 3, 1, 5, 5, 5, 5)),
  ns_bp_hold = list(t = 30, n_centre = 23, gp_n = 20, bp_n = 4,
    stationary = 0, bps = floor(seq_len(30) / 7) + 1)
)
cases <- c(
  unlist(lapply(c(67, 207), function(t) {
    setNames(lapply(c("a", "b", "c", "d"), rt_scenario, t = t),
      paste0(c("a", "b", "c", "d"), "_", t))
  }), recursive = FALSE),
  edge
)
set.seed(1)
out <- list()
for (nm in names(cases)) {
  d <- cases[[nm]]
  d$w <- rnorm(d$t)
  fits <- lapply(0:1, function(v) {
    f <- mod$sample(
      data = c(d, list(version = v)), chains = 1, iter_warmup = 1,
      iter_sampling = 1, refresh = 0, show_messages = FALSE,
      fixed_param = FALSE, seed = 1
    )
    f$init_model_methods()
    f
  })
  n_par <- 1 + d$gp_n + d$bp_n
  for (k in 1:5) {
    upars <- c(rnorm(1, 0, 0.3), rnorm(n_par - 1, 0, 0.1))
    lp <- vapply(fits, function(f) f$log_prob(upars), numeric(1))
    g <- lapply(fits, function(f) f$grad_log_prob(upars))
    out[[length(out) + 1]] <- data.frame(
      case = nm, draw = k, lp_diff = abs(diff(lp)),
      grad_maxdiff = max(abs(g[[1]] - g[[2]]))
    )
  }
}
out <- do.call(rbind, out)
print(aggregate(cbind(lp_diff, grad_maxdiff) ~ case, out, max), digits = 3)
cat("Overall max |lp diff|:", max(out$lp_diff),
  " max |grad diff|:", max(out$grad_maxdiff), "\n")
