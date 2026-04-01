# Synthetic recovery benchmark for estimate_dist()
#
# Simulates censored delay data with known parameters, fits
# lognormal and gamma distributions, and checks recovery.
library(EpiNow2)
library(primarycensored)
library(data.table)

old_opts <- options()
options(mc.cores = 4)

set.seed(42)

# --- True parameters ---
true_lnorm <- list(meanlog = 1.5, sdlog = 0.7)
true_gamma <- list(shape = 3.0, rate = 0.8)

# --- Simulate censored delay data ---
simulate_linelist <- function(n, rdist, dist_args,
                              pwindow = 1, D = 30) {
  delays <- do.call(rprimarycensored, c(
    list(n = n, rdist = rdist, pwindow = pwindow, D = D),
    dist_args
  ))
  # Random primary event dates over a 60-day window
  pdate_lwr <- as.Date("2023-01-01") + sample(0:59, n, TRUE)
  data.frame(
    pdate_lwr = pdate_lwr,
    sdate_lwr = pdate_lwr + delays,
    obs_date = pdate_lwr + D
  )
}

n_obs <- 500

cat("Simulating", n_obs, "lognormal delays...\n")
ll_lnorm <- simulate_linelist(
  n_obs, rlnorm, true_lnorm,
  D = 30
)

cat("Simulating", n_obs, "gamma delays...\n")
ll_gamma <- simulate_linelist(
  n_obs, rgamma, true_gamma,
  D = 30
)

# --- Fit models ---
stan_args <- stan_opts(
  control = list(adapt_delta = 0.95)
)

cat("\nFitting lognormal model...\n")
fit_lnorm <- estimate_dist(
  ll_lnorm,
  dist = "lognormal",
  stan = stan_args,
  verbose = TRUE
)

cat("\nFitting gamma model...\n")
fit_gamma <- estimate_dist(
  ll_gamma,
  dist = "gamma",
  stan = stan_args,
  verbose = TRUE
)

# --- Extract posterior samples ---
extract_params <- function(fit, param_names) {
  samples <- extract_samples(fit$fit, pars = "params")
  post <- samples$params
  result <- lapply(seq_along(param_names), function(i) {
    list(
      mean = mean(post[, i]),
      sd = sd(post[, i]),
      q025 = quantile(post[, i], 0.025),
      q975 = quantile(post[, i], 0.975)
    )
  })
  names(result) <- param_names
  result
}

lnorm_params <- extract_params(
  fit_lnorm, c("meanlog", "sdlog")
)
gamma_params <- extract_params(
  fit_gamma, c("shape", "rate")
)

# --- Check convergence ---
get_diagnostics <- function(fit) {
  sampler <- rstan::get_sampler_params(fit$fit, inc_warmup = FALSE)
  divergent <- sum(sapply(sampler, function(x) {
    sum(x[, "divergent__"])
  }))
  max_treedepth <- max(sapply(sampler, function(x) {
    max(x[, "treedepth__"])
  }))
  rhat <- rstan::summary(fit$fit, pars = "params")$summary
  list(
    n_divergent = divergent,
    max_treedepth = max_treedepth,
    rhat = rhat[, "Rhat"],
    n_eff = rhat[, "n_eff"]
  )
}

diag_lnorm <- get_diagnostics(fit_lnorm)
diag_gamma <- get_diagnostics(fit_gamma)

# --- Build summary table ---
build_row <- function(dist, param, true_val, posterior,
                      diagnostics, idx) {
  data.frame(
    distribution = dist,
    parameter = param,
    true_value = true_val,
    posterior_mean = posterior[[param]]$mean,
    posterior_sd = posterior[[param]]$sd,
    ci_lower = posterior[[param]]$q025,
    ci_upper = posterior[[param]]$q975,
    covers_true = (
      true_val >= posterior[[param]]$q025 &&
        true_val <= posterior[[param]]$q975
    ),
    rhat = diagnostics$rhat[idx],
    n_eff = diagnostics$n_eff[idx]
  )
}

summary_table <- rbind(
  build_row(
    "lognormal", "meanlog", true_lnorm$meanlog,
    lnorm_params, diag_lnorm, 1
  ),
  build_row(
    "lognormal", "sdlog", true_lnorm$sdlog,
    lnorm_params, diag_lnorm, 2
  ),
  build_row(
    "gamma", "shape", true_gamma$shape,
    gamma_params, diag_gamma, 1
  ),
  build_row(
    "gamma", "rate", true_gamma$rate,
    gamma_params, diag_gamma, 2
  )
)

# --- Print results ---
cat("\n=== Parameter recovery summary ===\n\n")
print(summary_table, digits = 3, row.names = FALSE)

cat("\n=== Convergence diagnostics ===\n")
cat(
  "Lognormal: divergences =", diag_lnorm$n_divergent,
  ", max Rhat =", max(diag_lnorm$rhat), "\n"
)
cat(
  "Gamma: divergences =", diag_gamma$n_divergent,
  ", max Rhat =", max(diag_gamma$rhat), "\n"
)

# Check all 95% CIs cover true values
all_covered <- all(summary_table$covers_true)
cat(
  "\nAll 95% CIs cover true values:",
  ifelse(all_covered, "YES", "NO"), "\n"
)

# --- Save results ---
results <- list(
  summary = summary_table,
  truth = list(
    lognormal = true_lnorm,
    gamma = true_gamma
  ),
  fits = list(
    lognormal = fit_lnorm,
    gamma = fit_gamma
  ),
  diagnostics = list(
    lognormal = diag_lnorm,
    gamma = diag_gamma
  )
)
saveRDS(results, file = "estimate_dist_synthetic.rds")
cat("\nResults saved to estimate_dist_synthetic.rds\n")

options(old_opts)
