source("touchstone/setup.R")

dry <- estimate_infections(
  reported_cases = reported_cases,
  generation_time = generation_time,
  delays = delays,
  rt = rt,
  dry_run = TRUE
)

init <- dry$args$init()
data <- dry$args$data

files <- c(
  "convolve.stan", "pmfs.stan", "observation_model.stan", "secondary.stan",
  "rt.stan", "infections.stan", "gaussian_process.stan"
)
suppressMessages(
  expose_stan_fns(
    files,
    target_dir = system.file("stan/functions", package = "EpiNow2")
  )
)

gt_fixed_pmf <- discretised_pmf(
  data$gt_mean_mean[1], data$gt_sd_mean[1], data$gt_max[1], data$gt_dist[1], 1
)
gt_mean <- numeric(0)
gt_sd <- numeric(0)
gt_rev_pmf <- combine_pmfs(
  gt_fixed_pmf, gt_mean, gt_sd, data$gt_max, data$gt_dist,
  data$gt_max, 1, 1
)

delay_fixed_pmf <- discretised_pmf(
  data$delay_mean_mean[2], data$delay_sd_mean[2], data$delay_max[2],
  data$delay_dist[2], 1
)
delay_max_total <- sum(data$delay_max) - length(data$delay_max) + 1
delay_rev_pmf <- combine_pmfs(
  delay_fixed_pmf, init$delay_mean, init$delay_sd, data$delay_max,
  data$delay_dist, delay_max_total, 1, 1
)

ot <- data$t - data$seeding_time - data$horizon
ot_h <- ot + data$horizon
noise_terms <- setup_noise(
  ot_h, data$t, data$horizon, data$estimate_r, data$stationary,
  data$future_fixed, data$fixed_from
)
PHI <- setup_gp(data$M, data$L, noise_terms)
noise <- update_gp(
  PHI, data$M, data$L, init$alpha[1], init$rho[1], init$eta, data$gp_type
)
bp_effects <- numeric(0)

R <- update_Rt(
  ot_h, init$log_R, noise, data$breakpoints, bp_effects, data$stationary
)

infections <- generate_infections(
  R, data$seeding_time, gt_rev_pmf, init$initial_infections,
  init$initial_growth, data$pop, data$future_time
)

reports <- convolve_to_report(infections, delay_rev_pmf, data$seeding_time)

week_effect_reports <- day_of_week_effect(
  reports, data$day_of_week, init$day_of_week_simplex
)

obs_reports <- week_effect_reports[1:ot]
