# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# installs branches to benchmark
touchstone::branch_install()

#  benchmnark README example
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  default = { epinow(
    reported_cases = reported_cases,
    generation_time = generation_time,
    delays = delays,
    rt = rt,
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  no_delay = { epinow(
    reported_cases = reported_cases,
    generation_time = generation_time,
    rt = rt,
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  nonmechanistic = { epinow(
    reported_cases = reported_cases,
    generation_time = generation_time,
    delays = delays,
    rt = NULL,
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# benchmark individual model components
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  gt_fixed_pmf = {
    combine_pmfs(
      gt_fixed_pmf, gt_mean, gt_sd, data$gt_max, data$gt_dist,
      data$gt_max, 1, 1
    )
  },
  n = 10000
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  delay_var_pmf = {
    combine_pmfs(
      delay_fixed_pmf, init$delay_mean, init$delay_sd, data$delay_max,
      data$delay_dist, delay_max_total, 1, 1
    )
  },
  n = 10000
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  update_gp = {
    update_gp(
      PHI, data$M, data$L, init$alpha[1], init$rho[1], init$eta, data$gp_type
    )
  },
  n = 10000
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  update_Rt = {
    update_Rt(
      ot_h, init$log_R, noise, data$breakpoints, bp_effects, data$stationary
    )
  },
  n = 10000
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  generate_infections = {
    generate_infections(
      R, data$seeding_time, gt_rev_pmf, init$initial_infections,
      init$initial_growth, data$pop, data$future_time
    )
  },
  n = 10000
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  convolve_to_report = {
    convolve_to_report(infections, delay_rev_pmf, data$seeding_time)
  },
  n = 10000
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  day_of_week_effect = {
    day_of_week_effect(reports, data$day_of_week, init$day_of_week_simplex)
  },
  n = 10000
)

touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup_stan.R") },
  report_lp = {
    report_lp(
      data$cases, obs_reports, init$rep_phi, data$phi_mean, data$phi_sd,
      data$model_type, data$obs_weight
    )
  },
  n = 10000
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
