# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# installs branches to benchmark
touchstone::branch_install()

#  benchmnark README example
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  renewal = { epinow(
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
  expr_before_benchmark = {
    source("touchstone/ssetup.R")
    source("touchstone/setup_stan.R")
  },
  gt_pmf = get_delay_rev_pmf(
    data$gt_id, delay_type_max[data$gt_id], data$delay_types_p,
    data$delay_types_id, data$delay_types_groups, data$delay_max,
    data$delay_np_pmf, data$delay_np_pmf_groups, init$delay_mean,
    init$delay_sd, data$delay_dist, 1, 1, 0
  ),
  n = 100
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
