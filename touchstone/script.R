# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# installs branches to benchmark
touchstone::branch_install()

#  benchmnark README example
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  default = { epinow(
    data = reported_cases,
    generation_time = generation_time_opts(fixed_generation_time),
    delays = delay_opts(fixed_delays),
    rt = rt_opts(prior = Normal(mean = 2, sd = 0.2)),
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# benchmark readme example with uncertain delays and gt
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  uncertain = { epinow(
    data = reported_cases,
    generation_time = generation_time_opts(example_generation_time),
    delays = delays,
    rt = rt_opts(prior = Normal(mean = 2, sd = 0.2)),
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# benchmark readme example without delays
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  no_delays = { epinow(
    data = reported_cases,
    generation_time = generation_time_opts(fixed_generation_time),
    rt = rt_opts(prior = Normal(mean = 2, sd = 0.2)),
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# benchmark readme example with a stationary GP
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  stationary = { epinow(
    data = reported_cases,
    generation_time = generation_time_opts(fixed_generation_time),
    delays = delay_opts(fixed_delays),
    rt = rt_opts(prior = Normal(mean = 2, sd = 0.2), gp_on = "R0"),
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# benchmark readme example with a weekly random walk
touchstone::benchmark_run(
  expr_before_benchmark = { source("touchstone/setup.R") },
  random_walk = { epinow(
    data = reported_cases,
    generation_time = generation_time_opts(fixed_generation_time),
    delays = delay_opts(fixed_delays),
    rt = rt_opts(prior = Normal(mean = 2, sd = 0.2), rw = 7),
    gp = NULL,
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
