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

# benchmark estimate_truncation on the example truncated dataset
# Data is defined inline (not in setup.R) so the fixtures travel with the
# benchmark expression when touchstone checks out the comparison branch,
# whose setup.R may not yet define these variables.
touchstone::benchmark_run(
  expr_before_benchmark = {
    source("touchstone/setup.R")
    truncated_cases <- example_truncated
  },
  estimate_truncation = { estimate_truncation(
    data = truncated_cases,
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# benchmark estimate_secondary on simulated incidence data
touchstone::benchmark_run(
  expr_before_benchmark = {
    source("touchstone/setup.R")
    secondary_cases <- data.table::as.data.table(example_confirmed[1:60])
    secondary_cases[, primary := confirm]
    secondary_cases[, scaling := 0.4]
    secondary_cases[, meanlog := 1.8]
    secondary_cases[, sdlog := 0.5]
    secondary_cases <- convolve_and_scale(
      secondary_cases, type = "incidence"
    )
  },
  estimate_secondary = { estimate_secondary(
    data = secondary_cases,
    obs = obs_opts(
      scale = Normal(mean = 0.2, sd = 0.2), week_effect = FALSE
    ),
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# benchmark estimate_dist on an interval-censored linelist
touchstone::benchmark_run(
  expr_before_benchmark = {
    source("touchstone/setup.R")
    set.seed(12345)
    dist_n <- 200
    dist_D <- 30
    dist_pdate_lwr <- as.Date("2023-01-01") +
      sample(0:59, dist_n, replace = TRUE)
    dist_delays <- primarycensored::rprimarycensored(
      n = dist_n, rdist = rlnorm,
      meanlog = 1.5, sdlog = 0.7,
      pwindow = 1, D = dist_D
    )
    dist_linelist <- data.frame(
      pdate_lwr = dist_pdate_lwr,
      sdate_lwr = dist_pdate_lwr + dist_delays,
      obs_date = dist_pdate_lwr + dist_D
    )
  },
  estimate_dist = { estimate_dist(
    data = dist_linelist,
    dist = "lognormal",
    stan = stan_opts(
      cores = 2, samples = 500, chains = 2,
      control = list(adapt_delta = 0.95)),
    verbose = interactive()
  ) },
  n = 5
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
