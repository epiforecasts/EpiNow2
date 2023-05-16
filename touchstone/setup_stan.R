files <- c(
  "convolve.stan", "pmfs.stan", "observation_model.stan", "secondary.stan",
  "rt.stan", "infections.stan", "delays.stan"
)
suppressMessages(
  expose_stan_fns(
    files,
    target_dir = system.file("stan/functions", package = "EpiNow2")
  )
)

horizon <- 14

seeding_time <- get_seeding_time(delays, generation_time)

backcalc <- backcalc_opts()

reported_cases <- example_confirmed[1:60]

shifted_cases <- create_shifted_cases(
  reported_cases,
  seeding_time,
  backcalc$prior_window,
  horizon
)

data <- create_stan_data(
  reported_cases = reported_cases,
  seeding_time = seeding_time,
  rt = rt,
  gp = gp_opts(),
  obs = obs_opts(),
  backcalc = backcalc,
  shifted_cases = shifted_cases$confirm,
  horizon = horizon
)

data <- c(data, create_stan_delays(
  gt = generation_time,
  delay = delays,
  trunc = trunc_opts(),
  ot = data$t - data$seeding_time - data$horizon
))

init <- create_initial_conditions(data)()

delay_type_max <- get_delay_type_max(
  data$delay_types, data$delay_types_p, data$delay_types_id,
  data$delay_types_groups, data$delay_max, data$delay_np_pmf_groups
)
