## call from browser inside initialisation
files <- c(
  "convolve.stan", "gaussian_process.stan", "pmfs.stan",
  "observation_model.stan", "secondary.stan", "params.stan",
  "rt.stan", "infections.stan", "delays.stan", "generated_quantities.stan"
)
suppressMessages(
  expose_stan_fns(files,
    target_dir = system.file("stan/functions", package = "EpiNow2")
  )
)

simulate <- function(data,
                     generation_time = gt_opts(),
                     delays = delay_opts(),
                     truncation = trunc_opts(),
                     rt = rt_opts(),
                     backcalc = backcalc_opts(),
                     gp = gp_opts(),
                     obs = obs_opts(),
                     forecast = forecast_opts(),
                     stan = stan_opts(),
                     inits = NULL) {

  seeding_time <- get_seeding_time(delays, generation_time, rt)

  reported_cases <- default_fill_missing_obs(data, obs, "confirm")
  if (forecast$horizon > 0) {
    reported_cases <- add_horizon(
      reported_cases, forecast$horizon, forecast$accumulate
    )
  }
  reported_cases <- create_clean_reported_cases(
    reported_cases,
    filter_leading_zeros = TRUE,
    zero_threshold = Inf
  )
  reported_cases <- data.table::rbindlist(list(
    data.table::data.table(
      date = seq(
        min(reported_cases$date) - seeding_time - backcalc$prior_window,
        min(reported_cases$date) - 1,
        by = "days"
      ),
      confirm = 0, accumulate = FALSE, breakpoint = 0
    ),
    reported_cases[, .(date, confirm, accumulate, breakpoint)]
  ))
  shifted_cases <- create_shifted_cases(
    reported_cases,
    seeding_time,
    backcalc$prior_window,
    forecast$horizon
  )
  reported_cases <- reported_cases[-(1:backcalc$prior_window)]

  # Define stan model parameters
  stan_data <- create_stan_data(
    reported_cases,
    seeding_time = seeding_time,
    rt = rt,
    gp = gp,
    obs = obs,
    backcalc = backcalc,
    shifted_cases = shifted_cases$confirm,
    forecast = forecast
  )

  stan_data <- c(stan_data, create_stan_delays(
    gt = generation_time,
    delay = delays,
    trunc = truncation,
    time_points = stan_data$t - stan_data$seeding_time - stan_data$horizon
  ))

  if (is.null(inits)) {
    init <- create_initial_conditions(stan_data)
    inits <- init()
  } else {
    if (stan_data$bp_n == 0) {
      inits$bp_sd <- array(numeric(0))
      inits$bp_effects <- array(numeric(0))
    }
  }
  for (n in names(inits)) assign(n, inits[[n]])
  for (n in names(stan_data)) assign(n, stan_data[[n]])

  ot <- t - seeding_time - horizon
  ot_h <- ot + horizon
  noise_terms <- setup_noise(
    ot_h, t, horizon, estimate_r, stationary, future_fixed, fixed_from
  )
  PHI <- setup_gp(M, L, noise_terms, gp_type == 1, w0)
  delay_type_max <- get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  )
  initial_infections_guess <- max(
    0,
    log(mean(head(cases, ifelse(length(cases) > 7, 7, length(cases)))))
  )
  if (!fixed) {
    alpha <- get_param(
      alpha_id, params_fixed_lookup, params_variable_lookup, params_value,
      params
    )
    rescaled_rho <- 2 * get_param(
      rho_id, params_fixed_lookup, params_variable_lookup,
      params_value, params
    ) / noise_terms
    noise <- update_gp(
      PHI, M, L, alpha, rescaled_rho, eta, gp_type, nu
    )
  } else {
    noise <- numeric(0)
  }
  if (estimate_r) {
    gt_rev_pmf <- get_delay_rev_pmf(
      gt_id, delay_type_max[gt_id] + 1, delay_types_p, delay_types_id,
      delay_types_groups, delay_max, delay_np_pmf,
      delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
      1, 1, 0
    )
    R0 <- get_param(
      R0_id, params_fixed_lookup, params_variable_lookup, params_value, params
    )
    R <- update_Rt(
      ot_h, R0, noise, breakpoints, bp_effects, stationary
    )
    frac_obs <- get_param(
      frac_obs_id, params_fixed_lookup, params_variable_lookup, params_value,
      params
    )
    pop <- get_param(
        pop_id, params_fixed_lookup, params_variable_lookup, params_value,
        params
    )
    infections <- generate_infections(
      R, seeding_time, gt_rev_pmf, initial_infections, pop,
      use_pop, future_time, obs_scale, frac_obs, 1
    )
  } else {
    infections <- deconvolve_infections(
      shifted_cases, noise, fixed, backcalc_prior
    )
  }
  delay_rev_pmf <- get_delay_rev_pmf(
    delay_id, delay_type_max[delay_id] + 1, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
    0, 1, 0
  )
  reports <- convolve_to_report(infections, delay_rev_pmf, seeding_time)
  if (week_effect > 1) {
    reports <- day_of_week_effect(reports, day_of_week, day_of_week_simplex)
  }
  if (obs_scale) {
    frac_obs <- get_param(
      frac_obs_id, params_fixed_lookup, params_variable_lookup, params_value,
      params
    )
    reports <- scale_obs(reports, frac_obs)
  }
  if (trunc_id) {
    trunc_rev_cmf <- get_delay_rev_pmf(
      trunc_id, delay_type_max[trunc_id] + 1, delay_types_p, delay_types_id,
      delay_types_groups, delay_max, delay_np_pmf,
      delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
      0, 1, 1
    )
    obs_reports <- truncate_obs(reports[1:ot], trunc_rev_cmf, 0)
  } else {
    obs_reports <- reports[1:ot]
  }
  if (any_accumulate) {
    obs_reports <- accumulate_reports(obs_reports, accumulate)
  }
  if (!fixed) {
    gaussian_process_lp(eta)
  }
  delays_lp(
    delay_params, delay_params_mean, delay_params_sd, delay_params_groups,
    delay_dist, delay_weight
  )
  params_lp(
    params, prior_dist, prior_dist_params, params_lower, params_upper
  )
  rt_lp(
    initial_infections, bp_effects, bp_sd, bp_n,
    cases, initial_infections_guess
  )
  if (likelihood) {
    dispersion <- get_param(
      dispersion_id, params_fixed_lookup, params_variable_lookup, params_value,
      params
    )
    report_lp(
      cases, case_times, obs_reports, dispersion, model_type, obs_weight
    )
  }
  if (!fixed) {
    rescaled_rho <- get_param(
      rho_id, params_fixed_lookup, params_variable_lookup,
      params_value, params
    )
    x <- seq(1, noise_terms)
    rho <- rescaled_rho * 0.5 * (max(x) - 1)
  }
  return(reports)
}
