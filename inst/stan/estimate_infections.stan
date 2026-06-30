functions {
#include functions/primarycensored.stan
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/gaussian_process.stan
#include functions/state.stan
#include functions/rt.stan
#include functions/infections.stan
#include functions/observation_model.stan
#include functions/generated_quantities.stan
#include functions/params.stan
}

data {
#include data/observations.stan
#include data/delays.stan
#include data/rt.stan
#include data/backcalc.stan
#include data/observation_model.stan
#include data/params.stan
#include data/estimate_infections_params.stan
#include data/states.stan
}

transformed data {
  // observations
  int ot = t - seeding_time - horizon;  // observed time
  int ot_h = ot + horizon;  // observed time + forecast horizon

  array[delay_types] int delay_type_max;
  profile("assign max") {
    delay_type_max = get_delay_type_max(
      delay_types, delay_types_p, delay_types_id,
      delay_types_groups, delay_max, delay_np_pmf_groups
    );
  }

  // initial infections scaling (on the log scale)
  real initial_infections_guess;
  if (num_elements(cases) > 0) {
    initial_infections_guess = fmax(
      0,
      log(mean(head(cases, num_elements(cases) > 7 ? 7 : num_elements(cases))))
    );
  } else {
    initial_infections_guess = 0;
  }

  // Time-varying states. The observation window runs to the end of the data
  // (ot under the renewal model, t - horizon under back-calculation); the full
  // window additionally spans the forecast horizon.
  int state_data_window = estimate_r ? ot : (t - horizon);
  int state_full_window = estimate_r ? ot_h : t;
  // The free-noise window is where the state varies before being held constant.
  // "project" (state_future_fixed = 0) lets it vary over the whole horizon;
  // otherwise it is fixed from `state_future_from` relative to the data end.
  int state_obs;
  if (state_future_fixed == 0) {
    state_obs = state_full_window;
  } else {
    state_obs = state_data_window + state_future_from;
    if (state_obs < 1) state_obs = 1;
    if (state_obs > state_full_window) state_obs = state_full_window;
  }
  // Init-anchored states are centred over the observation window (never beyond
  // it), so the identifiability anchor is invariant to projection.
  int state_centre =
    state_data_window < state_obs ? state_data_window : state_obs;
  // number of random walk steps per time-varying parameter state (the value is
  // held constant for `state_rw_period` time points between steps)
  int state_rw_n =
    state_obs > 1 ? to_int(ceil(1.0 * state_obs / state_rw_period)) - 1 : 0;
  // basis functions and basis matrix for gaussian process states (shared)
  int gp_M = n_gp_states > 0 ? to_int(ceil(state_obs * gp_basis_prop)) : 0;
  matrix[n_gp_states > 0 ? state_obs : 0, gp_M] gp_PHI;
  if (n_gp_states > 0) {
    gp_PHI = setup_gp(gp_M, gp_boundary_scale, state_obs, 0, 1.0);
  }
}

parameters {
  vector<lower = params_lower, upper = params_upper>[n_params_variable] params;
  array[estimate_r] real initial_infections;    // seed infections
  // standard deviation of breakpoint effect
  array[bp_n > 0 ? 1 : 0] real<lower = 0> bp_sd;
  vector[bp_n] bp_effects;                   // Rt breakpoint effects
  // delay parameters
  vector<lower = delay_params_lower>[delay_params_length] delay_params;
  // raw gamma values for estimated nonparametric delay PMFs;
  // normalised within each ragged segment to give a Dirichlet draw
  vector<lower = 0>[delay_np_est_length] delay_np_est_raw;
  simplex[week_effect] day_of_week_simplex; // day of week reporting effect
  // time-varying parameter states
  vector[n_rw_states * state_rw_n] state_rw_steps; // random walk steps
  vector<lower = 0>[n_rw_states] state_rw_sd;      // random walk step sd
  vector[n_gp_states * gp_M] state_gp_eta;         // GP basis coefficients
  vector<lower = 0>[n_gp_states] state_gp_alpha;   // GP magnitude
  vector<lower = 0>[n_gp_states] state_gp_rho;     // GP lengthscale
}

transformed parameters {
  // combined fixed + estimated nonparametric delay PMF
  vector[delay_np_pmf_length] delay_np_pmf_use = combine_np_pmf(
    delay_np_pmf, delay_n_np_est, delay_np_est_groups,
    delay_np_est_pos, delay_np_est_raw
  );

  vector<lower = 0>[estimate_r > 0 ? ot_h : 0] R; // reproduction number
  vector[t] infections; // latent infections
  vector[ot_h] reports; // estimated reported cases
  vector[ot] obs_reports; // observed estimated reported cases
  vector[estimate_r * (delay_type_max[delay_id_generation_time] + 1)]
    gt_rev_pmf;

  // trajectory of the (possibly time-varying) fraction observed; constant when
  // no state is attached to fraction_observed
  vector[ot_h] fraction_observed_traj = get_state_trajectory(
    param_id_fraction_observed, ot_h, state_obs, state_centre,
    get_param(
      param_id_fraction_observed, params_fixed_lookup, params_variable_lookup,
      params_value, params
    ),
    state_param_id, state_type, state_link, state_pos, state_anchor,
    state_rw_steps, state_rw_n, state_rw_period,
    state_gp_eta, gp_M, gp_PHI, gp_boundary_scale, gp_kernel, gp_nu,
    state_gp_alpha, state_gp_rho
  );
  // trajectory of the (possibly time-varying) reporting overdispersion
  vector[ot_h] reporting_overdispersion_traj = get_state_trajectory(
    param_id_reporting_overdispersion, ot_h, state_obs, state_centre,
    get_param(
      param_id_reporting_overdispersion, params_fixed_lookup,
      params_variable_lookup, params_value, params
    ),
    state_param_id, state_type, state_link, state_pos, state_anchor,
    state_rw_steps, state_rw_n, state_rw_period,
    state_gp_eta, gp_M, gp_PHI, gp_boundary_scale, gp_kernel, gp_nu,
    state_gp_alpha, state_gp_rho
  );

  // Estimate latent infections
  if (estimate_r) {
    profile("gt") {
      gt_rev_pmf = get_delay_rev_pmf(
        delay_id_generation_time, delay_type_max[delay_id_generation_time] + 1,
        delay_types_p, delay_types_id, delay_types_groups, delay_max,
        delay_np_pmf_use, delay_np_pmf_groups, delay_params, delay_params_groups,
        delay_dist, 1, 1, 0
      );
    }
    profile("Rt") {
      // Rt is the trajectory of the R state (constant, or a GP/RW). The sampled
      // R parameter is its level; for an init-anchored state the user prior is
      // applied to the derived initial Rt (R[1]) by the state machinery.
      R = get_state_trajectory(
        param_id_R, ot_h, state_obs, state_centre,
        get_param(
          param_id_R, params_fixed_lookup, params_variable_lookup,
          params_value, params
        ),
        state_param_id, state_type, state_link, state_pos, state_anchor,
        state_rw_steps, state_rw_n, state_rw_period,
        state_gp_eta, gp_M, gp_PHI, gp_boundary_scale, gp_kernel, gp_nu,
        state_gp_alpha, state_gp_rho
      );
    }
    profile("infections") {
      real pop = get_param(
        param_id_pop, params_fixed_lookup, params_variable_lookup, params_value,
        params
      );
      infections = generate_infections(
        R, seeding_time, gt_rev_pmf, initial_infections, pop, use_pop, pop_floor,
        future_time, obs_scale, fraction_observed_traj[1], 1
      );
    }
  } else {
    // back-calculation: latent infections are a Gaussian process on the log
    // scale anchored at an initial value (I0), held at their last value through
    // the forecast horizon
    profile("infections") {
      infections = get_state_trajectory(
        param_id_I, t, state_obs, state_centre,
        get_param(
          param_id_I, params_fixed_lookup, params_variable_lookup,
          params_value, params
        ),
        state_param_id, state_type, state_link, state_pos, state_anchor,
        state_rw_steps, state_rw_n, state_rw_period,
        state_gp_eta, gp_M, gp_PHI, gp_boundary_scale, gp_kernel, gp_nu,
        state_gp_alpha, state_gp_rho
      );
    }
  }

  // initial value of each state's trajectory (for init-anchor priors), reusing
  // the trajectories computed above rather than recomputing them
  vector[n_states] state_init;
  for (s in 1:n_states) {
    int id = state_param_id[s];
    if (id == param_id_R) {
      state_init[s] = R[1];
    } else if (id == param_id_I) {
      state_init[s] = infections[1];
    } else if (id == param_id_fraction_observed) {
      state_init[s] = fraction_observed_traj[1];
    } else if (id == param_id_reporting_overdispersion) {
      state_init[s] = reporting_overdispersion_traj[1];
    } else {
      reject("no trajectory available for state parameter id ", id);
    }
  }

  // convolve from latent infections to mean of observations
  if (delay_id_reporting) {
    vector[delay_type_max[delay_id_reporting] + 1] reporting_rev_pmf;
    profile("delays") {
      reporting_rev_pmf = get_delay_rev_pmf(
        delay_id_reporting, delay_type_max[delay_id_reporting] + 1,
        delay_types_p, delay_types_id, delay_types_groups, delay_max,
        delay_np_pmf_use, delay_np_pmf_groups, delay_params, delay_params_groups,
        delay_dist, 0, 1, 0
      );
    }
    profile("reports") {
      reports = convolve_to_report(infections, reporting_rev_pmf, seeding_time);
    }
  } else {
    reports = infections[(seeding_time + 1):t];
  }

  // weekly reporting effect
  if (week_effect > 1) {
    profile("day of the week") {
      reports = day_of_week_effect(reports, day_of_week, day_of_week_simplex);
    }
  }

  // scaling of reported cases by fraction observed
  if (obs_scale) {
    profile("scale") {
      reports = reports .* fraction_observed_traj;
    }
  }

  // truncate near time cases to observed reports
  if (delay_id_truncation) {
    vector[delay_type_max[delay_id_truncation] + 1] trunc_rev_cmf;
    profile("truncation") {
      trunc_rev_cmf = get_delay_rev_pmf(
        delay_id_truncation, delay_type_max[delay_id_truncation] + 1,
        delay_types_p, delay_types_id, delay_types_groups, delay_max,
        delay_np_pmf_use, delay_np_pmf_groups, delay_params, delay_params_groups,
        delay_dist, 0, 1, 1
      );
    }
    profile("truncate") {
      obs_reports = truncate_obs(reports[1:ot], trunc_rev_cmf, 0);
    }
  } else {
    obs_reports = reports[1:ot];
  }

  // accumulate reports
  if (any_accumulate) {
     profile("accumulate") {
       obs_reports = accumulate_reports(obs_reports, accumulate);
    }
  }
}

model {
  // penalized priors for delay distributions
  profile("delays lp") {
    delays_lp(
      delay_params, delay_params_mean, delay_params_sd, delay_params_groups,
      delay_dist, delay_weight
    );
    delays_np_lp(delay_np_est_raw, delay_np_est_alpha);
  }

  // parameter priors
  profile("param lp") {
    params_lp(
      params, prior_dist, prior_dist_params, params_lower, params_upper,
      params_prior_skip
    );
  }

  // priors for time-varying parameter states
  profile("state lp") {
    for (r in 1:n_rw_states) {
      apply_prior_lp(
        state_rw_sd[r], rw_sd_dist[r],
        rw_sd_dist_params[2 * r - 1], rw_sd_dist_params[2 * r],
        0, positive_infinity()
      );
      segment(state_rw_steps, (r - 1) * state_rw_n + 1, state_rw_n) ~
        normal(0, state_rw_sd[r]);
    }
    for (g in 1:n_gp_states) {
      apply_prior_lp(
        state_gp_alpha[g], gp_alpha_dist[g],
        gp_alpha_dist_params[2 * g - 1], gp_alpha_dist_params[2 * g],
        0, positive_infinity()
      );
      apply_prior_lp(
        state_gp_rho[g], gp_rho_dist[g],
        gp_rho_dist_params[2 * g - 1], gp_rho_dist_params[2 * g],
        0, positive_infinity()
      );
      segment(state_gp_eta, (g - 1) * gp_M + 1, gp_M) ~ std_normal();
    }
    // init-anchor states: user prior on the derived initial value (with the
    // log-link Jacobian), the level itself being free scaffolding
    for (s in 1:n_states) {
      if (state_anchor[s]) {
        apply_prior_lp(
          state_init[s], state_init_dist[s],
          state_init_dist_params[2 * s - 1], state_init_dist_params[2 * s],
          state_init_lower[s], state_init_upper[s]
        );
        if (state_link[s] == 0) {
          target += log(state_init[s]);
        }
      }
    }
  }

  if (estimate_r) {
    // priors on Rt
    profile("rt lp") {
      rt_lp(
        initial_infections, bp_effects, bp_sd, bp_n,
        cases, initial_infections_guess
      );
    }
  }

  profile("init lp") {
    init_priors_lp(init_param_ids, init_dists, init_dist_params,
                   init_lower, init_upper, param_id_R, R);
  }

  // observed reports from mean of reports (update likelihood)
  if (likelihood) {
    profile("report lp") {
      report_lp(
        cases, case_times, obs_reports, reporting_overdispersion_traj[1:ot],
        model_type, obs_weight
      );
    }
  }
}

generated quantities {
  array[it] int imputed_reports;
  vector[estimate_r > 0 ? 0 : ot_h] gen_R;
  vector[ot_h - 1] r;
  vector[return_likelihood ? ot : 0] log_lik;
  // Adjusted Rt accounting for susceptible depletion (only when use_pop > 0)
  vector[(estimate_r > 0 && use_pop > 0) ? ot_h : 0] R_adj;

  profile("generated quantities") {
    {
      vector[delay_type_max[delay_id_generation_time] + 1]
        gt_rev_pmf_for_growth;

      if (estimate_r == 0) {
        // sample generation time
        vector[delay_params_length] delay_params_sample = to_vector(normal_lb_rng(
          delay_params_mean, delay_params_sd, delay_params_lower
        ));
        vector[delay_type_max[delay_id_generation_time] + 1] sampled_gt_rev_pmf =
          get_delay_rev_pmf(
            delay_id_generation_time,
            delay_type_max[delay_id_generation_time] + 1, delay_types_p,
            delay_types_id, delay_types_groups, delay_max, delay_np_pmf_use,
            delay_np_pmf_groups, delay_params_sample, delay_params_groups,
            delay_dist, 1, 1, 0
          );
        gt_rev_pmf_for_growth = sampled_gt_rev_pmf;
        // calculate Rt using infections and generation time
        gen_R = calculate_Rt(
          infections, seeding_time, sampled_gt_rev_pmf, rt_half_window
        );
      } else {
        gt_rev_pmf_for_growth = gt_rev_pmf;
      }
  
      // estimate growth from infections
      r = calculate_growth(
        infections, seeding_time, gt_rev_pmf_for_growth, growth_method
        );

      // Calculate adjusted Rt when population adjustment is enabled
      // R_adj = infections / infectiousness (back-calculated from adjusted infections)
      if (estimate_r > 0 && use_pop > 0) {
        R_adj = calculate_Rt(infections, seeding_time, gt_rev_pmf, 0);
      }
    }

    // simulate reported cases
    if (any_accumulate) {
      vector[ot_h] accumulated_reports =
        accumulate_reports(reports, accumulate);
      imputed_reports = report_rng(
        accumulated_reports[imputed_times],
        reporting_overdispersion_traj[imputed_times], model_type
      );
    } else {
      imputed_reports = report_rng(
        reports[imputed_times], reporting_overdispersion_traj[imputed_times],
        model_type
      );
    }

    // log likelihood of model
    if (return_likelihood) {
      log_lik = report_log_lik(
        cases, obs_reports[case_times], reporting_overdispersion_traj[case_times],
        model_type, obs_weight
      );
    }
  }
}
