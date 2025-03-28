functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/gaussian_process.stan
#include functions/rt.stan
#include functions/infections.stan
#include functions/observation_model.stan
#include functions/generated_quantities.stan
#include functions/params.stan
}

data {
#include data/observations.stan
#include data/delays.stan
#include data/gaussian_process.stan
#include data/rt.stan
#include data/backcalc.stan
#include data/observation_model.stan
#include data/params.stan
#include data/estimate_infections_params.stan
}

transformed data {
  // observations
  int ot = t - seeding_time - horizon;  // observed time
  int ot_h = ot + horizon;  // observed time + forecast horizon
  // gaussian process
  int noise_terms = setup_noise(
    ot_h, t, horizon, estimate_r, stationary, future_fixed, fixed_from
  );
  matrix[noise_terms, gp_type == 1 ? 2 * M : M] PHI = setup_gp(
    M, L, noise_terms, gp_type == 1, w0
  );  // basis function

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
}

parameters {
  vector<lower = params_lower, upper = params_upper>[n_params_variable] params;
  // gaussian process
  vector[fixed ? 0 : gp_type == 1 ? 2*M : M] eta;  // unconstrained noise
  // Rt
  array[estimate_r] real initial_infections;    // seed infections
  // standard deviation of breakpoint effect
  array[bp_n > 0 ? 1 : 0] real<lower = 0> bp_sd;
  vector[bp_n] bp_effects;                   // Rt breakpoint effects
  // delay parameters
  vector<lower = delay_params_lower>[delay_params_length] delay_params;
  simplex[week_effect] day_of_week_simplex; // day of week reporting effect
}

transformed parameters {
  // noise generated by the gaussian process
  vector[fixed ? 0 : noise_terms] noise;
  vector<lower = 0>[estimate_r > 0 ? ot_h : 0] R; // reproduction number
  vector[t] infections; // latent infections
  vector[ot_h] reports; // estimated reported cases
  vector[ot] obs_reports; // observed estimated reported cases
  vector[estimate_r * (delay_type_max[gt_id] + 1)] gt_rev_pmf;

  // GP in noise - spectral densities
  profile("update gp") {
    if (!fixed) {
      real alpha = get_param(
        param_id_alpha, params_fixed_lookup, params_variable_lookup, params_value,
        params
      );
      real rescaled_rho = 2 * get_param(
        param_id_rho, params_fixed_lookup, params_variable_lookup,
        params_value, params
      ) / noise_terms;
      noise = update_gp(
        PHI, M, L, alpha, rescaled_rho, eta, gp_type, nu
      );
    }
  }

  // Estimate latent infections
  if (estimate_r) {
    profile("gt") {
      gt_rev_pmf = get_delay_rev_pmf(
        gt_id, delay_type_max[gt_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
        1, 1, 0
      );
    }
    profile("R0") {
      real R0 = get_param(
        param_id_R0, params_fixed_lookup, params_variable_lookup, params_value, params
      );
      R = update_Rt(
        ot_h, R0, noise, breakpoints, bp_effects, stationary
      );
    }
    profile("infections") {
      real frac_obs = get_param(
        param_id_frac_obs, params_fixed_lookup, params_variable_lookup, params_value,
        params
      );
      infections = generate_infections(
        R, seeding_time, gt_rev_pmf, initial_infections, pop,
        future_time, obs_scale, frac_obs, 1
      );
    }
  } else {
    // via deconvolution
    profile("infections") {
      infections = deconvolve_infections(
        shifted_cases, noise, fixed, backcalc_prior
      );
    }
  }

  // convolve from latent infections to mean of observations
  if (delay_id) {
    vector[delay_type_max[delay_id] + 1] delay_rev_pmf;
    profile("delays") {
      delay_rev_pmf = get_delay_rev_pmf(
        delay_id, delay_type_max[delay_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
        0, 1, 0
      );
    }
    profile("reports") {
      reports = convolve_to_report(infections, delay_rev_pmf, seeding_time);
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
      real frac_obs = get_param(
        param_id_frac_obs, params_fixed_lookup, params_variable_lookup, params_value,
        params
      );
      reports = scale_obs(reports, frac_obs);
    }
  }

  // truncate near time cases to observed reports
  if (trunc_id) {
    vector[delay_type_max[trunc_id] + 1] trunc_rev_cmf;
    profile("truncation") {
      trunc_rev_cmf = get_delay_rev_pmf(
        trunc_id, delay_type_max[trunc_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
        0, 1, 1
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
  // priors for noise GP
  if (!fixed) {
    profile("gp lp") {
      gaussian_process_lp(eta);
    }
  }

  // penalized priors for delay distributions
  profile("delays lp") {
    delays_lp(
      delay_params, delay_params_mean, delay_params_sd, delay_params_groups,
      delay_dist, delay_weight
    );
  }

  // parameter priors
  profile("param lp") {
    params_lp(
      params, prior_dist, prior_dist_params, params_lower, params_upper
    );
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

  // observed reports from mean of reports (update likelihood)
  if (likelihood) {
    profile("report lp") {
      real dispersion = get_param(
        param_id_dispersion, params_fixed_lookup, params_variable_lookup, params_value,
        params
      );
      report_lp(
        cases, case_times, obs_reports, dispersion, model_type, obs_weight
      );
    }
  }
}

generated quantities {
  array[it] int imputed_reports;
  vector[estimate_r > 0 ? 0 : ot_h] gen_R;
  vector[ot_h - 1] r;
  vector[return_likelihood ? ot : 0] log_lik;

  profile("generated quantities") {
    real dispersion = get_param(
      param_id_dispersion, params_fixed_lookup, params_variable_lookup, params_value,
      params
    );
    if (!fixed) {
      real rescaled_rho = 2 * get_param(
        param_id_rho, params_fixed_lookup, params_variable_lookup,
        params_value, params
      ) / noise_terms;
      vector[noise_terms] x = linspaced_vector(noise_terms, 1, noise_terms);
    }

    if (estimate_r == 0) {
      // sample generation time
      vector[delay_params_length] delay_params_sample = to_vector(normal_lb_rng(
        delay_params_mean, delay_params_sd, delay_params_lower
      ));
      vector[delay_type_max[gt_id] + 1] sampled_gt_rev_pmf = get_delay_rev_pmf(
        gt_id, delay_type_max[gt_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params_sample, delay_params_groups,
        delay_dist, 1, 1, 0
      );
      // calculate Rt using infections and generation time
      gen_R = calculate_Rt(
        infections, seeding_time, sampled_gt_rev_pmf, rt_half_window
      );
    }

    // estimate growth from infections
    r = calculate_growth(infections, seeding_time + 1);

    // simulate reported cases
    if (any_accumulate) {
      vector[ot_h] accumulated_reports =
        accumulate_reports(reports, accumulate);
      imputed_reports = report_rng(
        accumulated_reports[imputed_times], dispersion, model_type
      );
    } else {
      imputed_reports = report_rng(reports, dispersion, model_type);
    }

    // log likelihood of model
    if (return_likelihood) {
      log_lik = report_log_lik(
        cases, obs_reports[case_times], dispersion, model_type, obs_weight
      );
    }
  }
}
