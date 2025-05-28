functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/observation_model.stan
#include functions/secondary.stan
#include functions/params.stan
}

data {
  int t;                             // total time (length of input data)
  int lt;                            // number of likelihood timepoints
  array[t] int<lower = 0> obs;             // observed secondary data
  array[lt] int obs_time;             // observed secondary data
  vector[t] primary;                 // observed primary data
  int burn_in;                       // time period to not use for fitting
  int any_accumulate; // Should any missing values be accumulated?
  array[t] int accumulate;  // Should missing values be accumulated (by time)
#include data/secondary.stan
#include data/delays.stan
#include data/observation_model.stan
#include data/params.stan
#include data/estimate_secondary_params.stan
}

transformed data{
  array[delay_types] int delay_type_max = get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  );
}

parameters{
  // observation model
  vector<lower = delay_params_lower>[delay_params_length] delay_params;
  simplex[week_effect] day_of_week_simplex;  // day of week reporting effect
  vector<lower = params_lower, upper = params_upper>[n_params_variable] params;
}

transformed parameters {
  vector<lower=0>[t] secondary;
  // calculate secondary reports from primary

  {
    vector[t] scaled;
    vector[t] convolved = rep_vector(1e-5, t);

    // scaling of primary reports by fraction observed
    if (obs_scale) {
      real frac_obs = get_param(
        param_id_frac_obs, params_fixed_lookup, params_variable_lookup, params_value,
        params
      );
      scaled = scale_obs(primary, frac_obs);
    } else {
      scaled = primary;
    }

    if (delay_id) {
      vector[delay_type_max[delay_id] + 1] delay_rev_pmf = get_delay_rev_pmf(
        delay_id, delay_type_max[delay_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
        0, 1, 0
      );
      convolved = convolved + convolve_to_report(scaled, delay_rev_pmf, 0);
    } else {
      convolved = convolved + scaled;
    }

    secondary = calculate_secondary(
      scaled, convolved, obs, cumulative, historic, primary_hist_additive,
      current, primary_current_additive, t
    );
  }

  // weekly reporting effect
  if (week_effect > 1) {
    secondary = day_of_week_effect(secondary, day_of_week, day_of_week_simplex);
  }

  // truncate near time cases to observed reports
  if (trunc_id) {
    vector[delay_type_max[trunc_id]] trunc_rev_cmf = get_delay_rev_pmf(
      trunc_id, delay_type_max[trunc_id] + 1, delay_types_p, delay_types_id,
      delay_types_groups, delay_max, delay_np_pmf,
      delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
      0, 1, 1
    );
    secondary = truncate_obs(secondary, trunc_rev_cmf, 0);
  }

  // accumulate reports
  if (any_accumulate) {
    profile("accumulate") {
      secondary = accumulate_reports(secondary, accumulate);
    }
  }
}

model {
  // penalised priors for delay distributions
  delays_lp(
    delay_params, delay_params_mean, delay_params_sd, delay_params_groups,
    delay_dist, delay_weight
  );

  // parameter priors
  profile("param lp") {
    params_lp(
      params, prior_dist, prior_dist_params, params_lower, params_upper
    );
  }
  // observed secondary reports from mean of secondary reports (update likelihood)
  if (likelihood) {
    real dispersion = get_param(
      param_id_dispersion, params_fixed_lookup, params_variable_lookup, params_value,
      params
    );
    report_lp(
      obs[(burn_in + 1):t][obs_time], obs_time, secondary[(burn_in + 1):t],
      dispersion, model_type, 1
    );
  }
}

generated quantities {
  array[t - burn_in] int sim_secondary;
  vector[return_likelihood > 1 ? t - burn_in : 0] log_lik;
  {
    real dispersion = get_param(
      param_id_dispersion, params_fixed_lookup, params_variable_lookup, params_value,
      params
    );
    // simulate secondary reports
    sim_secondary = report_rng(
      secondary[(burn_in + 1):t], dispersion, model_type
    );
    // log likelihood of model
    if (return_likelihood) {
      log_lik = report_log_lik(
        obs[(burn_in + 1):t], secondary[(burn_in + 1):t],
        dispersion, model_type, obs_weight
      );
    }
  }
}
