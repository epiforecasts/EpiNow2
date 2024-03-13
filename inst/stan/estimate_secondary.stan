functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/observation_model.stan
#include functions/secondary.stan
}

data {
  int t;                             // time of observations
  int lt;                             // time of observations
  array[t] int<lower = 0> obs;             // observed secondary data
  array[lt] int obs_time;             // observed secondary data
  vector[t] primary;                 // observed primary data
  int burn_in;                       // time period to not use for fitting
#include data/secondary.stan
#include data/delays.stan
#include data/observation_model.stan
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
  array[obs_scale] real<lower = 0, upper = 1> frac_obs;   // fraction of cases that are ultimately observed
  array[model_type] real<lower = 0> rep_phi;   // overdispersion of the reporting process
}

transformed parameters {
  vector<lower=0>[t] secondary;
  // calculate secondary reports from primary

  {
    vector[t] scaled;
    vector[t] convolved = rep_vector(1e-5, t);

    // scaling of primary reports by fraction observed
    if (obs_scale) {
      scaled = scale_obs(primary, obs_scale_sd > 0 ? frac_obs[1] : obs_scale_mean);
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
    secondary = truncate(secondary, trunc_rev_cmf, 0);
 }
}

model {
  // penalised priors for delay distributions
  delays_lp(
    delay_params, delay_params_mean, delay_params_sd, delay_params_groups,
    delay_dist, delay_weight
  );

  // prior primary report scaling
  if (obs_scale) {
    frac_obs[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0, 1];
   }
  // observed secondary reports from mean of secondary reports (update likelihood)
  if (likelihood) {
    report_lp(
      obs[(burn_in + 1):t][obs_time], obs_time, secondary[(burn_in + 1):t],
      rep_phi, phi_mean, phi_sd, model_type, 1, accumulate
    );
  }
}

generated quantities {
  array[t - burn_in] int sim_secondary;
  vector[return_likelihood > 1 ? t - burn_in : 0] log_lik;
  // simulate secondary reports
  sim_secondary = report_rng(secondary[(burn_in + 1):t], rep_phi, model_type);
  // log likelihood of model
  if (return_likelihood) {
    log_lik = report_log_lik(obs[(burn_in + 1):t], secondary[(burn_in + 1):t],
                             rep_phi, model_type, obs_weight);
  }
}
