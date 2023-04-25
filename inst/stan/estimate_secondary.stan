functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/observation_model.stan
#include functions/secondary.stan
}

data {
  int t;                             // time of observations
  int<lower = 0> obs[t];             // observed secondary data
  vector[t] primary;                 // observed primary data
  int burn_in;                       // time period to not use for fitting
#include data/secondary.stan
#include data/delays.stan
#include data/observation_model.stan
}

transformed data{
  int delay_type_max[delay_types] = get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  );
}

parameters{
  // observation model
  real delay_mean[delay_n_p];
  real<lower = 0> delay_sd[delay_n_p];      // sd of delays
  simplex[week_effect] day_of_week_simplex;  // day of week reporting effect
  real<lower = 0, upper = 1> frac_obs[obs_scale];   // fraction of cases that are ultimately observed
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  vector<lower=0>[t] secondary;
  // calculate secondary reports from primary

  {
    vector[delay_type_max[delay_id]] delay_rev_pmf;
    if (delay_id) {
      delay_rev_pmf = get_delay_rev_pmf(
        delay_id, delay_type_max[delay_id], delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_mean, delay_sd, delay_dist,
        0, 1, 0
      );
    } else {
      delay_rev_pmf = to_vector({ 1 });
    }
    secondary = calculate_secondary(
      primary, obs, frac_obs, delay_rev_pmf, cumulative, historic,
      primary_hist_additive, current, primary_current_additive, t
    );
  }

 // weekly reporting effect
 if (week_effect > 1) {
   secondary = day_of_week_effect(secondary, day_of_week, day_of_week_simplex);
 }
 // truncate near time cases to observed reports
 if (trunc_id) {
    vector[delay_type_max[trunc_id]] trunc_rev_cmf = get_delay_rev_pmf(
      trunc_id, delay_type_max[trunc_id], delay_types_p, delay_types_id,
      delay_types_groups, delay_max, delay_np_pmf,
      delay_np_pmf_groups, delay_mean, delay_sd, delay_dist,
      0, 1, 1
    );
    secondary = truncate(secondary, trunc_rev_cmf, 0);
 }
}

model {
  // penalised priors for delay distributions
  delays_lp(
    delay_mean, delay_mean_mean, delay_mean_sd, delay_sd, delay_sd_mean,
    delay_sd_sd, delay_dist, delay_weight
  );
  
  // prior primary report scaling
  if (obs_scale) {
    frac_obs[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0, 1];
   }
  // observed secondary reports from mean of secondary reports (update likelihood)
  if (likelihood) {
    report_lp(obs[(burn_in + 1):t], secondary[(burn_in + 1):t],
              rep_phi, phi_mean, phi_sd, model_type, 1);
  }
}

generated quantities {
  int sim_secondary[t - burn_in];
  vector[return_likelihood > 1 ? t - burn_in : 0] log_lik;
  // simulate secondary reports
  sim_secondary = report_rng(secondary[(burn_in + 1):t], rep_phi, model_type);
  // log likelihood of model
  if (return_likelihood) {
    log_lik = report_log_lik(obs[(burn_in + 1):t], secondary[(burn_in + 1):t],
                             rep_phi, model_type, obs_weight);
  }
}
