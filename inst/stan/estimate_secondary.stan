functions {
#include functions/convolve.stan
#include functions/pmfs.stan
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

transformed data {
  int delay_max_fixed =
    num_elements(delay_np_pmf) - num_elements(delay_np_pmf_groups) + 1;
  int delay_max_total = delay_max_fixed + sum(delay_max) - num_elements(delay_max);
  int trunc_max_fixed =
    num_elements(trunc_np_pmf) - num_elements(trunc_np_pmf_groups) + 1;
  int trunc_max_total = trunc_max_fixed + sum(trunc_max) - num_elements(trunc_max);

  vector[trunc_max_fixed] trunc_fixed_pmf;
  vector[delay_max_fixed] delay_fixed_pmf;

  trunc_fixed_pmf = convolve_ragged_pmf(
    trunc_np_pmf, trunc_np_pmf_groups, trunc_max_fixed
  );

  delay_fixed_pmf = convolve_ragged_pmf(
    delay_np_pmf, delay_np_pmf_groups, delay_max_fixed
  );
}

parameters{
  // observation model
  real delay_mean[delay_n_p];
  real<lower = 0> delay_sd[delay_n_p];      // sd of delays
  simplex[week_effect] day_of_week_simplex;  // day of week reporting effect
  real<lower = 0, upper = 1> frac_obs[obs_scale];   // fraction of cases that are ultimately observed
  real trunc_mean[trunc_n_p];      // mean of truncation
  real trunc_sd[trunc_n_p];        // sd of truncation
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  vector<lower=0>[t] secondary;
  // calculate secondary reports from primary

  {
    vector[delay_max_total] delay_rev_pmf;
    delay_rev_pmf = combine_pmfs(
      delay_fixed_pmf, delay_mean, delay_sd, delay_max, delay_dist, delay_max_total, 0, 1
    );
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
 {
   vector[trunc_max_total] trunc_rev_cmf;
   trunc_rev_cmf = reverse_mf(cumulative_sum(combine_pmfs(
     trunc_fixed_pmf, trunc_mean, trunc_sd, trunc_max, trunc_dist, trunc_max_total, 0, 0
   )));
   secondary = truncate(secondary, trunc_rev_cmf, 0);
 }
}

model {
  // penalised priors for delay distributions
  delays_lp(
    delay_mean, delay_mean_mean, delay_mean_sd, delay_sd, delay_sd_mean,
    delay_sd_sd, delay_dist, delay_weight
  );
  
  // priors for truncation
  delays_lp(trunc_mean, trunc_sd, trunc_mean_mean, trunc_mean_sd,
            trunc_sd_mean, trunc_sd_sd, trunc_dist, 1);
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
