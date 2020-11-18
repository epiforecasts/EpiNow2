functions {
#include functions/pmfs.stan
#include functions/convolve.stan
#include functions/observation_model.stan
#include functions/secondary.stan
}

data {  
#include data/secondary.stan
#include data/delays.stan
#include data/observation_model.stan
}

parameters{
  // observation model
  real delay_mean[delays];               // mean of delays
  real delay_sd[delays];                 // sd of delays
  simplex[week_effect ? 7 : 1] day_of_week_simplex;   // day of week reporting effect 
  real<lower = 0> frac_obs[obs_scale];   // fraction of cases that are ultimately observed
  real truncation_mean[truncation];      // mean of truncation
  real truncation_sd[truncation];        // sd of truncation
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  vector[t] secondary_reports;
  // calculate secondary reports from primary
  secondary_reports = calculate_secondary(reports, obs, frac_obs[1], delay_mean, 
                                          delay_sd, max_delay, cumulative, 
                                          historic, primary_hist_additive, 
                                          current, primary_current_additive, t);
 // weekly reporting effect
 if (week_effect) {
   secondary_reports = day_of_week_effect(secondary_reports, day_of_week, day_of_week_simplex);
  }
 // truncate near time cases to observed reports 
 secondary_reports = truncate(secondary_reports, truncation_mean, truncation_sd, max_truncation, 0);
}

model {
  // penalised priors for delay distributions
  delays_lp(delay_mean, delay_mean_mean, delay_mean_sd, delay_sd, delay_sd_mean, delay_sd_sd, 1);
  // priors for truncation
  truncation_lp(truncation_mean, truncation_sd, trunc_mean_mean, trunc_mean_sd, 
                trunc_sd_mean, trunc_sd_sd);
  // prior primary report scaling
  frac_obs[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0,];
  // observed secondary reports from mean of secondary reports (update likelihood)
  report_lp(obs, secondary_reports, rep_phi, 1, model_type, 1);
}
  
generated quantities {
  int sim_reports[t]; 
  vector[t] log_lik;
  // simulate secondary reports
  sim_reports = report_rng(secondary_reports, rep_phi, model_type);
  // log likelihood of model
  log_lik = report_log_lik(obs, secondary_reports, rep_phi, model_type, obs_weight);
}
