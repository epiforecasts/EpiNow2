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

generated quantities {
  vector[t] secondary;
  int sim_secondary[t]; 
  vector[t] log_lik;
  // calculate secondary reports from primary
  secondary = calculate_secondary(primary, obs, frac_obs, delay_mean, 
                                  delay_sd, max_delay, cumulative, 
                                  historic, primary_hist_additive, 
                                  current, primary_current_additive, t);
 // weekly reporting effect
 if (week_effect) {
   secondary = day_of_week_effect(secondary, day_of_week, day_of_week_simplex);
  }
 // truncate near time cases to observed reports 
 secondary = truncate(secondary, truncation_mean, truncation_sd, max_truncation, 0);
  // simulate secondary reports
  sim_secondary = report_rng(secondary, rep_phi, model_type);
  // log likelihood of model
  log_lik = report_log_lik(obs, secondary, rep_phi, model_type, obs_weight);
}
