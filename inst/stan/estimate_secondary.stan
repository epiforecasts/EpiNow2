functions {
#include functions/pmfs.stan
#include functions/convolve.stan
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
    matrix[t - 1, M] fobs_PHI = setup_gp(fobs_M, fobs_L, t - 1);
    matrix[t - 1, delaym_M] delaym_PHI = setup_gp(delaym_M, delaym_L, t - 1);
    matrix[t - 1, delaysd_M]  delaysd_PHI = setup_gp( delaysd_M,  delaysd_L, t - 1);
}

parameters{
  // observation model
  real delay_mean[delays];               // mean of delays
  real<lower = 0> delay_sd[delays];      // sd of delays
  simplex[week_effect] day_of_week_simplex;  // day of week reporting effect
  real<lower = 0> frac_obs_init[obs_scale > 0 ? 1 : 0];
  // gaussian process
  real<lower = fobs_ls_min,upper = fobs_ls_max> fobs_rho[obs_scale == 2 ? 1 : 0];  
  real<lower = 0> fobs_alpha[obs_scale == 2 ? 1 : 0];   
  vector[obs_scale == 2 ? fobs_M : 0] fobs_eta; 
  real<lower = delay_ls_min,upper = delay_ls_min> delaym_rho[delay_t];  
  real<lower = delay_ls_min,upper = delay_ls_min> delaysd_rho[delay_t]; 
  real<lower = 0> delaym_alpha[delay_t];  
  real<lower = 0> delaysd_alpha[delay_t]; 
  vector[delay_t? delay_M : 0] delaym_eta; 
  vector[delay_t? delay_M : 0] delaysd_eta; 
  real truncation_mean[truncation];      // mean of truncation
  real truncation_sd[truncation];        // sd of truncation
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  vector<lower=0>[t] secondary;
  vector<lower = 0>[t] frac_obs; 
  if (obs_scale > 0) {
    frac_obs = rep_vector(frac_obs_init, t);
    if (obs_scale == 2) {
      vector[t-1] gp = update_gp(fobs_PHI, fobs_M, fobs_L, fobs_alpha[1],
                                 fobs_rho[1], fobs_eta, fobs_gp_type);
      gp = cumulative_sum(gp);
      frac_obs[2:t] = frac_obs[2:t] .* gp;
    }
  }
  // calculate secondary reports from primary
  secondary = calculate_secondary(primary, obs, frac_obs, delay_mean,
                                  delay_sd, max_delay, cumulative,
                                  historic, primary_hist_additive,
                                  current, primary_current_additive, t);
 // weekly reporting effect
 if (week_effect > 1) {
   secondary = day_of_week_effect(secondary, day_of_week, day_of_week_simplex);
  }
 // truncate near time cases to observed reports
 secondary = truncate(secondary, truncation_mean, truncation_sd,
                      max_truncation, 0);
}

model {
  // penalised priors for delay distributions
  delays_lp(delay_mean, delay_mean_mean, delay_mean_sd, delay_sd, delay_sd_mean, delay_sd_sd, 1);
  // priors for truncation
  truncation_lp(truncation_mean, truncation_sd, trunc_mean_mean, trunc_mean_sd,
                trunc_sd_mean, trunc_sd_sd);
  // prior primary report scaling
  if (obs_scale > 0) {
    frac_obs_init[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0,];
    if (obs_scale > 1) {
      gaussian_process_lp(fobs_rho[1], fobs_alpha[1], fobs_eta, fobs_ls_meanlog,
                          fobs_ls_sdlog, fobs_ls_min, fobs_ls_max, fobs_alpha_sd);
    }
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
