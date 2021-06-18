functions {
#include functions/utils.stan
#include functions/pmfs.stan
#include functions/convolve.stan
#include functions/observation_model.stan
#include functions/secondary.stan
#include functions/gaussian_process.stan
}

data {
  int t;                             // time of observations
  int t_dim[1];                      // time  as a integer vector
  int<lower = 0> obs[t];             // observed secondary data
  vector[t] primary;                 // observed primary data
  int burn_in;                       // time period to not use for fitting
#include data/secondary.stan
#include data/delays.stan
#include data/observation_model.stan
#include data/gaussian_process.stan
}

transformed data {
  vector[gp_mat_dim] PHI;
  // Set up gassian process kernals
  if (gps) {  
    PHI = setup_gps(gps, M, L, gp_dims, gp_order, gp_mat_dim);
  }
}

parameters{
  // observation model
  vector[delays] dmean_init;               // mean of delays
  vector<lower = 0>[delays] dsd_init;      // sd of delays
  simplex[week_effect] day_of_week_simplex;  // day of week reporting effect
  vector<lower = 0>[obs_scale > 0 ? 1 : 0] frac_obs_init;
  // gaussian process
  real<lower = 0,upper = 1> rho_raw[gps];  
  real<lower = 0> alpha[gps];   
  vector[gps ? sum(M) : 0] eta; 
  real truncation_mean[truncation];      // mean of truncation
  real truncation_sd[truncation];        // sd of truncation
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  // Define stored parameters
  vector<lower=0>[t] secondary;
  vector<lower = 0>[t] frac_obs; 
  vector[t*delays] delay_mean;
  vector<lower = 0> [t*delays] delay_sd;
  {
    // Define temporary parameters
    vector[t*total_delay] pmfs;
    vector[gp_dim] gp;
    // Update Gaussian processes
    if (gps) {
      gp = update_gps(PHI, gps, gp_dims, gp_dim, M, L, alpha, rho_raw, eta,
                      ls_min, ls_max, gp_order, gp_type);
    }
    // Cast observation scaling to all time points and scale with GP
    if (obs_scale) {
      int mod_index = sum(head(gp_dims, sum(obs_scale_gp)));
      vector[mod_index] mod = head(gp, mod_index);
      frac_obs = vector_param(frac_obs_init, mod, 1, 1, t_dim, obs_scale_gp, t);
    }
    // Cast delays to all time points and scale with GP
    if (delays) {
      int delay_static = sum(delays_gp);
      int mod_index = sum(tail(gp_dims, delay_static));
      vector[mod_index] mod = tail(gp, mod_index);
      delay_mean = vector_param(dmean_init, mod, delays, 1, t_dim, delays_gp, t);
      delay_sd = vector_param(dsd_init, mod, delays, 1, t_dim, delays_gp, t);
      // Calculate PMFs as needed for delay distribtions
      // Steps: Calculate unique PMFs, convolve, cast to cover all time points
      if (delay_static) {
        pmfs = 
          vector_pmf(dmean_init, dsd_init, max_delay, delays, 1, t_dim, t, 1);
      }else{
        int broadcast[t] = rep_int(1, t);
        pmfs = 
          vector_pmf(delay_mean, delay_sd, max_delay, delays, t, broadcast, t, 1);
      }
    }  
      
    // calculate secondary reports from primary
    secondary = calculate_secondary(primary, obs, frac_obs, pmfs,
                                    total_delay, cumulative, historic, primary_hist_additive, current,
                                    primary_current_additive, t);
  // weekly reporting effect
  if (week_effect > 1) {
    secondary = day_of_week_effect(secondary, day_of_week, day_of_week_simplex);
    }
  // truncate near time cases to observed reports
  secondary = truncate(secondary, truncation_mean, truncation_sd,
                        max_truncation, 0);
  }
}

model {
  // penalised priors for delay distributions
  delays_lp(dmean_init, delay_mean_mean, delay_mean_sd, dsd_init,
            delay_sd_mean, delay_sd_sd, 1);

  // priors for truncation
  truncation_lp(truncation_mean, truncation_sd, trunc_mean_mean, trunc_mean_sd,
                trunc_sd_mean, trunc_sd_sd);
  // prior primary report scaling
  if (obs_scale > 0) {
    frac_obs_init[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0,];
  }

  if (gps){
    gaussian_process_lp(rho_raw, alpha, eta, ls_meanlog, ls_sdlog, alpha_sd);
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
