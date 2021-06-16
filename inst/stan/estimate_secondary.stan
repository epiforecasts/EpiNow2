functions {
#include functions/pmfs.stan
#include functions/convolve.stan
#include functions/observation_model.stan
#include functions/secondary.stan
#include functions/gaussian_process.stan
}

data {
  int t;                             // time of observations
  int<lower = 0> obs[t];             // observed secondary data
  vector[t] primary;                 // observed primary data
  int burn_in;                       // time period to not use for fitting
#include data/secondary.stan
#include data/delays.stan
#include data/observation_model.stan
#include data/gaussian_process.stan
}

transformed data {
  vector[gp_global_dim]  PHI;
  if (gps) {  
    PHI = setup_gps(gps, M, L, gp_dims, gp_global_dim);
  }
}

parameters{
  // observation model
  real delay_mean_init[delays];               // mean of delays
  real<lower = 0> delay_sd_init[delays];      // sd of delays
  simplex[week_effect] day_of_week_simplex;  // day of week reporting effect
  real<lower = 0> frac_obs_init[obs_scale > 0 ? 1 : 0];
  // gaussian process
  real<lower = 0,upper = 1> rho_raw[gps];  
  real<lower = 0> alpha[gps];   
  vector[gps ? sum(M) : 0] eta; 
  real truncation_mean[truncation];      // mean of truncation
  real truncation_sd[truncation];        // sd of truncation
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  vector<lower=0>[t] secondary;
  vector[sum(gp_dims)] gp;
  vector<lower = 0>[t] frac_obs; 
  vector[t*delays] delay_mean;
  vector<lower = 0> [t*delays] delay_sd;
  vector[sum(max_delay)] pmfs;
  if (gps) {
    gp = update_gps(PHI, gps, gp_dims, M, L, alpha, rho_raw, eta, ls_min,
                    ls_max, order, gp_type);
  }
  if (obs_scale) {
    frac_obs = rep_vector(frac_obs_init[1], t);
    if (obs_scale_gp) {
      frac_obs[2:t] = frac_obs[2:t] .* exp(head(gp, gp_dims[1]));
    }
  }
  if (delays) {
    int pos = 1;
    int gp_int = obs_scale_gp;
    int gp_pos = gp_int > 0 ? sum(gp_dims[1:gp_int]) + 1 : 1;
    for (i in 1:delays) {
      vector[t] dmeant = rep_vector(delay_mean_init[i], t);
      vector[t] dsdt = rep_vector(delay_sd_init[i], t);
      if (delays_gp[i]) {
        gp_int += 1;
        dmeant = dmeant .* exp(segment(gp, gp_pos, gp_dims[gp_int]));
        gp_pos += gp_dims[gp_int]; 
        
        gp_int += 1;
        dsdt = dsdt .* exp(segment(gp, gp_pos, gp_dims[gp_int]));
        gp_pos += gp_dims[gp_int]; 
      }
      delay_mean[pos:(pos + t - 1)] = dmeant;
      delay_sd[pos:(pos + t - 1)] = dsdt;
      pos += t;
    }
    pmfs = calculate_pmfs(delay_mean, delay_sd, max_delay);
  }
    
  // calculate secondary reports from primary
  secondary = calculate_secondary(primary, obs, frac_obs, pmfs, delays,
                                  max_delay, cumulative, historic, primary_hist_additive, current,
                                  primary_current_additive, t);
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
  delays_lp(delay_mean_init, delay_mean_mean, delay_mean_sd, delay_sd_init,
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
