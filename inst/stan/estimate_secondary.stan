functions {
#include functions/pmfs.stan
#include functions/convolve.stan
#include functions/observation_model.stan
#include functions/generated_quantities.stan
}


data {
#include data/observations.stan
#include data/delays.stan
#include data/observation_model.stan
}

parameters{
  // observation model
  real delay_mean[delays];               // mean of delays
  real delay_sd[delays];                 // sd of delays
  simplex[week_effect ? 7 : 1] day_of_week_simplex;   // day of week reporting effect 
  real<lower = 0> frac_obs[obs_scale];   // fraction of cases that are ultimately observed
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  vector[t] scaled_reports;
  vector[t] conv_reports;                            
  vector[t] secondary_reports = rep_vector(0.0, t);
  // scaling of reported cases by fraction 
  scaled_reports = scale_obs(reports, frac_obs[1]);
  // convolve from reports to contributions from these reports
  conv_reports = convolve_to_report(scaled_reports, delay_mean, delay_sd, max_delay, seeding_time);
  // combine reports with previous secondary data
  for (i in 1:t) {
    if (cum_reports & i > 1) {
      secondary_reports[i] = obs[i - 1];
    }
    if (conv_additive) {
      secondary_reports[i] += conv_reports[i];
    }else{
      secondary_reports[i] -= conv_reports[i];
    }
    if (current_reports) {
      if (current_additive) {
        secondary_reports[i] += scaled_reports[i];
      }else{
        secondary_reports[i] -= scaled_reports[i];
      }
    }
  }
 // weekly reporting effect
 if (week_effect) {
   secondary_reports = day_of_week_effect(secondary_reports, day_of_week, day_of_week_simplex);
  }
}

model {
  // penalised priors for delay distributions
  delays_lp(delay_mean, delay_mean_mean, delay_mean_sd, delay_sd, delay_sd_mean, delay_sd_sd, 1);
  // prior observation scaling
  frac_obs[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0,];
  // observed reports from mean of reports (update likelihood)
  report_lp(obs, secondary_reports, rep_phi, 1, model_type, 1);
}
  
generated quantities {
  int sim_reports[t]; 
  vector[t] log_lik;
  // simulate reported cases
  sim_reports = report_rng(secondary_reports, rep_phi, model_type);
  // log likelihood of model
  log_lik = report_log_lik(cases, obs_reports, rep_phi, model_type, obs_weight);
}
