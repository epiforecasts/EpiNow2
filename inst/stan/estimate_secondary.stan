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
  int delay_max_fixed =
    sum(delay_max[fixed_delays]) - num_elements(fixed_delays) + 1;
  int delay_max_total = (delays == 0 ? 0 :
    sum(delay_max) - num_elements(delay_max) + 1);
  vector[truncation && trunc_fixed[1] ? trunc_max[1] : 0] trunc_fixed_pmf;
  vector[delay_max_fixed] fixed_delays_pmf;

  if (truncation && trunc_fixed[1]) {
    trunc_fixed_pmf = discretised_pmf(
      trunc_mean_mean[1], trunc_sd_mean[1], trunc_max[1], trunc_dist[1], 0
    );
  }
  if (n_fixed_delays) {
    fixed_delays_pmf = combine_pmfs(
      to_vector([ 1 ]),
      delay_mean_mean[fixed_delays],
      delay_sd_mean[fixed_delays],
      delay_max[fixed_delays],
      delay_dist[fixed_delays],
      delay_max_fixed,
      0
    );
  }
}

parameters{
  // observation model
  real delay_mean[n_uncertain_mean_delays];               // mean of delays
  real<lower = 0> delay_sd[n_uncertain_sd_delays];      // sd of delays
  simplex[week_effect] day_of_week_simplex;  // day of week reporting effect
  real<lower = 0> frac_obs[obs_scale];   // fraction of cases that are ultimately observed
  real trunc_mean[truncation];      // mean of truncation
  real trunc_sd[truncation];        // sd of truncation
  real<lower = 0> rep_phi[model_type];   // overdispersion of the reporting process
}

transformed parameters {
  vector<lower=0>[t] secondary;
  // calculate secondary reports from primary
  {
    vector[delay_max_total] delay_pmf;
    delay_pmf = combine_pmfs(
      fixed_delays_pmf, delay_mean, delay_sd, delay_max, delay_dist, delay_max_total, 0
    );
    secondary = calculate_secondary(primary, obs, frac_obs, delay_pmf, cumulative,
                                    historic, primary_hist_additive,
                                    current, primary_current_additive, t);
  }
 // weekly reporting effect
 if (week_effect > 1) {
   secondary = day_of_week_effect(secondary, day_of_week, day_of_week_simplex);
 }
 // truncate near time cases to observed reports
 if (truncation) {
   vector[trunc_max[1]] trunc_cmf;
   trunc_cmf = cumulative_sum(combine_pmfs(
     trunc_fixed_pmf, trunc_mean, trunc_sd, trunc_max, trunc_dist, trunc_max[1], 0
   ));
   secondary = truncate(secondary, trunc_cmf, 0);
 }
}

model {
  // penalised priors for delay distributions
  delays_lp(delay_mean,
            delay_mean_mean[uncertain_mean_delays],
            delay_mean_sd[uncertain_mean_delays],
            delay_sd,
            delay_sd_mean[uncertain_sd_delays],
            delay_sd_sd[uncertain_sd_delays], t);
  // priors for truncation
  truncation_lp(trunc_mean, trunc_sd, trunc_mean_mean, trunc_mean_sd,
                trunc_sd_mean, trunc_sd_sd);
  // prior primary report scaling
  if (obs_scale) {
    frac_obs[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0,];
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
