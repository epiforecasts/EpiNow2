functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/observation_model.stan
#include functions/secondary.stan
}

data {
  int t;                             // time of observations
  array[t] int<lower = 0> obs;             // observed secondary data
  vector[t] primary;                 // observed primary data
  int burn_in;                       // time period to not use for fitting
#include data/secondary.stan
#include data/delays.stan
#include data/observation_model.stan
}

transformed data{
  int predict = t;
#include chunks/delay_type_max.stan
}

parameters{
  // observation model
#include params/delays.stan
#include params/observation_model.stan
}

transformed parameters {
  vector<lower=0>[t] reports; // secondary reports

  {
#include chunks/delay_rev_pmf.stan
#include chunks/calculate_secondary.stan
  // weekly reporting effect
#include chunks/day_of_week_effect.stan
  // truncate near time cases to observed reports
#include chunks/trunc_rev_cmf.stan
    reports = truncate(reports, trunc_rev_cmf, 0);
  }
}

model {
  // penalised priors for delay distributions
#include chunks/delays_lp.stan
  // prior primary report scaling
#include chunks/obs_scale_lp.stan
  // observed secondary reports from mean of secondary reports (update likelihood)
  if (likelihood) {
    array[t - burn_in] int cases = obs[(burn_in + 1):t];
    vector[t - burn_in] obs_reports = reports[(burn_in + 1):t];
#include chunks/report_lp.stan
  }
}

generated quantities {
  array[t - burn_in] int sim_secondary;
  vector[return_likelihood > 1 ? t - burn_in : 0] log_lik;
  // simulate secondary reports
  sim_secondary = report_rng(reports[(burn_in + 1):t], rep_phi, model_type);
  // log likelihood of model
  if (return_likelihood) {
    array[t - burn_in] int cases = obs[(burn_in + 1):t];
    vector[t - burn_in] obs_reports = reports[(burn_in + 1):t];
#include chunks/report_log_lik.stan
  }
}
