functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/observation_model.stan
#include functions/secondary.stan
}

data {
  // dimensions
  int n; // number of samples
  int t; // time
  int h; // forecast horizon
  int all_dates; // should all dates have simulations returned
  // secondary model specific data
  array[t - h] int<lower = 0> obs;         // observed secondary data
  matrix[n, t] primary_samples;              // observed primary data
#include data/secondary.stan
  // delay from infection to report
#include data/simulation_delays.stan
  // observation model
#include data/simulation_observation_model.stan
}

transformed data {
  int predict = t - h;
#include chunks/delay_type_max.stan
}

generated quantities {
  array[n, all_dates ? t : h] int sim_secondary;
  for (i in 1:n) {
    vector[t] reports;
    vector[t] primary = to_vector(primary_samples[i]);
#include chunks/sim_vars.stan

#include chunks/delay_rev_pmf.stan
    // calculate secondary reports from primary
#include chunks/calculate_secondary.stan
    // weekly reporting effect
#include chunks/day_of_week_effect.stan

    // simulate secondary reports
    sim_secondary[i] = report_rng(
      tail(reports, all_dates ? t : h), rep_phi, model_type
    );
  }
}
