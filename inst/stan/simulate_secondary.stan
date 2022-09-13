functions {
#include functions/pmfs.stan
#include functions/convolve.stan
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
  int<lower = 0> obs[t - h];         // observed secondary data
  matrix[n, t] primary;              // observed primary data
#include data/secondary.stan
  // delay from infection to report
#include data/simulation_delays.stan
  // observation model
#include data/simulation_observation_model.stan
}

transformed data {
  int max_total_delay = sum(max_delay) - num_elements(max_delay) + 1;
}

generated quantities {
  int sim_secondary[n, all_dates ? t : h];
  for (i in 1:n) {
    vector[t] secondary;
    vector[max_total_delay] delay_pmf;
    delay_pmf = combine_pmfs(
      to_vector([ 1 ]),
      delay_mean[i],
      delay_sd[i],
      max_delay,
      max_total_delay,
      0
    );

    // calculate secondary reports from primary
    secondary =
       calculate_secondary(to_vector(primary[i]), obs, frac_obs[i], delay_pmf, cumulative,
                           historic, primary_hist_additive,
                           current, primary_current_additive, t - h + 1);
    // weekly reporting effect
    if (week_effect > 1) {
      secondary = day_of_week_effect(secondary, day_of_week, to_vector(day_of_week_simplex[i]));
    }
    // simulate secondary reports
    sim_secondary[i] = report_rng(tail(secondary, all_dates ? t : h), rep_phi[i], model_type);
  }
}
