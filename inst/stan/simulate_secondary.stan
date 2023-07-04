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
  matrix[n, t] primary;              // observed primary data
#include data/secondary.stan
  // delay from infection to report
#include data/simulation_delays.stan
  // observation model
#include data/simulation_observation_model.stan
}

transformed data {
  array[delay_types] int delay_type_max = get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  );
}

generated quantities {
  array[n, all_dates ? t : h] int sim_secondary;
  for (i in 1:n) {
    vector[t] secondary;
    vector[t] scaled;
    vector[t] convolved = rep_vector(1e-5, t);

    if (obs_scale) {
      scaled = scale_obs(to_vector(primary[i]), frac_obs[i, 1]);
    } else {
      scaled = to_vector(primary[i]);
    }

    if (delay_id) {
      vector[delay_type_max[delay_id] + 1] delay_rev_pmf = get_delay_rev_pmf(
        delay_id, delay_type_max[delay_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params[i], delay_params_groups, delay_dist,
        0, 1, 0
      );
      convolved = convolved + convolve_to_report(scaled, delay_rev_pmf, 0);
    } else {
      convolved = convolved + scaled;
    }

    // calculate secondary reports from primary
    secondary = calculate_secondary(
      scaled, convolved, obs, cumulative, historic, primary_hist_additive,
      current, primary_current_additive, t - h + 1
    );

    // weekly reporting effect
    if (week_effect > 1) {
      secondary = day_of_week_effect(
        secondary, day_of_week, to_vector(day_of_week_simplex[i])
      );
    }

    // truncate near time cases to observed reports
    if (trunc_id) {
      vector[delay_type_max[trunc_id] + 1] trunc_rev_cmf = get_delay_rev_pmf(
        trunc_id, delay_type_max[trunc_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params[i], delay_params_groups, delay_dist,
        0, 1, 1
      );
      secondary = truncate(
        secondary, trunc_rev_cmf, 0
      );
    }

    // simulate secondary reports
    sim_secondary[i] = report_rng(
      tail(secondary, all_dates ? t : h), rep_phi[i], obs_dist
    );
  }
}
