functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/gaussian_process.stan
#include functions/infections.stan
#include functions/observation_model.stan
#include functions/generated_quantities.stan
}

data {
  // dimensions
  int n; // number of samples
  int t; // unobserved time
  int seeding_time; // time period used for seeding and not observed
  int future_time; // fixed future time
  // Rt
#include data/simulation_rt.stan
  // delay from infection to report
#include data/simulation_delays.stan
  // observation model
#include data/simulation_observation_model.stan
}

transformed data {
  int delay_type_max[delay_types] = get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  );
}

generated quantities {
  // generated quantities
  vector[n] infections[t]; //latent infections
  vector[n] reports[t - seeding_time]; // observed cases
  int imputed_reports[n, t - seeding_time];
  vector[n] r[t - seeding_time];
  vector[seeding_time] uobs_inf;
  for (i in 1:n) {
    // generate infections from Rt trace
    vector[delay_type_max[gt_id]] gt_rev_pmf;
    gt_rev_pmf = get_delay_rev_pmf(
      gt_id, delay_type_max[gt_id], delay_types_p, delay_types_id,
      delay_types_groups, delay_max, delay_np_pmf,
      delay_np_pmf_groups, delay_mean[i], delay_sd[i], delay_dist,
      1, 1, 0
    );

    uobs_inf = generate_seed(initial_infections[i], initial_growth[i], seeding_time);
     // generate infections from Rt trace
    infections[i] = renewal_model(R[i], uobs_inf, gt_rev_pmf, pop, future_time);
    // convolve from latent infections to mean of observations
    if (delay_id) {
      vector[delay_type_max[delay_id]] delay_rev_pmf = get_delay_rev_pmf(
        delay_id, delay_type_max[delay_id], delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_mean[i], delay_sd[i], delay_dist,
        0, 1, 0
      );
      reports[i] = convolve_to_report(infections[i], delay_rev_pmf, seeding_time);
    } else {
      reports[i] = infections[(seeding_time + 1):t];
    }
    // weekly reporting effect
    if (week_effect > 1) {
      reports[i] = day_of_week_effect(
        reports[i], day_of_week, to_vector(day_of_week_simplex[i])
      );
    }
    // scale observations
    if (obs_scale) {
      reports[i] = scale_obs(reports[i], frac_obs[i, 1]);
    }
   // simulate reported cases
   imputed_reports[i] = report_rng(reports[i], rep_phi[i], obs_dist);
   r[i] = calculate_growth(infections[i], seeding_time);
  }
}
