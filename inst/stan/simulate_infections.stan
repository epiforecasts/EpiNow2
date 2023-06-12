functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/gaussian_process.stan
#include functions/rt.stan
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
  matrix[n, t] infections; //latent infections
  matrix[n, t - seeding_time] reports; // observed cases
  int imputed_reports[n, t - seeding_time];
  real r[n, t - seeding_time];
  for (i in 1:n) {
    // generate infections from Rt trace
    vector[delay_type_max[gt_id]] gt_rev_pmf;
    gt_rev_pmf = get_delay_rev_pmf(
      gt_id, delay_type_max[gt_id], delay_types_p, delay_types_id,
      delay_types_groups, delay_max, delay_np_pmf,
      delay_np_pmf_groups, delay_mean[i], delay_sd[i], delay_dist,
      1, 1, 0
    );

    infections[i] = to_row_vector(generate_infections(
      to_vector(R[i]), seeding_time, gt_rev_pmf, initial_infections[i],
      initial_growth[i], pop, future_time
    ));

    if (delay_id) {
      vector[delay_type_max[delay_id]] delay_rev_pmf = get_delay_rev_pmf(
        delay_id, delay_type_max[delay_id], delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_mean[i], delay_sd[i], delay_dist,
        0, 1, 0
      );
     // convolve from latent infections to mean of observations
      reports[i] = to_row_vector(convolve_to_report(
        to_vector(infections[i]), delay_rev_pmf, seeding_time)
      );
    } else {
      reports[i] = to_row_vector(infections[(seeding_time + 1):t]);
    }

    // weekly reporting effect
    if (week_effect > 1) {
      reports[i] = to_row_vector(
        day_of_week_effect(to_vector(reports[i]), day_of_week,
                           to_vector(day_of_week_simplex[i])));
    }
    // scale observations
    if (obs_scale) {
      reports[i] = to_row_vector(scale_obs(to_vector(reports[i]), frac_obs[i, 1]));
    }
    // simulate reported cases
    imputed_reports[i] = report_rng(
      to_vector(reports[i]), rep_phi[i], model_type
    );
   r[i] = R_to_growth(to_vector(R[i]), delay_mean[i, gt_id], delay_sd[i, gt_id]);
  }
}
