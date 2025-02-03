functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/delays.stan
#include functions/gaussian_process.stan
#include functions/rt.stan
#include functions/infections.stan
#include functions/observation_model.stan
#include functions/generated_quantities.stan
#include functions/params.stan
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
  // parameters
#include data/params.stan
#include data/estimate_infections_params.stan
  matrix[n, n_params_variable] params; // parameters
}

transformed data {
  array[delay_types] int delay_type_max = get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  );
}

generated quantities {
  // generated quantities
  matrix[n, t] infections; //latent infections
  matrix[n, t - seeding_time] reports; // observed cases
  array[n, t - seeding_time] int imputed_reports;
  matrix[n, t - seeding_time - 1] r;
  {
    vector[n] dispersion = get_param(
      dispersion_id, params_fixed_lookup, params_variable_lookup,
      params_value, params
    );
    vector[n] frac_obs = get_param(
      frac_obs_id, params_fixed_lookup, params_variable_lookup,
      params_value, params
    );
    for (i in 1:n) {
      // generate infections from Rt trace
      vector[delay_type_max[gt_id] + 1] gt_rev_pmf;
        gt_rev_pmf = get_delay_rev_pmf(
        gt_id, delay_type_max[gt_id] + 1, delay_types_p, delay_types_id,
        delay_types_groups, delay_max, delay_np_pmf,
        delay_np_pmf_groups, delay_params[i], delay_params_groups, delay_dist,
        1, 1, 0
      );

      infections[i] = to_row_vector(generate_infections(
        to_vector(R[i]), seeding_time, gt_rev_pmf, initial_infections[i],
        pop, future_time, obs_scale, frac_obs[i], initial_as_scale
      ));

      if (delay_id) {
        vector[delay_type_max[delay_id] + 1] delay_rev_pmf = get_delay_rev_pmf(
          delay_id, delay_type_max[delay_id] + 1, delay_types_p, delay_types_id,
          delay_types_groups, delay_max, delay_np_pmf,
          delay_np_pmf_groups, delay_params[i], delay_params_groups, delay_dist,
          0, 1, 0
        );
        // convolve from latent infections to mean of observations
        reports[i] = to_row_vector(convolve_to_report(
          to_vector(infections[i]), delay_rev_pmf, seeding_time)
        );
      } else {
        reports[i] = to_row_vector(
          infections[i, (seeding_time + 1):t]
        );
      }

      // weekly reporting effect
      if (week_effect > 1) {
        reports[i] = to_row_vector(
          day_of_week_effect(to_vector(reports[i]), day_of_week,
                            to_vector(day_of_week_simplex[i])));
      }
      // truncate near time cases to observed reports
      if (trunc_id) {
        vector[delay_type_max[trunc_id] + 1] trunc_rev_cmf = get_delay_rev_pmf(
          trunc_id, delay_type_max[trunc_id] + 1, delay_types_p, delay_types_id,
          delay_types_groups, delay_max, delay_np_pmf,
          delay_np_pmf_groups, delay_params[i], delay_params_groups, delay_dist,
          0, 1, 1
        );
        reports[i] = to_row_vector(truncate_obs(
          to_vector(reports[i]), trunc_rev_cmf, 0)
        );
      }
      // scale observations
      if (obs_scale) {
        reports[i] = to_row_vector(scale_obs(to_vector(reports[i]), frac_obs[i]));
      }
      // simulate reported cases
      imputed_reports[i] = report_rng(
        to_vector(reports[i]), dispersion[i], model_type
      );
      r[i] = to_row_vector(
        calculate_growth(to_vector(infections[i]), seeding_time + 1)
      );
    }
  }
}
