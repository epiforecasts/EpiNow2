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
#include chunks/delay_type_max.stan
}

generated quantities {
  // generated quantities
  array[n, t] real sim_infections; //latent infections
  array[n, t - seeding_time] real sim_reports; // observed cases
  array[n, t - seeding_time] int sim_imputed_reports;
  array[n, t - seeding_time] real sim_r;
  for (i in 1:n) {
    // generate infections from Rt trace
#include chunks/sim_vars.stan
    vector[t - seeding_time] R = to_vector(R_samples[i]);
    array[seeding_time > 0] real initial_infections;
    array[seeding_time > 1] real initial_growth;
    if (seeding_time > 0) {
      initial_infections = initial_infections_samples[i];
      if (seeding_time > 1 ) {
        initial_growth = initial_growth_samples[i];
      }
    }

    {
      vector[t] infections;
      vector[t - seeding_time] reports;
      array[t - seeding_time] int imputed_reports;
      array[t - seeding_time] real r;
#include chunks/gt_rev_pmf.stan
#include chunks/delay_rev_pmf.stan
#include chunks/generate_infections.stan
#include chunks/convolve_to_report.stan
#include chunks/day_of_week_effect.stan
#include chunks/scale_obs.stan
#include chunks/impute_reports.stan
#include chunks/R_to_growth.stan
      sim_infections[i] = to_array_1d(infections);
      sim_reports[i] = to_array_1d(reports);
      sim_imputed_reports[i] = to_array_1d(imputed_reports);
      sim_r[i] = r;
    }
  }
}
