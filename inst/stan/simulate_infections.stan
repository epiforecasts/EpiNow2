functions {
#include functions/pmfs.stan
#include functions/convolve.stan
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
  // Rt
  real initial_infections[seeding_time ? n : 0, 1]; // initial logged infections
  real initial_growth[seeding_time > 1 ? n : 0, 1]; //initial growth
  real<lower = 0> gt_mean[n, 1];  // mean of generation time
  real<lower = 0> gt_sd[n, 1];   // sd of generation time
  int max_gt;                    // maximum generation time
  matrix[n, t - seeding_time] R; // reproduction number
  // delay from infection to report
  int delays;                           // no. of delay distributions
  real<lower = 0> delay_mean[n, delays];// mean of delays
  real<lower = 0> delay_sd[n, delays];  // sd of delays
  int max_delay[delays];                // maximum delay
  // observation model
  int day_of_week[t - seeding_time]; // day of the week indicator (1 - 7)
  int week_effect;                   // should a day of the week effect be estimated
  real<lower = 0> day_of_week_simplex[n, 7];
  int model_type;
  real<lower = 0> rep_phi[n, model_type];  // overdispersion of the reporting process
}

generated quantities {
  // generated quantities
  matrix[n, t] infections; //latent infections
  matrix[n, t - seeding_time] reports; // observed cases
  int imputed_reports[n, t - seeding_time];
  real r[n, t - seeding_time];
  for (i in 1:n) {
    // generate infections from Rt trace
    infections[i] = to_row_vector(generate_infections(to_vector(R[i]), seeding_time, 
                                                      gt_mean[i], gt_sd[i], max_gt, 
                                                      initial_infections[i], initial_growth[i]));
    // convolve from latent infections to mean of observations
    reports[i] = to_row_vector(convolve_to_report(to_vector(infections[i]), delay_mean[i], 
                                                  delay_sd[i], max_delay, seeding_time));
    // weekly reporting effect 
    if (week_effect) {
      reports[i] = to_row_vector(day_of_week_effect(to_vector(reports[i]), day_of_week, 
                                                    to_vector(day_of_week_simplex[i])));
    }
    // scale observations
    if (scale_obs) {
      reports[i] = to_row_vector(scale_obs(to_vector(reports[i]), day_of_week, 
                                           to_vector(day_of_week_simplex[i])));
    }
   // simulate reported cases
   imputed_reports[i] = report_rng(to_vector(reports[i]), rep_phi[i], model_type);
   r[i] = R_to_growth(to_vector(R[i]), gt_mean[i, 1], gt_sd[i, 1]);
  }
}
