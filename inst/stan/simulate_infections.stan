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
  int n; // number of samples
  int delays;                  // no. of delay distributions
  real<lower = 0> delay_mean[n, delays];   // mean of delays
  real<lower = 0> delay_sd[n, delays];   // sd of delays
  int t;                                            // unobserved time
  int seeding_time;                                 // time period used for seeding and not observed
  matrix[n, seeding_time] initial_infections; // seed infections
  int max_delay[delays];       // maximum incubation period
  int max_gt;                        // maximum generation time
  real<lower = 0> gt_mean[n, 1];  // mean of generation time
  real<lower = 0> gt_sd[n, 1];   // sd of generation time
  int model_type;
  int day_of_week[t - seeding_time]; // day of the week indicator (1 - 7)
  int week_effect;                   // should a day of the week effect be estimated
  real<lower = 0> day_of_week_simplex[n, 7];
  real<lower = 0> rep_phi[n, model_type];  // overdispersion of the reporting process
  vector<lower = 0>[t] shifted_cases;               // median shifted smoothed cases
  matrix[n, t - seeding_time] R;                    // reproduction number
}

generated quantities {
  // Simulate latent infections
  matrix[n, t] infections;
  matrix[n, t - seeding_time] reports;                                   // observed cases
  int imputed_reports[n, t - seeding_time];
  real r[n, t - seeding_time];

  for (i in 1:n) {
    infections[i] = to_row_vector(generate_infections(to_vector(R[i]), seeding_time, gt_mean[i], gt_sd[i], max_gt, shifted_cases, to_vector(initial_infections[i])));
  }
  // convolve from latent infections to mean of observations
  for (i in 1:n) {
    reports[i] = to_row_vector(convolve_to_report(to_vector(infections[i]), delay_mean[i], delay_sd[i], max_delay, seeding_time));
  }
  // weekly reporting effect
  if (week_effect) {
    for (i in 1:n) {
      reports[i] = to_row_vector(day_of_week_effect(to_vector(reports[i]), day_of_week, to_vector(day_of_week_simplex[i])));
    }
  }
  // simulate reported cases
  for (i in 1:n) {
    imputed_reports[i] = report_rng(to_vector(reports[i]), rep_phi[i], model_type);
  }
  for (i in 1:n) {
    r[i] = R_to_growth(to_vector(R[i]), gt_mean[i, 1], gt_sd[i, 1]);
  }
}
