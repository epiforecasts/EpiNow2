  array[t - seeding_time] int day_of_week; // day of the week indicator (1 - 7)
  int week_effect;                   // should a day of the week effect be estimated
  array[n, week_effect] real<lower = 0> day_of_week_simplex_samples;
  int obs_scale;
  array[n, obs_scale] real<lower = 0, upper = 1> frac_obs_samples;
  int model_type;
  array[n, model_type] real<lower = 0> rep_phi_samples;  // overdispersion of the reporting process
  int<lower = 0> trunc_id; // id of truncation
