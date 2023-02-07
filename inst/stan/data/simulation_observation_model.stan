  int day_of_week[t - seeding_time]; // day of the week indicator (1 - 7)
  int week_effect;                   // should a day of the week effect be estimated
  real<lower = 0> day_of_week_simplex[n, week_effect];
  int obs_scale;
  real<lower = 0, upper = 1> frac_obs[n, obs_scale];
  int model_type;
  real<lower = 0> rep_phi[n, model_type];  // overdispersion of the reporting process

