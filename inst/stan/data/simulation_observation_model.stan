array[t - seeding_time] int day_of_week; // day of the week indicator (1 - 7)
int week_effect; // should a day of the week effect be estimated
array[n, week_effect] real<lower = 0> day_of_week_simplex;
int obs_scale;
int model_type;
int<lower = 0> trunc_id; // id of truncation
