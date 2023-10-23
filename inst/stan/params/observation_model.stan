simplex[week_effect] day_of_week_simplex;// day of week reporting effect
array[obs_scale] real<lower = 0, upper = 1> frac_obs;     // fraction of cases that are ultimately observed
array[model_type] real<lower = 0> rep_phi;     // overdispersion of the reporting process
