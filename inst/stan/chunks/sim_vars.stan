array[delay_n_p] real delay_mean = delay_mean_samples[i];
array[delay_n_p] real delay_sd = delay_sd_samples[i];
array[obs_scale] real frac_obs = frac_obs_samples[i];
array[model_type] real rep_phi = rep_phi_samples[i];
vector[week_effect] day_of_week_simplex = to_vector(
  day_of_week_simplex_samples[i]
);
