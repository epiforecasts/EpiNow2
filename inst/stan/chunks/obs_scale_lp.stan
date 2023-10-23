if (obs_scale) {
  frac_obs[1] ~ normal(obs_scale_mean, obs_scale_sd) T[0, 1];
}
