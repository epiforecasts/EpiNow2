// apply day of the week effect
vector day_of_week_effect(vector reports, int[] day_of_week, vector effect) {
  int t = num_elements(reports);
  // scale day of week effect
  vector[7] scaled_effect = 7 * effect;
  vector[t] scaled_reports;
  for (s in 1:t) {
    // add reporting effects (adjust for simplex scale)
    scaled_reports[s] = reports[s] * scaled_effect[day_of_week[s]];
   }
  return(scaled_reports);
}
// Scale observations by fraction reported
vector scale_obs(vector reports, real frac_obs, int t) {
  vector[t] scaled_reports;
  scaled_reports = reports * frac_obs;
  return(scaled_reports);
}
// Scale observations by fraction reported and update log density of 
// fraction reported
vector scale_observations_lp(vector reports, real frac_obs,
                             real frac_mean, real frac_sd) {
  int t = num_elements(reports);
  vector[t] scaled_reports;
  frac_obs ~ normal(frac_mean, frac_sd) T[0, 1];
  scaled_reports = scale_obs(reports, frac_obs, t);
  return(scaled_reports);
}
// update log density for reported cases
void report_lp(int[] cases, vector reports, 
               real[] rep_phi, int phi_prior,
               int model_type, int horizon,
               real weight) {
  int t = num_elements(reports) - horizon;
  real sqrt_phi;
  if (model_type) {
    // the reciprocal overdispersion parameter (phi)
    rep_phi[model_type] ~ normal(0, phi_prior) T[0,];
    sqrt_phi = 1 / sqrt(rep_phi[model_type]);
    // defer to poisson if phi is large, to avoid overflow
    if (sqrt_phi > 1e4) {
      target += poisson_lpmf(cases | reports[1:t]) * weight;
    } else {
      target += neg_binomial_2_lpmf(cases | reports[1:t], sqrt_phi) * weight;
    }
  } else {
    target += poisson_lpmf(cases | reports[1:t]) * weight;
  }
}
