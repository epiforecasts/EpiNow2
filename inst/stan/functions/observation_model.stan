// apply day of the week effect
vector day_of_week_effect(vector reports, array[] int day_of_week, vector effect) {
  int t = num_elements(reports);
  int wl = num_elements(effect);
  // scale day of week effect
  vector[wl] scaled_effect = wl * effect;
  vector[t] scaled_reports;
  for (s in 1:t) {
    // add reporting effects (adjust for simplex scale)
    scaled_reports[s] = reports[s] * scaled_effect[day_of_week[s]];
   }
  return(scaled_reports);
}
// Scale observations by fraction reported and update log density of
// fraction reported
vector scale_obs(vector reports, real frac_obs) {
  int t = num_elements(reports);
  vector[t] scaled_reports;
  scaled_reports = reports * frac_obs;
  return(scaled_reports);
}
// Truncate observed data by some truncation distribution
vector truncate(vector reports, vector trunc_rev_cmf, int reconstruct) {
  int t = num_elements(reports);
  vector[t] trunc_reports = reports;
  // Calculate cmf of truncation delay
  int trunc_max = min(t, num_elements(trunc_rev_cmf));
  int first_t = t - trunc_max + 1;
  // Apply cdf of truncation delay to truncation max last entries in reports
  if (reconstruct) {
    trunc_reports[first_t:t] ./= trunc_rev_cmf[1:trunc_max];
  } else {
    trunc_reports[first_t:t] .*= trunc_rev_cmf[1:trunc_max];
  }
  return(trunc_reports);
}
// Truncation distribution priors
void truncation_lp(array[] real truncation_mean, array[] real truncation_sd,
                   array[] real trunc_mean_mean, array[] real trunc_mean_sd,
                   array[] real trunc_sd_mean, array[] real trunc_sd_sd) {
  int truncation = num_elements(truncation_mean);
  if (truncation) {
    if (trunc_mean_sd[1] > 0) {
      // uncertain mean
      truncation_mean ~ normal(trunc_mean_mean, trunc_mean_sd);
    }
    if (trunc_sd_sd[1] > 0) {
      // uncertain sd
      truncation_sd ~ normal(trunc_sd_mean, trunc_sd_sd);
    }
  }
}
// update log density for reported cases
void report_lp(array[] int cases, array[] int cases_time, vector reports,
               array[] real rep_phi, real phi_mean, real phi_sd,
               int model_type, real weight, int accumulate) {
  int n = num_elements(cases) - accumulate; // number of observations
  vector[n] obs_reports; // reports at observation time
  array[n] int obs_cases; // observed cases at observation time
  if (accumulate) {
    int t = num_elements(reports);
    int current_obs = 0;
    obs_reports = rep_vector(0, n);
    for (i in 1:t) {
      if (current_obs > 0) { // first observation gets ignored when acucmulating
        obs_reports[current_obs] += reports[i];
      }
      if (i == cases_time[current_obs]) {
        current_obs += 1;
      }
    }
    obs_cases = cases[2:(n - 1)];
  } else {
    obs_reports = reports[cases_time];
    obs_cases = cases;
  }
  if (model_type) {
    real dispersion = 1 / pow(rep_phi[model_type], 2); 
    rep_phi[model_type] ~ normal(phi_mean, phi_sd) T[0,];
    if (weight == 1) {
      obs_cases ~ neg_binomial_2(obs_reports, dispersion);
    } else {
      target += neg_binomial_2_lpmf(
        obs_cases | obs_reports, dispersion
      ) * weight;
    }
  } else {
    if (weight == 1) {
      obs_cases ~ poisson(obs_reports);
    } else {
      target += poisson_lpmf(obs_cases | obs_reports) * weight;
    }
  }
}
// update log likelihood (as above but not vectorised and returning log likelihood)
vector report_log_lik(array[] int cases, vector reports,
                      array[] real rep_phi, int model_type, real weight) {
  int t = num_elements(reports);
  vector[t] log_lik;

  // defer to poisson if phi is large, to avoid overflow
  if (model_type == 0) {
    for (i in 1:t) {
      log_lik[i] = poisson_lpmf(cases[i] | reports[i]) * weight;
    }
  } else {
    real dispersion = 1 / pow(rep_phi[model_type], 2);
    for (i in 1:t) {
      log_lik[i] = neg_binomial_2_lpmf(cases[i] | reports[i], dispersion) * weight;
    }
  }
  return(log_lik);
}
// sample reported cases from the observation model
array[] int report_rng(vector reports, array[] real rep_phi, int model_type) {
  int t = num_elements(reports);
  array[t] int sampled_reports;
  real dispersion = 1e5;
  if (model_type) {
    dispersion = 1 / pow(rep_phi[model_type], 2);
  }
    
  for (s in 1:t) {
    if (reports[s] < 1e-8) {
      sampled_reports[s] = 0;
    } else {
      // defer to poisson if phi is large, to avoid overflow
      if (dispersion > 1e4) {
        sampled_reports[s] = poisson_rng(reports[s] > 1e8 ? 1e8 : reports[s]);
      } else {
        sampled_reports[s] = neg_binomial_2_rng(reports[s] > 1e8 ? 1e8 : reports[s], dispersion);
      }
    }
  }
  return(sampled_reports);
}
