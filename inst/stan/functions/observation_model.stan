/**
 * Apply day of the week effect to reports
 *
 * This function applies a day of the week effect to a vector of reports.
 *
 * @param reports Vector of reports to be adjusted.
 * @param day_of_week Array of integers representing the day of the week for each report.
 * @param effect Vector of day of week effects.
 *
 * @return A vector of reports adjusted for day of the week effects.
 */
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

/**
 * Scale observations by fraction reported
 *
 * This function scales a vector of reports by a fraction observed.
 *
 * @param reports Vector of reports to be scaled.
 * @param frac_obs Real value representing the fraction observed.
 *
 * @return A vector of scaled reports.
 */
vector scale_obs(vector reports, real frac_obs) {
  int t = num_elements(reports);
  vector[t] scaled_reports;
  scaled_reports = reports * frac_obs;
  return(scaled_reports);
}

/**
 * Truncate observed data by a truncation distribution
 *
 * This function truncates a vector of reports based on a truncation distribution.
 *
 * @param reports Vector of reports to be truncated.
 * @param trunc_rev_cmf Vector representing the reverse cumulative mass function of the truncation distribution.
 * @param reconstruct Integer flag indicating whether to reconstruct (1) or truncate (0) the data.
 *
 * @return A vector of truncated reports.
 */
vector truncate_obs(vector reports, vector trunc_rev_cmf, int reconstruct) {
  int t = num_elements(reports);
  int trunc_max = num_elements(trunc_rev_cmf);
  vector[t] trunc_reports = reports;
  // Calculate cmf of truncation delay
  int joint_max = min(t, trunc_max);
  int first_t = t - joint_max + 1;
  int first_trunc = trunc_max - joint_max + 1;

  // Apply cdf of truncation delay to truncation max last entries in reports
  if (reconstruct) {
    trunc_reports[first_t:t] ./= trunc_rev_cmf[first_trunc:trunc_max];
  } else {
    trunc_reports[first_t:t] .*= trunc_rev_cmf[first_trunc:trunc_max];
  }
  return(trunc_reports);
}

/**
 * Update log density for truncation distribution priors
 *
 * This function updates the log density for truncation distribution priors.
 *
 * @param truncation_mean Array of real values for truncation mean.
 * @param truncation_sd Array of real values for truncation standard deviation.
 * @param trunc_mean_mean Array of real values for mean of truncation mean prior.
 * @param trunc_mean_sd Array of real values for standard deviation of truncation mean prior.
 * @param trunc_sd_mean Array of real values for mean of truncation standard deviation prior.
 * @param trunc_sd_sd Array of real values for standard deviation of truncation standard deviation prior.
 */
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

/**
 * Update log density for reported cases
 *
 * This function updates the log density for reported cases based on the specified model type.
 *
 * @param cases Array of integer observed cases.
 * @param cases_time Array of integer time indices for observed cases.
 * @param reports Vector of expected reports.
 * @param rep_phi Array of real values for reporting overdispersion.
 * @param phi_mean Real value for mean of reporting overdispersion prior.
 * @param phi_sd Real value for standard deviation of reporting overdispersion prior.
 * @param model_type Integer indicating the model type (0 for Poisson, >0 for Negative Binomial).
 * @param weight Real value for weighting the log density contribution.
 * @param accumulate Integer flag indicating whether to accumulate reports (1) or not (0).
 */
void report_lp(array[] int cases, array[] int cases_time, vector reports,
               array[] real rep_phi, real phi_mean, real phi_sd,
               int model_type, real weight, int accumulate) {
  int n = num_elements(cases_time) - accumulate; // number of observations
  vector[n] obs_reports; // reports at observation time
  array[n] int obs_cases; // observed cases at observation time
  if (accumulate) {
    int t = num_elements(reports);
    int i = 0;
    int current_obs = 0;
    obs_reports = rep_vector(0, n);
    while (i <= t && current_obs <= n) {
      if (current_obs > 0) { // first observation gets ignored when accumulating
        obs_reports[current_obs] += reports[i];
      }
      if (i == cases_time[current_obs + 1]) {
        current_obs += 1;
      }
      i += 1;
    }
    obs_cases = cases[2:(n + 1)];
  } else {
    obs_reports = reports[cases_time];
    obs_cases = cases;
  }
  if (model_type) {
    real dispersion = inv_square(phi_sd > 0 ? rep_phi[model_type] : phi_mean);
    if (phi_sd > 0) {
      rep_phi[model_type] ~ normal(phi_mean, phi_sd) T[0,];
    }
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

/**
 * Calculate log likelihood for reported cases
 *
 * This function calculates the log likelihood for reported cases based on the specified model type.
 *
 * @param cases Array of integer observed cases.
 * @param reports Vector of expected reports.
 * @param rep_phi Array of real values for reporting overdispersion.
 * @param model_type Integer indicating the model type (0 for Poisson, >0 for Negative Binomial).
 * @param weight Real value for weighting the log likelihood contribution.
 *
 * @return A vector of log likelihoods for each time point.
 */
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
    real dispersion = inv_square(rep_phi[model_type]);
    for (i in 1:t) {
      log_lik[i] = neg_binomial_2_lpmf(cases[i] | reports[i], dispersion) * weight;
    }
  }
  return(log_lik);
}

/**
 * Generate random samples of reported cases
 *
 * This function generates random samples of reported cases based on the specified model type.
 *
 * @param reports Vector of expected reports.
 * @param rep_phi Array of real values for reporting overdispersion.
 * @param model_type Integer indicating the model type (0 for Poisson, >0 for Negative Binomial).
 *
 * @return An array of integer sampled reports.
 */
array[] int report_rng(vector reports, array[] real rep_phi, int model_type) {
  int t = num_elements(reports);
  array[t] int sampled_reports;
  real dispersion = 1e5;
  if (model_type) {
    dispersion = inv_square(rep_phi[model_type]);
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
