/**
 * Apply day of the week effect to reports
 *
 * This function applies a day of the week effect to a vector of reports.
 *
 * @param reports Vector of reports to be adjusted.
 * @param day_of_week Array of integers representing the day of the week for
 * each report.
 * @param effect Vector of day of week effects.
 *
 * @return A vector of reports adjusted for day of the week effects.
 */
vector day_of_week_effect(vector reports, array[] int day_of_week,
                          vector effect) {
  int wl = num_elements(effect);
  vector[wl] scaled_effect = wl * effect;
  return reports .* scaled_effect[day_of_week];
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
 * This function truncates a vector of reports based on a truncation
 * distribution.
 *
 * @param reports Vector of reports to be truncated.
 * @param trunc_rev_cmf Vector representing the reverse cumulative mass function
 * of the truncation distribution.
 * @param reconstruct Integer flag indicating whether to reconstruct (1) or
 * truncate (0) the data.
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
 * @param trunc_mean_mean Array of real values for mean of truncation mean
 * prior.
 * @param trunc_mean_sd Array of real values for standard deviation of
 * truncation mean prior.
 * @param trunc_sd_mean Array of real values for mean of truncation standard
 * deviation prior.
 * @param trunc_sd_sd Array of real values for standard deviation of truncation
 * standard deviation prior.
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
 * This function updates the log density for reported cases based on the
 * specified model type.
 *
 * @param cases Array of integer observed cases.
 * @param case_times Array of integer time indices for observed cases.
 * @param reports Vector of expected reports.
 * @param rep_phi Real values for reporting overdispersion.
 * @param model_type Integer indicating the model type (0 for Poisson, >0 for
 * Negative Binomial).
 * @param weight Real value for weighting the log density contribution.
 * @param accumulate Array of integers indicating, for each time point, whether
 * to accumulate reports (1) or not (0).
 */
void report_lp(array[] int cases, array[] int case_times, vector reports,
               real rep_phi, int model_type, real weight) {
  int n = num_elements(case_times); // number of observations
  vector[n] obs_reports = reports[case_times]; // reports at observation time
  if (model_type) {
    real dispersion = inv_square(rep_phi);
    if (weight == 1) {
      cases ~ neg_binomial_2(obs_reports, dispersion);
    } else {
      target += neg_binomial_2_lpmf(
        cases | obs_reports, dispersion
      ) * weight;
    }
  } else {
    if (weight == 1) {
      cases ~ poisson(obs_reports);
    } else {
      target += poisson_lpmf(cases | obs_reports) * weight;
    }
  }
}

/**
 * Accumulate reports according to a binary flag at each time point
 *
 * This function accumulates reports according to a binary flag at each time point.
 *
 * @param reports Vector of expected reports.
 * @param accumulate Array of integers indicating, for each time point, whether to accumulate or not.
 *
 * @return A vector of accumulated reports.
 */
vector accumulate_reports(vector reports, array[] int accumulate) {
  int ot_h = num_elements(reports); // number of reporting time points modelled
  vector[ot_h] accumulated_reports = reports;
  for (i in 1:(ot_h - 1)) {
    if (accumulate[i]) { // first observation gets ignored when accumulating
      accumulated_reports[i + 1] += accumulated_reports[i];
    }
  }
  return accumulated_reports;
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
                      real rep_phi, int model_type, real weight) {
  int t = num_elements(reports);
  vector[t] log_lik;

  // defer to poisson if phi is large, to avoid overflow
  if (model_type == 0) {
    for (i in 1:t) {
      log_lik[i] = poisson_lpmf(cases[i] | reports[i]) * weight;
    }
  } else {
    real dispersion = inv_square(rep_phi);
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
 * @param rep_phi Real value for reporting overdispersion.
 * @param model_type Integer indicating the model type (0 for Poisson, >0 for Negative Binomial).
 *
 * @return An array of integer sampled reports.
 */
array[] int report_rng(vector reports, real rep_phi, int model_type) {
  int t = num_elements(reports);
  array[t] int sampled_reports;
  real dispersion = 1e5;
  if (model_type) {
    dispersion = inv_square(rep_phi);
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
