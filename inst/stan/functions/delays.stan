/**
 * Delay Distribution Functions
 *
 * This group of functions handles the creation, manipulation, and application
 * of delay distributions in the model. Delay distributions represent the time
 * between events (e.g., infection to symptom onset, symptom onset to reporting).
 *
 */

/**
 * Get the maximum delay for each delay type
 *
 * @param delay_types Number of delay types
 * @param delay_types_p Array indicating whether each delay is parametric (1) or non-parametric (0)
 * @param delay_types_id Array mapping delay types to their respective IDs
 * @param delay_types_groups Array of indices defining groups of delay types
 * @param delay_max Array of maximum delays for parametric distributions
 * @param delay_np_pmf_groups Array of indices for accessing non-parametric PMFs
 * @return An array of maximum delays for each delay type
 */
array[] int get_delay_type_max(
  int delay_types, array[] int delay_types_p, array[] int delay_types_id,
  array[] int delay_types_groups, array[] int delay_max,
  array[] int delay_np_pmf_groups
) {
  array[delay_types] int ret;
  for (i in 1:delay_types) {
    ret[i] = 0;
    for (j in delay_types_groups[i]:(delay_types_groups[i + 1] - 1)) {
      if (delay_types_p[j]) { // parametric
        ret[i] += delay_max[delay_types_id[j]];
      } else { // nonparametric
        ret[i] += delay_np_pmf_groups[delay_types_id[j] + 1] -
          delay_np_pmf_groups[delay_types_id[j]] - 1;
      }
    }
  }
  return ret;
}

/**
 * Get the reversed probability mass function for a delay
 *
 * @param delay_id Identifier for the specific delay distribution
 * @param len Length of the output PMF
 * @param delay_types_p Array indicating whether each delay is parametric (1) or non-parametric (0)
 * @param delay_types_id Array mapping delay types to their respective IDs
 * @param delay_types_groups Array of indices defining groups of delay types
 * @param delay_max Array of maximum delays for parametric distributions
 * @param delay_np_pmf Vector of probability mass functions for non-parametric delays
 * @param delay_np_pmf_groups Array of indices for accessing non-parametric PMFs
 * @param delay_params Vector of parameters for parametric delay distributions
 * @param delay_params_groups Array of indices for accessing delay parameters
 * @param delay_dist Array of distribution types (0: lognormal, 1: gamma)
 * @param left_truncate Whether to left-truncate the PMF (1) or not (0)
 * @param reverse_pmf Whether to reverse the PMF (1) or not (0)
 * @param cumulative Whether to return cumulative (1) or daily (0) values
 * @return A vector containing the (reversed) PMF of length len
 */
vector get_delay_rev_pmf(
  int delay_id, int len, array[] int delay_types_p, array[] int delay_types_id,
  array[] int delay_types_groups, array[] int delay_max,
  vector delay_np_pmf, array[] int delay_np_pmf_groups,
  vector delay_params, array[] int delay_params_groups, array[] int delay_dist,
  int left_truncate, int reverse_pmf, int cumulative
) {
  // loop over delays
  vector[len] pmf = rep_vector(0, len);
  int current_len = 1;
  int new_len;
  for (i in
         delay_types_groups[delay_id]:(delay_types_groups[delay_id + 1] - 1)) {
    if (delay_types_p[i]) { // parametric
      int start = delay_params_groups[delay_types_id[i]];
      int end = delay_params_groups[delay_types_id[i] + 1] - 1;
      vector[delay_max[delay_types_id[i]] + 1] new_variable_pmf =
        discretised_pmf(
          delay_params[start:end],
          delay_max[delay_types_id[i]] + 1,
          delay_dist[delay_types_id[i]]
      );
      new_len = current_len + delay_max[delay_types_id[i]];
      if (current_len == 1) { // first delay
        pmf[1:new_len] = new_variable_pmf;
      } else { // subsequent delay to be convolved
        pmf[1:new_len] = convolve_with_rev_pmf(
          pmf[1:current_len], reverse(new_variable_pmf), new_len
        );
      }
    } else { // nonparametric
      int start = delay_np_pmf_groups[delay_types_id[i]];
      int end = delay_np_pmf_groups[delay_types_id[i] + 1] - 1;
      new_len = current_len + end - start;
      if (current_len == 1) { // first delay
        pmf[1:new_len] = delay_np_pmf[start:end];
      } else { // subsequent delay to be convolved
        pmf[1:new_len] = convolve_with_rev_pmf(
          pmf[1:current_len], reverse(delay_np_pmf[start:end]), new_len
        );
      }
    }
    current_len = new_len;
  }
  if (left_truncate) {
    pmf = append_row(
      rep_vector(0, left_truncate),
      pmf[(left_truncate + 1):len] / sum(pmf[(left_truncate + 1):len])
    );
  }
  if (cumulative) {
    pmf = cumulative_sum(pmf);
  }
  if (reverse_pmf) {
    pmf = reverse(pmf);
  }
  return pmf;
}

/**
 * Update log density for delay distribution priors
 *
 * @param delay_params Vector of parameters for parametric delay distributions
 * @param delay_params_mean Vector of prior means for delay parameters
 * @param delay_params_sd Vector of prior standard deviations for delay parameters
 * @param delay_params_groups Array of indices for accessing delay parameters
 * @param delay_dist Array of distribution types (0: lognormal, 1: gamma)
 * @param weight Array of weights for each delay distribution in the log density
 */
void delays_lp(vector delay_params,
               vector delay_params_mean, vector delay_params_sd,
               array[] int delay_params_groups,
               array[] int delay_dist, array[] int weight) {
  int n_delays = num_elements(delay_params_groups) - 1;
  if (n_delays == 0) {
    return;
  }
  for (d in 1:n_delays) {
    int start = delay_params_groups[d];
    int end = delay_params_groups[d + 1] - 1;
    for (s in start:end) {
      if (delay_params_sd[s] > 0) {
        if (weight[d] > 1) {
          target += weight[d] *
            normal_lpdf(
              delay_params[s] | delay_params_mean[s], delay_params_sd[s]
            );
        } else {
          delay_params[s] ~ normal(delay_params_mean[s], delay_params_sd[s]);
        }
      }
    }
  }
}

/**
 * Generate random samples from a normal distribution with lower bounds
 *
 * @param mu Vector of means
 * @param sigma Vector of standard deviations
 * @param lb Vector of lower bounds
 * @return A vector of random samples from the truncated normal distribution
 */
vector normal_lb_rng(vector mu, vector sigma, vector lb) {
  int len = num_elements(mu);
  vector[len] ret;
  for (i in 1:len) {
    real p = normal_cdf(lb[i] | mu[i], sigma[i]);  // cdf for bounds
    real u = uniform_rng(p, 1);
    ret[i] = (sigma[i] * inv_Phi(u)) + mu[i];  // inverse cdf for value
  }
  return ret;
}
