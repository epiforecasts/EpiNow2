/**
 * Get the maximum delay length for each delay type.
 *
 * @param delay_types The number of delay types.
 * @param delay_types_p Array indicating if each delay is parametric (1) or non-parametric (0).
 * @param delay_types_id Array mapping each delay to its corresponding ID.
 * @param delay_types_groups Array of indices defining the grouping of delays by type.
 * @param delay_max Array of maximum delay lengths for parametric delays.
 * @param delay_np_pmf_groups Array of indices defining the grouping of non-parametric PMFs.
 * @return An array of maximum delay lengths for each delay type.
 */
array[] int get_delay_type_max(
  int delay_types, array[] int delay_types_p, array[] int delay_types_id,
  array[] int delay_types_groups, array[] int delay_max, array[] int delay_np_pmf_groups
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
 * Get the reversed probability mass function (PMF) for a specific delay.
 *
 * @param delay_id The ID of the delay.
 * @param len The desired length of the output PMF.
 * @param delay_types_p Array indicating if each delay is parametric (1) or non-parametric (0).
 * @param delay_types_id Array mapping each delay to its corresponding ID.
 * @param delay_types_groups Array of indices defining the grouping of delays by type.
 * @param delay_max Array of maximum delay lengths for parametric delays.
 * @param delay_np_pmf Array of non-parametric PMF values.
 * @param delay_np_pmf_groups Array of indices defining the grouping of non-parametric PMFs.
 * @param delay_params Array of parameters for parametric delays.
 * @param delay_params_groups Array of indices defining the grouping of parametric delay parameters.
 * @param delay_dist Array of distribution types for parametric delays.
 * @param left_truncate The number of time steps to left-truncate the PMF.
 * @param reverse_pmf Flag indicating whether to reverse the output PMF.
 * @param cumulative Flag indicating whether to return the cumulative PMF.
 * @return A vector representing the reversed PMF for the specified delay.
 */
vector get_delay_rev_pmf(
  int delay_id, int len, array[] int delay_types_p, array[] int delay_types_id,
  array[] int delay_types_groups, array[] int delay_max,
  vector delay_np_pmf, array[] int delay_np_pmf_groups,
  vector delay_params, array[] int delay_params_groups, array[] int delay_dist,
  int left_truncate, int reverse_pmf, int cumulative
) {
  vector[len] pmf = rep_vector(0, len);
  int current_len = 1;
  
  for (i in delay_types_groups[delay_id]:(delay_types_groups[delay_id + 1] - 1)) {
    vector[delay_max[delay_types_id[i]] + 1] new_variable_pmf;
    int new_len;
    
    if (delay_types_p[i]) {
      int start = delay_params_groups[delay_types_id[i]];
      int end = delay_params_groups[delay_types_id[i] + 1] - 1;
      new_variable_pmf = discretised_pmf(
        delay_params[start:end],
        delay_max[delay_types_id[i]] + 1,
        delay_dist[delay_types_id[i]]
      );
      new_len = current_len + delay_max[delay_types_id[i]];
    } else {
      int start = delay_np_pmf_groups[delay_types_id[i]];
      int end = delay_np_pmf_groups[delay_types_id[i] + 1] - 1;
      new_variable_pmf = delay_np_pmf[start:end];
      new_len = current_len + end - start;
    }
    
    if (current_len == 1) {
      pmf[1:new_len] = new_variable_pmf;
    } else {
      pmf[1:new_len] = convolve_with_rev_pmf(
        pmf[1:current_len], reverse(new_variable_pmf), new_len
      );
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
        // uncertain mean
        target += normal_lpdf(
          delay_params[s] | delay_params_mean[s], delay_params_sd[s]
        ) * weight[d];
        // if a distribution with postive support only truncate the prior
        if (delay_dist[d] == 1) {
          target += -normal_lccdf(
            0 | delay_params_mean[s], delay_params_sd[s]
          ) * weight[d];
        }
      }
    }
  }
}

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
