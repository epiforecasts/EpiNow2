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
  for (i in delay_types_groups[delay_id]:(delay_types_groups[delay_id + 1] - 1)) {
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
          pmf[1:current_len], reverse_mf(new_variable_pmf), new_len
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
          pmf[1:current_len], reverse_mf(delay_np_pmf[start:end]), new_len
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
    pmf = reverse_mf(pmf);
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
        if (weight[d] > 1) {
          target += weight[d] * normal_lpdf(delay_params[s] | delay_params_mean[s], delay_params_sd[s]);
        }else {
          delay_params[s] ~ normal(delay_params_mean[s], delay_params_sd[s]);
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
