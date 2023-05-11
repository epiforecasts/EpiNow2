int[] get_delay_type_max(
  int delay_types, int[] delay_types_p, int[] delay_types_id,
  int[] delay_types_groups, int[] delay_max, int[] delay_np_pmf_groups
) {
  int ret[delay_types];
  for (i in 1:delay_types) {
    ret[i] = 1;
    for (j in delay_types_groups[i]:(delay_types_groups[i + 1] - 1)) {
      if (delay_types_p[j]) { // parametric
        ret[i] += delay_max[delay_types_id[j]] - 1;
      } else { // nonparametric
        ret[i] += delay_np_pmf_groups[delay_types_id[j] + 1] -
          delay_np_pmf_groups[delay_types_id[j]] - 1;
      }
    }
  }
  return ret;
}

vector get_delay_rev_pmf(
  int delay_id, int len, int[] delay_types_p, int[] delay_types_id,
  int[] delay_types_groups, int[] delay_max,
  vector delay_np_pmf, int[] delay_np_pmf_groups,
  real[] delay_mean, real[] delay_sigma, int[] delay_dist,
  int left_truncate, int reverse_pmf, int cumulative
) {
  // loop over delays
  vector[len] pmf = rep_vector(0, len);
  int current_len = 1;
  int new_len;
  for (i in delay_types_groups[delay_id]:(delay_types_groups[delay_id + 1] - 1)) {
    if (delay_types_p[i]) { // parametric
      vector[delay_max[delay_types_id[i]]] new_variable_pmf =
        discretised_pmf(
          delay_mean[delay_types_id[i]],
          delay_sigma[delay_types_id[i]],
          delay_max[delay_types_id[i]],
          delay_dist[delay_types_id[i]]
      );
      new_len = current_len + delay_max[delay_types_id[i]] - 1;
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
          pmf[1:current_len], delay_np_pmf[end:start], new_len
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


void delays_lp(real[] delay_mean, real[] delay_mean_mean, real[] delay_mean_sd,
               real[] delay_sd, real[] delay_sd_mean, real[] delay_sd_sd,
               int[] delay_dist, int[] weight) {
    int mean_delays = num_elements(delay_mean);
    int sd_delays = num_elements(delay_sd);
    if (mean_delays) {
      for (s in 1:mean_delays) {
        if (delay_mean_sd[s] > 0) {
          // uncertain mean
          target += normal_lpdf(delay_mean[s] | delay_mean_mean[s], delay_mean_sd[s]) * weight[s];
          // if a distribution with postive support only truncate the prior
          if (delay_dist[s]) {
            target += -normal_lccdf(0 | delay_mean_mean[s], delay_mean_sd[s]) * weight[s];
          }
        }
      }
    }
    if (sd_delays) {
      for (s in 1:sd_delays) {
        if (delay_sd_sd[s] > 0) {
          // uncertain sd
          target += normal_lpdf(delay_sd[s] | delay_sd_mean[s], delay_sd_sd[s]) * weight[s];
          target += -normal_lccdf(0 | delay_sd_mean[s], delay_sd_sd[s]) * weight[s];
        }
     }
  }
}
