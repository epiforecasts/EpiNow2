// Calculate the daily probability of reporting using parametric
// distributions up to the maximum observed delay.
// If sigma is 0 all the probability mass is put on n.
// Adapted from https://github.com/epiforecasts/epinowcast
// @author Sam Abbott
// @author Adrian Lison
vector discretised_pmf(real mu, real sigma, int n, int dist) {
  vector[n] pmf;
  if (sigma > 0) {
    vector[n + 1] upper_cdf;
    if (dist == 0) {
      for (i in 1:(n + 1)) {
        upper_cdf[i] = lognormal_cdf(i - 1, mu, sigma);
      }
    } else if (dist == 1) {
      real alpha = mu^2 / sigma^2;
      real beta = mu / sigma^2;
      for (i in 1:(n + 1)) {
        upper_cdf[i] = gamma_cdf(i - 1, alpha, beta);
      }
    } else {
      reject("Unknown distribution function provided.");
    }
    // discretise
    pmf = upper_cdf[2:(n + 1)] - upper_cdf[1:n];
    // normalize
    pmf = pmf / (upper_cdf[n + 1] - upper_cdf[1]);
  } else {
    // delta function
    pmf = rep_vector(0, n);
    pmf[n] = 1;
  }
  return(pmf);
}

// reverse a mf
vector reverse_mf(vector pmf) {
  int max_pmf = num_elements(pmf);
  vector[max_pmf] rev_pmf;
  for (d in 1:max_pmf) {
    rev_pmf[d] = pmf[max_pmf - d + 1];
  }
  return rev_pmf;
}

// combined fixed/variable pmfs
vector combine_pmfs(vector fixed_pmf, real[] pmf_mu, real[] pmf_sigma, int[] pmf_n, int[] dist, int len, int left_truncate, int reverse_pmf) {
  int n_fixed = num_elements(fixed_pmf);
  int n_variable = num_elements(pmf_mu);
  vector[len] pmf = rep_vector(0, len);
  if (n_fixed > 0) {
    pmf[1:n_fixed] = fixed_pmf;
  } else if (n_variable > 0) {
    pmf[1] = 1;
  }
  if (n_variable > 0) {
    for (s in 1:n_variable) {
      vector[pmf_n[s]] variable_pmf;
      variable_pmf = discretised_pmf(pmf_mu[s], pmf_sigma[s], pmf_n[s], dist[s]);
      pmf = convolve(pmf, variable_pmf, len);
    }
  }
  if (left_truncate) {
    pmf = append_row(
      rep_vector(0, left_truncate),
      pmf[(left_truncate + 1):len] / sum(pmf[(left_truncate + 1):len])
    );
  }
  if (reverse_pmf) {
    pmf = reverse_mf(pmf);
  }
  return(pmf);
}

void delays_lp(real[] delay_mean, real[] delay_mean_mean, real[] delay_mean_sd,
               real[] delay_sd, real[] delay_sd_mean, real[] delay_sd_sd,
               int[] delay_dist, int weight) {
    int mean_delays = num_elements(delay_mean);
    int sd_delays = num_elements(delay_sd);
    if (mean_delays) {
      for (s in 1:mean_delays) {
        if (delay_mean_sd[s] > 0) {
          // uncertain mean
          target += normal_lpdf(delay_mean[s] | delay_mean_mean[s], delay_mean_sd[s]) * weight;
          // if a distribution with postive support only truncate the prior
          if (delay_dist[s]) {
            target += -normal_lccdf(0 | delay_mean_mean[s], delay_mean_sd[s]) * weight;
          }
        }
      }
    }
    if (sd_delays) {
      for (s in 1:sd_delays) {
        if (delay_sd_sd[s] > 0) {
          // uncertain sd
          target += normal_lpdf(delay_sd[s] | delay_sd_mean[s], delay_sd_sd[s]) * weight;
          target += -normal_lccdf(0 | delay_sd_mean[s], delay_sd_sd[s]) * weight;
        }
     }
  }
}

vector seq(int base, int len, int rev) {
  vector[len] seq;
  for (i in 1:len) {
    seq[i] = rev ? len + base - i : base + i - 1;
  }
  return(seq);
}

real pmf_mean(vector pmf, int base, int rev) {
  int len = num_elements(pmf);
  vector[len] pmf_seq = seq(base, len, rev);
  return(dot_product(pmf_seq, pmf));
}

real pmf_var(vector pmf, int base, int rev, real mean) {
  int len = num_elements(pmf);
  vector[len] pmf_seq = seq(base, len, rev);
  for (i in 1:len) {
    pmf_seq[i] = pow(pmf_seq[i], 2);
  }
  return(dot_product(pmf_seq, pmf) - pow(mean, 2));
}
