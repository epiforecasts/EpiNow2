// convolve two vectors
// length of x
vector convolve(vector x, vector y, int len) {
  int xlen = num_elements(x);
  int ylen = num_elements(y);
  vector[len] convolution = rep_vector(0, len);
  for (i in 1:xlen) {
    for (j in 1:(min(len - i + 1, ylen))) {
      convolution[i + j - 1] += x[i] * y[j];
    }
  }
  return(convolution);
}

// convolve latent infections to reported (but still unobserved) cases
vector convolve_to_report(vector infections,
                          vector delay_pmf,
                          int seeding_time) {
  int t = num_elements(infections);
  vector[t - seeding_time] reports;
  vector[t] unobs_reports = infections;
  int delays = num_elements(delay_pmf);
  if (delays) {
    unobs_reports = convolve(unobs_reports, delay_pmf, t);
    reports = unobs_reports[(seeding_time + 1):t];
  } else {
    reports = infections[(seeding_time + 1):t];
  }
  return(reports);
}

void delays_lp(real[] delay_mean, real[] delay_mean_mean, real[] delay_mean_sd,
               real[] delay_sd, real[] delay_sd_mean, real[] delay_sd_sd, int weight){
    int mean_delays = num_elements(delay_mean);
    int sd_delays = num_elements(delay_sd);
    if (mean_delays) {
      for (s in 1:mean_delays) {
        if (delay_mean_sd[s] > 0) {
          // uncertain mean
          target += normal_lpdf(delay_mean[s] | delay_mean_mean[s], delay_mean_sd[s]) * weight;
        }
      }
    }
    if (sd_delays) {
      for (s in 1:sd_delays) {
        if (delay_sd_sd[s] > 0) {
          // uncertain sd
          target += normal_lpdf(delay_sd[s] | delay_sd_mean[s], delay_sd_sd[s]) * weight;
        }
     }
  }
}

// combined fixed/variable pmfs
vector combine_pmfs(vector fixed_pmf, real[] pmf_mu, real[] pmf_sigma, int[] pmf_n, int[] dist, int len, int truncate) {
  int n_fixed = num_elements(fixed_pmf);
  int n_variable = num_elements(pmf_mu);
  vector[len] pmf = rep_vector(0, len);
  if (n_fixed > 0) {
    pmf[1:n_fixed] = fixed_pmf;
  } else if (n_variable > 0) {
    pmf[1] = 1;
  }
  for (s in 1:n_variable) {
    vector[pmf_n[s]] variable_pmf;
    variable_pmf = discretised_pmf(pmf_mu[s], pmf_sigma[s], pmf_n[s], dist[s], truncate);
    pmf = convolve(pmf, variable_pmf, len);
  }
  return(pmf);
}
