// convolve two vectors
// length of x
// produces a convolution across the lenght specified
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

// convolve two vectors as a backwards dot product
// y vector shoud be reversed
// limited to the length of x and backwards looking for x indexes
// designed for use convolve a case vector and a delay pmf
vector convolve_dot_product(vector x, vector y, int len) {
    int ylen = num_elements(y);
    vector[len] z;
    for (s in 1:len) {
        z[s] = 1e-5 + // add a small value to ensure numerical stability
          dot_product(
            x[max(1, (s - ylen + 1)):s], tail(y, min(ylen, s))
          );
    }
   return(z);
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
    unobs_reports = convolve_dot_product(unobs_reports, delay_pmf, t);
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
