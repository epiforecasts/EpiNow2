// reverse a pmf
vector reverse_pmf(vector pmf, int max_pmf) {
  vector[max_pmf] rev_pmf;
  for (d in 1:max_pmf) {
    rev_pmf[d] = pmf[max_pmf - d + 1];
  }
  return rev_pmf;
}

// convolve a pmf and case vector 
vector convolve(vector cases, vector pmf) {
    int t = num_elements(cases);
    int max_pmf = num_elements(pmf);
    vector[max_pmf] rev_pmf = reverse_pmf(pmf, max_pmf);
    vector[t] convolved_cases = rep_vector(1e-5, t);
    for (s in 1:t) {
        convolved_cases[s] += dot_product(cases[max(1, (s - max_pmf + 1)):s],
                                          tail(rev_pmf, min(max_pmf, s)));
    }
   return(convolved_cases);
  }

// convolve multiple pmfs
vector convolve_pmfs(real[,] delay_pmfs, int[] max_delay, int delays) {
  int conv_delay = sum(max_delay);
  vector[conv_delay] proc_pmf;
  vector[conv_delay] conv_pmf = rep_vector(0, conv_delay); 
  conv_pmf[1:max_delay[1]] = to_vector(delay_pmfs[1,]);
  for (s in 2:delays) {
    //P(Z = z) = sum_over_x(P(X = x) * P(Y = z - x))
    // indexing tweaked to account for starting at 1 in stan
    // this makes z = x an allowed contribution
    proc_pmf = rep_vector(0, conv_delay);
    for (z in 1:conv_delay) {
      for (x in 1:max_delay[s]){
        if (z - x >= 0) { 
         proc_pmf[z] += delay_pmfs[s, x] * conv_pmf[z - x + 1];
        }
      }
    }
    conv_pmf = proc_pmf;
  }
  return(conv_pmf);
}

// convolve latent infections to reported (but still unobserved) cases
vector convolve_to_report(vector infections, 
                          real[] delay_mean, 
                          real[] delay_sd,
                          int[] max_delay,
                          int seeding_time) {
  int t = num_elements(infections);
  vector[t - seeding_time] reports;
  vector[t] unobserved_reports;
  int delays = num_elements(delay_mean);
  if (delays) {
    real delay_pmfs[delays, max(max_delay)];
    int conv_delay = sum(max_delay);
    vector[conv_delay] conv_pmf;
    for (s in 1:delays) {
      for (j in 1:(max_delay[s])) {
        delay_pmfs[s, j] =
        discretised_lognormal_pmf(j - 1, delay_mean[s], delay_sd[s], max_delay[s]);
      }
    }
    if (delays == 1) {
      conv_pmf = to_vector(delay_pmfs[1,]);
    }else{
      conv_pmf = convolve_pmfs(delay_pmfs, max_delay, delays);
    }
    unobserved_reports = convolve(infections, conv_pmf);
    reports = unobserved_reports[(seeding_time + 1):t];
  }else{
    reports = infections[(seeding_time + 1):t];
  }
  return(reports);
}

void delays_lp(real[] delay_mean, real[] delay_mean_mean, real[] delay_mean_sd, 
               real[] delay_sd, real[] delay_sd_mean, real[] delay_sd_sd, int weight){
    int delays = num_elements(delay_mean);
    if (delays) {
    for (s in 1:delays) {
      target += normal_lpdf(delay_mean[s] | delay_mean_mean[s], delay_mean_sd[s]) * weight;
      target += normal_lpdf(delay_sd[s] | delay_sd_mean[s], delay_sd_sd[s]) * weight;
    }
  }
}
