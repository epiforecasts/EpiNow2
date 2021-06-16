// convolve a pdf and case vector
vector convolve(vector cases, vector pmf, int length) {
    int t = num_elements(cases);
    int pos = 1;
    vector[length] seg_pmf;
    vector[t] conv_cases = rep_vector(1e-5, t);
    for (s in 1:t) {
        seg_pmf = segment(pmf, pos, length);
        conv_cases[s] += dot_product(cases[max(1, (s - length + 1)):s],
                                     tail(seg_pmf, min(length, s)));
        pos = pos + 1;
    }
   return(conv_cases);
  }

vector convolve_pmfs(vector pmfs, int[] max_delay, int delays, int reverse) {
  int conv_delay = sum(max_delay);
  vector[conv_delay] proc_pmf;
  vector[conv_delay] conv_pmf = rep_vector(0, conv_delay); 
  conv_pmf[1:max_delay[1]] = head(pmfs, max_delay[1]);
  if (delays) {
    for (s in 2:delays) {
      //P(Z = z) = sum_over_x(P(X = x) * P(Y = z - x))
      // indexing tweaked to account for starting at 1 in stan
      // this makes z = x an allowed contribution
      proc_pmf = rep_vector(0, conv_delay);
      for (z in 1:conv_delay) {
        for (x in 1:max_delay[s]){
          if (z - x >= 0) { 
            proc_pmf[z] += pmfs[sum(max_delay[s - 1]) + x] * 
                           conv_pmf[z - x + 1];
          }
        }
      }
      conv_pmf = proc_pmf;
    }
  }
  if (reverse) {
    conv_pmf = reverse_mf(conv_pmf, conv_delay);
  }
  return(conv_pmf);
}

vector calculate_pmfs(vector delay_mean, vector delay_sd, int max_delay) {
  int delays = num_elements(delay_mean);
  vector[sum(max_delay)] pmf;
  int pos = 1;
  for (s in 1:delays) {
    int delay_indexes[max_delay[s]];
    for (i in 1:max_delay[s]) {
      delay_indexes[i] = i - 1;
    }
    pmf[pos:(pos + max_delay[s] - 1)] =
        discretised_lognormal_pmf(delay_indexes, delay_mean[s],
                                   delay_sd[s], max_delay[s]);
    pos = pos + max_delay[s];
  }
  pmf = pmf + rep_vector(1e-8, sum(max_delay));
  return(pmf);
}

// convolve count data by a pmf if required and otherwise trim
// count data as requested
vector convolve_counts(vector infections, vector pmfs,
                          int delays, int max_delay,
                          int seeding_time) {
  int t = num_elements(infections);
  vector[t - seeding_time] reports;
  vector[t] unobs_reports = infections;
  if (delays) {
    unobs_reports = convolve(unobs_reports, pmfs, max_delay);
    reports = unobs_reports[(seeding_time + 1):t];
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
