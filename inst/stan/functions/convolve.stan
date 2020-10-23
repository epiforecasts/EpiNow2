// convolve a pdf and case vector 
vector convolve(vector cases, vector pdf) {
    int t = num_elements(cases);
    int max_pdf = num_elements(pdf);
    vector[t] convolved_cases = rep_vector(1e-5, t);
    for (s in 1:t) {
        convolved_cases[s] += dot_product(cases[max(1, (s - max_pdf + 1)):s], tail(pdf, min(max_pdf, s)));
    }
   return(convolved_cases);
  }


// convolve latent infections to reported (but still unobserved) cases
vector convolve_to_report(vector infections, 
                          real[] delay_mean, 
                          real[] delay_sd,
                          int[] max_delay,
                          int seeding_time) {
  
  int t = num_elements(infections);
  vector[t - seeding_time] reports;
  vector[t] reports_hold;
  int delays = num_elements(delay_mean);
  
  if (delays) {
    for (s in 1:delays) {
      
      // 
      // reverse the distributions to allow vectorised access
      vector[max_delay[s]] rev_delay = rep_vector(1e-5, max_delay[s]);
      for (j in 1:(max_delay[s])) {
        rev_delay[j] +=
        discretised_lognormal_pmf(max_delay[s] - j, delay_mean[s], delay_sd[s], max_delay[s]);
      }
      if (s == 1) {
        reports_hold = convolve(infections, rev_delay);
      }else{
        reports_hold = convolve(reports_hold, rev_delay);
      }
    }
    reports = reports_hold[(seeding_time + 1):t];
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
