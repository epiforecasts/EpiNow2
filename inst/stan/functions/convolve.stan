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
                          vector delay_mean, 
                          vector delay_sd,
                          vector max_delay
                          int seeding_time) {
  
  int t = num_elements(infections);
  vector[t - seeding_time] reports;
  vector[t] reports_hold;
  int delays = num_elements(delay_mean);
  
  if (delays) {
    for (s in 1:delays) {
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
