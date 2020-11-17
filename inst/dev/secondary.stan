vector calculate_secondary(vector reports, vector obs, real frac_obs,
                           real[] delay_mean, real[] delay_sd,
                           int[] max_delay) {
  int t = num_elements(reports);
  vector[t] scaled_reports;
  vector[t] conv_reports;                            
  vector[t] secondary_reports = rep_vector(0.0, t);
  // scaling of reported cases by fraction 
  scaled_reports = scale_obs(reports, frac_obs);
  // convolve from reports to contributions from these reports
  conv_reports = convolve_to_report(scaled_reports, delay_mean, delay_sd, max_delay, 0);
  // combine reports with previous secondary data
  for (i in 1:t) {
    if (cum_reports & i > 1) {
      secondary_reports[i] = obs[i - 1];
    }
    if (conv_additive) {
      secondary_reports[i] += conv_reports[i];
    }else{
      secondary_reports[i] -= conv_reports[i];
    }
    if (current_reports) {
      if (current_additive) {
        secondary_reports[i] += scaled_reports[i];
      }else{
        secondary_reports[i] -= scaled_reports[i];
      }
    }
  }
  return(secondary_reports);
}