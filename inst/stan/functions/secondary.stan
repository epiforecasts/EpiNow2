// Calculate secondary reports condition only on primary reports
vector calculate_secondary(vector reports, int[] obs, real[] frac_obs,
                           real[] delay_mean, real[] delay_sd,
                           int[] max_delay, int cumulative, int historic,
                           int primary_hist_additive, int current,
                           int primary_current_additive, int predict) {
  int t = num_elements(reports);
  int obs_scale = num_elements(frac_obs);
  vector[t] scaled_reports;
  vector[t] conv_reports;                            
  vector[t] secondary_reports = rep_vector(0.0, t);
  // scaling of reported cases by fraction 
  if (obs_scale) {
    scaled_reports = scale_obs(reports, frac_obs[1]);
  }else{
    scaled_reports = reports;
  }
  // convolve from reports to contributions from these reports
  conv_reports = convolve_to_report(scaled_reports, delay_mean, delay_sd, max_delay, 0);
  // if predicting and using a cumulative target
  // combine reports with previous secondary data
  for (i in 1:t) {
    // update cumulative target
    if (cumulative && i > 1) {
      if (t > predict) {
        secondary_reports[i] = secondary_reports[i - 1];       
      }else{
        secondary_reports[i] = obs[i - 1];
      }
    }
    // update based on previous primary reports
    if (historic) {
      if (primary_hist_additive) {
      secondary_reports[i] += conv_reports[i];
      }else{
      secondary_reports[i] -= conv_reports[i];
      }
    }
    // update based on current primary reports
    if (current) {
      if (primary_current_additive) {
        secondary_reports[i] += scaled_reports[i];
      }else{
        secondary_reports[i] -= scaled_reports[i];
      }
    }
  }
  return(secondary_reports);
}
