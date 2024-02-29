// Calculate secondary reports condition only on primary reports
vector calculate_secondary(vector scaled_reports, vector conv_reports, array[] int obs,
                           int cumulative, int historic, int primary_hist_additive,
                           int current, int primary_current_additive, int predict) {
  int t = num_elements(scaled_reports);
  vector[t] secondary_reports = rep_vector(0.0, t);
  // if predicting and using a cumulative target
  // combine reports with previous secondary data
  for (i in 1:t) {
    // update cumulative target
    if (cumulative && i > 1) {
      if (i > predict) {
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
        secondary_reports[i] = fmax(0, secondary_reports[i] - conv_reports[i]);
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
    secondary_reports[i] = 1e-6 + secondary_reports[i];
  }
  return(secondary_reports);
}
