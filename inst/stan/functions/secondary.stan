/**
 * Secondary Reports Functions
 *
 * This group of functions handles the calculation and manipulation of secondary
 * reports in the model. Secondary reports are derived from primary reports through
 * various transformations and can represent different epidemiological quantities.
 *
 * @ingroup secondary_reports
 */

/**
 * @ingroup secondary_reports_estimation
 * @brief Calculate secondary reports from primary reports
 *
 * This function calculates secondary reports based on primary reports,
 * considering various options for how to combine historical and current data.
 * It supports both cumulative and non-cumulative targets, and additive or
 * subtractive relationships between primary and secondary reports.
 *
 * @param scaled_reports Vector of scaled primary reports
 * @param conv_reports Vector of convolved primary reports
 * @param obs Array of observed secondary reports
 * @param cumulative Whether to use cumulative target (1) or not (0)
 * @param historic Whether to use historical primary reports (1) or not (0)
 * @param primary_hist_additive Whether historical primary reports are additive (1) or subtractive (0)
 * @param current Whether to use current primary reports (1) or not (0)
 * @param primary_current_additive Whether current primary reports are additive (1) or subtractive (0)
 * @param predict Number of time points to predict
 * @return A vector of secondary reports
 *
 * @ingroup secondary_reports
 */
vector calculate_secondary(
  vector scaled_reports, vector conv_reports, array[] int obs,
  int cumulative, int historic, int primary_hist_additive,
  int current, int primary_current_additive, int predict
) {
  int t = num_elements(scaled_reports);
  vector[t] secondary_reports = rep_vector(0.0, t);
  // if predicting and using a cumulative target
  // combine reports with previous secondary data
  for (i in 1:t) {
    // update cumulative target
    if (cumulative && i > 1) {
      if (i > predict) {
        secondary_reports[i] = secondary_reports[i - 1];
      } else{
        secondary_reports[i] = obs[i - 1];
      }
    }
    // update based on previous primary reports
    if (historic) {
      if (primary_hist_additive) {
        secondary_reports[i] += conv_reports[i];
      } else{
        secondary_reports[i] = fmax(0, secondary_reports[i] - conv_reports[i]);
      }
    }
    // update based on current primary reports
    if (current) {
      if (primary_current_additive) {
        secondary_reports[i] += scaled_reports[i];
      } else{
        secondary_reports[i] -= scaled_reports[i];
      }
    }
    secondary_reports[i] = 1e-6 + secondary_reports[i];
  }
  return(secondary_reports);
}
