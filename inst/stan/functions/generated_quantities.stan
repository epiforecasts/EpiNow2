/**
 * Generated Quantities Functions
 *
 * Functions for calculating additional quantities from model outputs.
 */

/**
 * Calculate Rt directly from inferred infections
 *
 * This function estimates the reproduction number (Rt) using the Cori et al. approach,
 * directly from a time series of infections. Optionally applies smoothing.
 *
 * @param infections Vector of infection counts
 * @param seeding_time Number of time steps used for seeding
 * @param gt_rev_pmf Vector of reversed generation time PMF
 * @param smooth Number of time steps to use for smoothing (0 for no smoothing)
 * @return A vector of reproduction numbers (Rt)
 *
 * @ingroup rt_estimation
 */
vector calculate_Rt(vector infections, int seeding_time,
                    vector gt_rev_pmf, int smooth) {
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[ot] R;
  vector[ot] sR;
  vector[ot] infectiousness = rep_vector(1e-5, ot);
  // calculate Rt using Cori et al. approach
  for (s in 1:ot) {
    infectiousness[s] += update_infectiousness(
      infections, gt_rev_pmf, seeding_time, s
    );
    R[s] = infections[s + seeding_time] / infectiousness[s];
  }
  if (smooth) {
    for (s in 1:ot) {
      real window = 0;
      sR[s] = 0;
      for (i in max(1, s - smooth):min(ot, s + smooth)) {
        sR[s] += R[i];
        window += 1;
      }
      sR[s] = sR[s] / window;
    }
  } else{
    sR = R;
  }
  return(sR);
}

/**
 * Calculate growth rate
 *
 * This function calculates the growth rate from a time series of infections
 * by taking the log difference between consecutive time points.
 *
 * @param infections Vector of infection counts
 * @param seeding_time Number of time steps used for seeding
 * @return A vector of growth rates
 *
 * @ingroup rt_estimation
 */
vector calculate_growth_infections(vector infections, int seeding_time) {
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[t] log_inf = log(infections);
  vector[ot] growth =
    log_inf[(seeding_time + 1):t] - log_inf[seeding_time:(t - 1)];
  return(growth);
}

/**
 * Calculate growth rate using approach by Parag et al. 2022 
 * (https://doi.org/10.1111/rssa.12867)
 *
 * This function calculates the growth rate from a time series of infections
 * by taking the log derivative on the infectiousness and shifting it by the 
 * mean generation time.
 *
 * @param infections Vector of infection counts
 * @param seeding_time Number of time steps used for seeding
 * @param gt_rev_pmf Vector of reversed generation time PMF
 *  
 * @return A vector of growth rates
 *
 * @ingroup rt_estimation
 */
vector calculate_growth_infectiousness(vector infections, int seeding_time, 
                                       vector gt_rev_pmf) {
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[ot] infness_log = rep_vector(1e-5, ot);
  for (s in 1:ot) {
    infness_log[s] += log(update_infectiousness(
      infections, gt_rev_pmf, seeding_time, s
    ));
  }
  vector[ot] growth = rep_vector(1e-5, ot);
  int gt_length = num_elements(gt_rev_pmf);
  // mean generation time, will always be >= 1
  int mean_gen = to_next_int_index( // round weighted mean to next int
    dot_product(reverse(linspaced_vector(gt_length, 1, gt_length)), gt_rev_pmf)
    );
  growth[1:(ot-mean_gen)] = (
    infness_log[(1+mean_gen):ot] - infness_log[mean_gen:(ot - 1)]
    );
  // most recent growth rates remain undefined due to shift:
  growth[(ot-mean_gen+1):ot] = rep_vector(not_a_number(), mean_gen);
  return(growth);
}
