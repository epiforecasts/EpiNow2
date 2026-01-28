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
 * @param gt_rev_pmf Vector of reversed generation time PMF
 * @param growth_method Either 0 (log derivative of new infections) or 1 (log 
 * derivative of infectiousness, see Parag et al. 2022)
 *
 * @return A vector of growth rates
 *
 * @ingroup rt_estimation
 */
vector calculate_growth(vector infections, int seeding_time, 
                        data vector gt_rev_pmf, int growth_method) {
  if (growth_method == 0) {
    return(calculate_growth_infections(infections, seeding_time));
  } else if (growth_method == 1) {
    return(calculate_growth_infness(infections, seeding_time, gt_rev_pmf));
  } else {
    reject("growth_method must be 0 (infections) or 1 (infectiousness).");
  }
}

/**
 * Calculate growth rate
 *
 * This function calculates the growth rate from a time series of infections
 * by taking the log difference between consecutive time points.
 *
 * @param infections Vector of infection counts
 * @param seeding_time Number of time steps used for seeding
 *
 * @return A vector of growth rates
 *
 * @ingroup rt_estimation
 */
vector calculate_growth_infections(vector infections, int seeding_time) {
  int t = num_elements(infections);
  int ot = t - seeding_time;
  int start = 1 + seeding_time;
  vector[t] log_inf = log(infections);
  vector[ot - 1] growth = log_inf[(1+start):t] - log_inf[start:(t - 1)];
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
vector calculate_growth_infness(vector infections, int seeding_time, 
                                       data vector gt_rev_pmf) {
  int t = num_elements(infections);
  int ot = t - seeding_time;
  int start = 1 + seeding_time;
  if (ot <= 1) {
    reject("seeding_time must >1 time step shorter than infections vector.");
    }
  // infectiousness
  vector[ot] infness_log = rep_vector(1e-5, ot);
  for (s in 1:ot) {
    infness_log[s] += log(update_infectiousness(
      infections, gt_rev_pmf, seeding_time, s
    ));
  }
  // mean generation time, will always be >= 1
  int gt_length = num_elements(gt_rev_pmf);
  int mean_gen = to_int(round( // round weighted mean to next int
    dot_product(reverse(linspaced_vector(gt_length, 1, gt_length)), gt_rev_pmf)
    ));
  // growth rate
  vector[ot - 1] growth = infness_log[2:ot] - infness_log[1:(ot - 1)];  
  // shift by mean_gen (most recent growth rates remain undefined)
  growth[1:(ot - 1 - mean_gen)] = growth[(1 + mean_gen):(ot - 1)];
  growth[(ot - mean_gen):(ot - 1)] = rep_vector(not_a_number(), mean_gen);
  return(growth);
}
