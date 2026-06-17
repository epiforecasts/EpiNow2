/**
 * Reproduction Number (Rt) Functions
 *
 * This group of functions handles the calculation, updating, and conversion of
 * reproduction numbers in the model. The reproduction number represents the average
 * number of secondary infections caused by a single infected individual.
 *
 * @ingroup rt_estimation
 */

/**
 * Calculate the log-probability of the reproduction number (Rt) priors
 *
 * This function adds the log density contributions from priors on initial infections
 * and breakpoint effects to the target.
 *
 * @param initial_infections_scale Array of initial infection values
 * @param bp_effects Vector of breakpoint effects
 * @param bp_sd Array of breakpoint standard deviations
 * @param bp_n Number of breakpoints
 * @param cases Array of observed case counts
 * @param initial_infections_guess Initial guess for infections based on cases
 *
 * @ingroup rt_estimation
 */
void rt_lp(array[] real initial_infections_scale, vector bp_effects,
           array[] real bp_sd, int bp_n, array[] int cases,
           real initial_infections_guess) {
  //breakpoint effects on Rt
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
  initial_infections_scale ~ normal(initial_infections_guess, 2);
}

/**
 * Helper function for calculating r from R using Newton's method
 *
 * This function performs a single Newton step in the iterative calculation
 * of the growth rate r from the reproduction number R.
 *
 * Code is based on Julia code from
 * https://github.com/CDCgov/Rt-without-renewal/blob/d6344cc6e451e3e6c4188e4984247f890ae60795/EpiAware/test/predictive_checking/fast_approx_for_r.jl
 * under Apache license 2.0.
 *
 * @param R Reproduction number
 * @param r Current estimate of the growth rate
 * @param pmf Generation time probability mass function (first index: 0)
 * @return The Newton step for updating r
 *
 * @ingroup rt_estimation
 */
real R_to_r_newton_step(real R, real r, vector pmf) {
  int len = num_elements(pmf);
  vector[len] zero_series = linspaced_vector(len, 0, len - 1);
  vector[len] exp_r = exp(-r * zero_series);
  real ret = (R * dot_product(pmf, exp_r) - 1) /
    (- R * dot_product(pmf .* zero_series, exp_r));
  return(ret);
}

/**
 * Estimate the growth rate r from reproduction number R
 *
 * This function uses the Newton method to solve for the growth rate r
 * that corresponds to a given reproduction number R, using the generation
 * time distribution.
 *
 * Code is based on Julia code from
 * https://github.com/CDCgov/Rt-without-renewal/blob/d6344cc6e451e3e6c4188e4984247f890ae60795/EpiAware/test/predictive_checking/fast_approx_for_r.jl
 * under Apache license 2.0.
 *
 * @param R Reproduction number
 * @param gt_rev_pmf Reversed probability mass function of the generation time
 * @param abs_tol Absolute tolerance for the Newton solver
 * @return The estimated growth rate r
 *
 * @ingroup rt_estimation
 */
real R_to_r(real R, vector gt_rev_pmf, real abs_tol) {
  int gt_len = num_elements(gt_rev_pmf);
  vector[gt_len] gt_pmf = reverse(gt_rev_pmf);
  real mean_gt = dot_product(gt_pmf, linspaced_vector(gt_len, 0, gt_len - 1));
  real r = fmax((R - 1) / (R * mean_gt), -1);
  real step = abs_tol + 1;
  while (abs(step) > abs_tol) {
    step = R_to_r_newton_step(R, r, gt_pmf);
    r -= step;
  }

  return(r);
}
