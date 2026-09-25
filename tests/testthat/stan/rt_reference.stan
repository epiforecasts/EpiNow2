// Pure Stan reference for R_to_r(), used only by the tests.

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
 * The Newton solver in pure Stan, with the same arguments, return value and
 * cap of 100 steps as R_to_r().
 */
real R_to_r_stan(real R, vector gt_rev_pmf, real abs_tol) {
  int gt_len = num_elements(gt_rev_pmf);
  vector[gt_len] gt_pmf = reverse(gt_rev_pmf);
  real mean_gt = dot_product(gt_pmf, linspaced_vector(gt_len, 0, gt_len - 1));
  real r = fmax((R - 1) / (R * mean_gt), -1);
  real step = abs_tol + 1;
  int n = 0;
  while (n < 100 && abs(step) > abs_tol) {
    step = R_to_r_newton_step(R, r, gt_pmf);
    r -= step;
    n += 1;
  }

  return(r);
}
