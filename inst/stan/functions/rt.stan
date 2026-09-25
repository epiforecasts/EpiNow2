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
 * @ingroup rt_estimation
 * @brief Extend a vector to length t by repeating its last value.
 *
 * @param x Vector to extend
 * @param t Target length
 * @return x followed by `t - num_elements(x)` copies of its last value, or x
 *   unchanged if it already has at least t elements
 */
vector hold_forward(vector x, int t) {
  int n = num_elements(x);
  if (n >= t) {
    return x;
  }
  return append_row(x, rep_vector(x[n], t - n));
}

/**
 * @ingroup rt_estimation
 * @brief Uncentred Gaussian process contribution to log Rt.
 *
 * A stationary GP enters log Rt directly. A non-stationary GP enters as the
 * cumulative sum of daily increments starting from 0. Either path is held at
 * its last value up to length t.
 *
 * @param noise Vector of Gaussian process noise values
 * @param t Length of the time series
 * @param stationary Whether the Gaussian process is stationary (1) or
 *   non-stationary (0)
 * @return A vector of length t, all zeros if noise is empty
 */
vector gp_log_path(vector noise, int t, int stationary) {
  if (num_elements(noise) == 0) {
    return rep_vector(0, t);
  }
  if (stationary) {
    return hold_forward(noise, t);
  }
  return hold_forward(append_row(0, cumulative_sum(noise)), t);
}

/**
 * @ingroup rt_estimation
 * @brief Uncentred log Rt level of each breakpoint segment.
 *
 * @param bp_effects Vector of breakpoint effects
 * @return A vector with one more element than bp_effects: 0 for the first
 *   level, then the cumulative sum of the effects
 */
vector bp_log_levels(vector bp_effects) {
  return append_row(0, cumulative_sum(bp_effects));
}

/**
 * @ingroup rt_estimation
 * @brief Log Rt intercept that centres the paths over the centring window.
 *
 * Subtracting the means of the uncentred paths over the first `n_centre`
 * days from `log(R0)` makes the mean of log Rt over those days equal
 * `log(R0)`. The stationary GP is not centred.
 *
 * @param R0 Initial reproduction number
 * @param gp Uncentred GP path from `gp_log_path()`
 * @param bp Uncentred breakpoint levels from `bp_log_levels()`
 * @param bps Array of breakpoint indices
 * @param stationary Whether the Gaussian process is stationary (1) or
 *   non-stationary (0)
 * @param n_centre Number of leading days in the centring window
 * @return The centred intercept on the log scale
 */
real centred_log_intercept(real R0, vector gp, vector bp, array[] int bps,
                           int stationary, int n_centre) {
  // sum() / n rather than mean() as sum() is a single autodiff node
  real c = log(R0);
  if (!stationary) {
    c -= sum(gp[1:n_centre]) / n_centre;
  }
  if (num_elements(bp) > 1) {
    c -= sum(bp[bps[1:n_centre]]) / n_centre;
  }
  return c;
}

/**
 * @ingroup rt_estimation
 * @brief Update a vector of effective reproduction numbers (Rt) based on
 * an intercept, breakpoints (i.e. a random walk), and a Gaussian
 * process.
 *
 * @param t Length of the time series
 * @param R0 Initial reproduction number
 * @param noise Vector of Gaussian process noise values
 * @param bps Array of breakpoint indices
 * @param bp_effects Vector of breakpoint effects
 * @param stationary Whether the Gaussian process is stationary (1) or non-stationary (0)
 * @param n_centre Number of leading positions over which to centre the
 *   non-stationary GP trajectory and the breakpoint random walk. Set to the
 *   observation window length so the centring is invariant to the forecast
 *   horizon. Ignored for the GP branch when `stationary` is 1; the breakpoint
 *   path is centred whenever breakpoints are present.
 * @return A vector of length t containing the updated Rt values
 */
vector update_Rt(int t, real R0, vector noise, array[] int bps,
                 vector bp_effects, int stationary, int n_centre) {
  int bp_n = num_elements(bp_effects);
  vector[t] gp = gp_log_path(noise, t, stationary);
  vector[bp_n + 1] bp = bp_log_levels(bp_effects);
  real c = centred_log_intercept(R0, gp, bp, bps, stationary, n_centre);
  if (bp_n == 0) {
    return exp(c + gp);
  }
  vector[bp_n + 1] log_R_bp = c + bp;
  if (num_elements(noise) == 0) {
    // One exp per breakpoint level rather than per day
    vector[bp_n + 1] R_bp = exp(log_R_bp);
    return R_bp[bps];
  }
  return exp(log_R_bp[bps] + gp);
}

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
