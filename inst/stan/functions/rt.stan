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
 * Estimate the growth rate r from reproduction number R
 *
 * This function uses the Newton method to solve for the growth rate r
 * that corresponds to a given reproduction number R, using the generation
 * time distribution. It stops after 100 steps if the tolerance has not been
 * reached. It is implemented in C++, with a hand-written gradient, in
 * `inst/include/epinow2/R_to_r.hpp`, which gives the maths.
 *
 * @param R Reproduction number
 * @param gt_rev_pmf Reversed probability mass function of the generation time
 * @param abs_tol Absolute tolerance for the Newton solver
 * @return The estimated growth rate r
 *
 * @ingroup rt_estimation
 */
real R_to_r(real R, vector gt_rev_pmf, data real abs_tol);
