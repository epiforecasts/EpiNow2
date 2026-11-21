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
 * This function adds the log density contribution from the prior on initial
 * infections to the target. Rt itself (including any random walk or Gaussian
 * process on it) is a state, whose deviation and hyperparameter priors are
 * handled where states are assembled (see state.stan); this function is left
 * for the one Rt-specific prior that is not part of that machinery.
 *
 * @param initial_infections_scale Array of initial infection values
 * @param cases Array of observed case counts
 * @param initial_infections_guess Initial guess for infections based on cases
 *
 * @ingroup rt_estimation
 */
void rt_lp(array[] real initial_infections_scale, array[] int cases,
           real initial_infections_guess) {
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
 * @par Example
 * For a generation time with probability `{0.1, 0.3, 0.4, 0.2}` of taking 0,
 * 1, 2 or 3 days (in practice the 0-day probability would be at or near
 * zero, as a non-zero one is rejected when the generation time is given as
 * a nonparametric PMF), the reversed PMF is
 * `gt_rev_pmf = {0.2, 0.4, 0.3, 0.1}`.
 * `R_to_r(1.5, gt_rev_pmf, 1e-6)` then returns a growth rate of
 * approximately `0.254` per day.
 *
 * @ingroup rt_estimation
 */
real R_to_r(real R, vector gt_rev_pmf, data real abs_tol);
