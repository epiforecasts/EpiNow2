/**
 * Probability Mass Function (PMF) Utilities
 *
 * This file contains functions for creating and manipulating probability mass
 * functions, particularly for discretizing continuous distributions for use in
 * delay modeling.
 *
 * @ingroup pmf_handlers
 */

/**
 * Discretise a continuous distribution with proper interval censoring
 *
 * This function discretises continuous distributions (lognormal or gamma) to create
 * a probability mass function over discrete time points (days), accounting for
 * interval censoring using primarycensored.
 *
 * @param params Vector of distribution parameters ([mu, sigma] for lognormal or [shape, rate] for gamma)
 * @param n Number of days to calculate PMF for
 * @param dist Distribution type (0: lognormal, 1: gamma)
 * @return A vector of length n containing the discretised probability mass function
 *
 * @ingroup pmf_handlers
 */
vector discretised_pmf(vector params, int n, int dist) {
  vector[n] lpmf;
  int dist_id = dist + 1; // Convert: 0->1 (lognormal), 1->2 (gamma)
  array[2] real params_array = {params[1], params[2]};
  array[0] real primary_params = {}; // No primary distribution
  real pwindow = 1.0; // Daily censoring window
  int primary_id = 0; // No primary distribution
  real D = 1e10; // Effectively no truncation

  if (dist != 0 && dist != 1) {
    reject("Unknown distribution function provided. Use 0 for lognormal or 1 for gamma.");
  }

  if (n == 1) {
    lpmf[1] = 0; // log(1) = 0, as all mass is in the single bin
  } else {
    // Compute interval-censored probabilities for each day
    for (i in 1:n) {
      real d_lower = i - 1; // Lower bound of interval
      real d_upper = i; // Upper bound of interval

      // Use primarycensored for proper interval-censored discretisation
      lpmf[i] = primarycensored_lpmf(
        d_lower | dist_id, params_array, pwindow, d_upper, D,
        primary_id, primary_params
      );
    }

    // Normalise to ensure proper PMF (accounts for truncation at n)
    lpmf = lpmf - log_sum_exp(lpmf);
  }

  return exp(lpmf);
}
