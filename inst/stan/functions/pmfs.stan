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
 * Discretise a continuous distribution
 *
 * This function discretizes continuous distributions (lognormal or gamma) to create
 * a probability mass function over discrete time points (days).
 * Adapted from https://github.com/epiforecasts/epinowcast
 * (MIT License, copyright: epinowcast authors)
 *
 * @param params Vector of distribution parameters ([mu, sigma] for lognormal or [shape, rate] for gamma)
 * @param n Number of days to calculate PMF for
 * @param dist Distribution type (0: lognormal, 1: gamma)
 * @return A vector of length n containing the discretized probability mass function
 *
 * @ingroup pmf_handlers
 */
vector discretised_pmf(vector params, int n, int dist) {
  vector[n] lpmf;
  vector[n] upper_lcdf;
  if (dist == 0) {
    for (i in 1:n) {
      upper_lcdf[i] = lognormal_lcdf(i | params[1], params[2]);
    }
  } else if (dist == 1) {
    for (i in 1:n) {
      upper_lcdf[i] = gamma_lcdf(i | params[1], params[2]);
    }
  } else {
    reject("Unknown distribution function provided.");
  }
  // discretise
  if (n > 1) {
    lpmf[1] = upper_lcdf[1];
    lpmf[2] = upper_lcdf[2];
    if (n > 2) {
      lpmf[3:n] = log_diff_exp(upper_lcdf[3:n], upper_lcdf[1:(n - 2)]);
    }
    // normalize
    lpmf = lpmf - log_sum_exp(upper_lcdf[(n - 1):n]);
  } else {
    lpmf[1] = 0;
  }
  return(exp(lpmf));
}
