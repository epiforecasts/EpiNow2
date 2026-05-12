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
 * Discretise a continuous distribution using primary censoring
 *
 * Computes a properly primary-censored PMF using the vendored
 * primarycensored Stan functions. Assumes uniform primary event
 * distribution within a window of width 1.
 *
 * @param params Distribution parameters as a vector
 * @param n Number of days to calculate PMF for (max_delay + 1)
 * @param dist Distribution type using primarycensored convention
 *   (1: lognormal, 2: gamma, 3: weibull, 4: exponential)
 * @param L Left truncation point (0 for no truncation)
 * @return A vector of length n containing the discretised PMF
 *
 * @ingroup pmf_handlers
 */
vector discretised_pmf(
  vector params, data int n, int dist, data int L
) {
  int n_params = num_elements(params);
  array[n_params] real params_array;
  for (i in 1:n_params) {
    params_array[i] = params[i];
  }
  array[0] real primary_params;
  if (dist == 1 || dist == 2 || dist == 3) {
    return exp(
      primarycensored_sone_unit_uniform_lpmf_vectorized(
        n - 1, L * 1.0, n * 1.0, dist, params_array
      )
    );
  }
  return primarycensored_sone_pmf_vectorized(
    n - 1, L * 1.0, n * 1.0, dist,
    params_array, 1.0, 1, primary_params
  );
}
