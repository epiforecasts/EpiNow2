/**
 * convolution_functions Functions
 *
 * This file contains functions for performing discrete convolutions, which are
 * used throughout the model to combine time series with delay distributions.
 *
 * @ingroup convolution_functions
 */

/**
 * Convolve a vector with a reversed probability mass function.
 *
 * This function performs a discrete convolution of two vectors, where the
 * second vector is assumed to be an already reversed probability mass
 * function. It is declared here and implemented in C++ with a hand-written
 * reverse-mode gradient, in `inst/include/epinow2/convolve_with_rev_pmf.hpp`.
 * Models that include this file must be compiled with that header (see
 * `epinow2_cmdstan_model()`).
 *
 * Write n for the length of x, D for the length of y and z for the output.
 * The weight of a delay of d days is w_d = y[D - d], d = 0, ..., D - 1, and
 * x[s] = 0 outside 1, ..., n. The output is
 *
 *   z[t] = sum_{d = 0}^{D - 1} w_d x[t - d],  t = 1, ..., len.
 *
 * It is computed one lag at a time, as one vector update per lag,
 *
 *   z[(d + 1):(d + m)] += w_d x[1:m],  m = min(n, len - d),
 *
 * which is z[(d + 1):n] += w_d x[1:(n - d)] when len = n. When len > n the
 * output runs past the end of x and m stops each update at x[n], so the
 * extra entries are the tail of the full convolution.
 *
 * The gradient is the matching correlation. With zbar the gradient of the
 * target with respect to z, each lag adds
 *
 *   xbar[1:m] += w_d zbar[(d + 1):(d + m)],
 *   wbar_d    += zbar[(d + 1):(d + m)]' x[1:m],
 *
 * and the gradient with respect to y[D - d] is wbar_d. Both passes run on
 * plain numbers, so the whole convolution is one node on the autodiff
 * stack rather than one `dot_product()` node per output time.
 *
 * @param x The input vector to be convolved.
 * @param y The already reversed probability mass function vector.
 * @param len The desired length of the output vector.
 * @return A vector of length `len` containing the convolution result.
 * @throws If `len` is longer than the full convolution (n + D - 1) or
 * shorter than `x`.
 *
 * @ingroup convolution_functions
 */
vector convolve_with_rev_pmf(vector x, vector y, int len);

/**
 * Convolve infections to reported cases.
 *
 * This function convolves a vector of infections with a reversed delay
 * distribution to produce a vector of reported cases.
 *
 * @param infections A vector of infection counts.
 * @param delay_rev_pmf A vector representing the reversed probability mass
 * function of the delay distribution.
 * @param seeding_time The number of initial time steps to exclude from the
 * output.
 * @return A vector of reported cases, starting from `seeding_time + 1`.
 *
 * @ingroup convolution_functions
 */
vector convolve_to_report(vector infections,
                          vector delay_rev_pmf,
                          int seeding_time) {
  int t = num_elements(infections);
  int delays = num_elements(delay_rev_pmf);

  if (delays == 0) {
    return infections[(seeding_time + 1):t];
  }

  vector[t] unobs_reports = convolve_with_rev_pmf(infections, delay_rev_pmf, t);
  return unobs_reports[(seeding_time + 1):t];
}
