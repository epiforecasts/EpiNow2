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
 * function. It is implemented in C++, with a hand-written gradient, in
 * `inst/include/epinow2/convolve_with_rev_pmf.hpp`, which gives the maths.
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
