/**
 * Calculate convolution indices for the case where s <= xlen
 *
 * @param s Current position in the output vector
 * @param xlen Length of the x vector
 * @param ylen Length of the y vector
 * @return An array of integers: {start_x, end_x, start_y, end_y}
 */
int[] calc_conv_indices_xlen(int s, int xlen, int ylen) {
  int start_x = max(1, s - ylen + 1);
  int end_x = s;
  int start_y = max(1, ylen - s + 1);
  int end_y = ylen;
  return {start_x, end_x, start_y, end_y};
}

/**
 * Calculate convolution indices for the case where s > xlen
 *
 * @param s Current position in the output vector
 * @param xlen Length of the x vector
 * @param ylen Length of the y vector
 * @return An array of integers: {start_x, end_x, start_y, end_y}
 */
int[] calc_conv_indices_len(int s, int xlen, int ylen) {
  int start_x = min(1, s - ylen + 1);
  int end_x = xlen;
  int start_y = 1;
  int end_y = ylen - (s - xlen);
  return {start_x, end_x, start_y, end_y};
}

/**
 * Convolve a vector with a reversed probability mass function.
 *
 * This function performs a discrete convolution of two vectors, where the second vector
 * is assumed to be an already reversed probability mass function.
 *
 * @param x The input vector to be convolved.
 * @param y The already reversed probability mass function vector.
 * @param len The desired length of the output vector.
 * @return A vector of length `len` containing the convolution result.
 * @throws If `len` is longer than the sum of lengths of `x` and `y`.
 */
vector convolve_with_rev_pmf(vector x, vector y, int len) {
  int xlen = num_elements(x);
  int ylen = num_elements(y);
  vector[len] z = rep_vector(0, len);
  
  if (xlen + ylen - 1 < len) {
    reject("convolve_with_rev_pmf: len is longer than x and y convolved");
  }
  
  int max_s = min(len, xlen);
  
  for (s in 1:max_s) {
    int[] indices = calc_conv_indices_xlen(s, xlen, ylen);
    z[s] = dot_product(x[indices[1]:indices[2]], y[indices[3]:indices[4]]);
  }
  
  if (len > xlen) {
    for (s in (xlen + 1):len) {
      int[] indices = calc_conv_indices_len(s, xlen, ylen);
      z[s] = dot_product(x[indices[1]:indices[2]], y[indices[3]:indices[4]]);
    }
  }
  
  return z;
}

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