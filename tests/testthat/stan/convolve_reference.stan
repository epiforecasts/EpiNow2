// Pure Stan reference for convolve_with_rev_pmf(), used only by the tests.

/**
 * Calculate convolution indices for the case where s <= xlen
 *
 * @param s Current position in the output vector
 * @param xlen Length of the x vector
 * @param ylen Length of the y vector
 * @return An array of integers: {start_x, end_x, start_y, end_y}
 */
array[] int calc_conv_indices_xlen(int s, int xlen, int ylen) {
  int s_minus_ylen = s - ylen;
  int start_x = max(1, s_minus_ylen + 1);
  int end_x = s;
  int start_y = max(1, 1 - s_minus_ylen);
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
array[] int calc_conv_indices_len(int s, int xlen, int ylen) {
  int s_minus_ylen = s - ylen;
  int start_x = max(1, s_minus_ylen + 1);
  int end_x = xlen;
  int start_y = max(1, 1 - s_minus_ylen);
  int end_y = ylen + xlen - s;
  return {start_x, end_x, start_y, end_y};
}

/**
 * Convolve a vector with a reversed probability mass function (pure Stan).
 *
 * Reference implementation of `convolve_with_rev_pmf()`, which the package
 * models call through a C++ implementation. Used by the tests to check that
 * implementation's values and gradients.
 *
 * @param x The input vector to be convolved.
 * @param y The already reversed probability mass function vector.
 * @param len The desired length of the output vector.
 * @return A vector of length `len` containing the convolution result.
 * @throws If `len` is longer than the full convolution or shorter than `x`.
 */
vector convolve_with_rev_pmf_stan(vector x, vector y, int len) {
  int xlen = num_elements(x);
  int ylen = num_elements(y);

  if (xlen + ylen - 1 < len) {
    reject("convolve_with_rev_pmf: len is longer than x and y convolved");
  }

  if (xlen > len) {
    reject("convolve_with_rev_pmf: len is shorter than x");
  }

  vector[len] z;

  for (s in 1:xlen) {
    array[4] int indices = calc_conv_indices_xlen(s, xlen, ylen);
    z[s] = dot_product(x[indices[1]:indices[2]], y[indices[3]:indices[4]]);
  }

  // runs zero times unless len > xlen
  for (s in (xlen + 1):len) {
    array[4] int indices = calc_conv_indices_len(s, xlen, ylen);
    z[s] = dot_product(x[indices[1]:indices[2]], y[indices[3]:indices[4]]);
  }

  return z;
}
