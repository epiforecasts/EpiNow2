/**
  * Compute the log CDF of the delay distribution
  * @ingroup delay_log_cdfs
  *
  * @param delay Time delay
  * @param params Distribution parameters
  * @param dist_id Distribution identifier
  *   1: Lognormal, 2: Gamma, 3: Normal, 4: Exponential, 5: Weibull,
  *   6: Beta, 7: Cauchy, 8: Chi-square, 9: Inverse Chi-square,
  *   10: Double Exponential, 11: Inverse Gamma, 12: Logistic,
  *   13: Pareto, 14: Scaled Inverse Chi-square, 15: Student's t,
  *   16: Uniform, 17: von Mises
  *
  * @return Log CDF of the delay distribution
  *
  * @code
  * // Example: Lognormal distribution
  * real delay = 5.0;
  * array[2] real params = {0.0, 1.0}; // mean and standard deviation on log scale
  * int dist_id = 1; // Lognormal
  * real log_cdf = dist_lcdf(delay, params, dist_id);
  * @endcode
  */
real dist_lcdf(real delay, array[] real params, int dist_id) {
  if (delay <= 0) return negative_infinity();

  // Use if-else statements to handle different distribution types
  if (dist_id == 1) return lognormal_lcdf(delay | params[1], params[2]);
  else if (dist_id == 2) return gamma_lcdf(delay | params[1], params[2]);
  else if (dist_id == 3) return normal_lcdf(delay | params[1], params[2]);
  else if (dist_id == 4) return exponential_lcdf(delay | params[1]);
  else if (dist_id == 5) return weibull_lcdf(delay | params[1], params[2]);
  else if (dist_id == 6) return beta_lcdf(delay | params[1], params[2]);
  else if (dist_id == 7) return cauchy_lcdf(delay | params[1], params[2]);
  else if (dist_id == 8) return chi_square_lcdf(delay | params[1]);
  else if (dist_id == 9) return inv_chi_square_lcdf(delay | params[1]);
  else if (dist_id == 10) return double_exponential_lcdf(delay | params[1], params[2]);
  else if (dist_id == 11) return inv_gamma_lcdf(delay | params[1], params[2]);
  else if (dist_id == 12) return logistic_lcdf(delay | params[1], params[2]);
  else if (dist_id == 13) return pareto_lcdf(delay | params[1], params[2]);
  else if (dist_id == 14) return scaled_inv_chi_square_lcdf(delay | params[1], params[2]);
  else if (dist_id == 15) return student_t_lcdf(delay | params[1], params[2], params[3]);
  else if (dist_id == 16) return uniform_lcdf(delay | params[1], params[2]);
  else if (dist_id == 17) return von_mises_lcdf(delay | params[1], params[2]);
  else reject("Invalid distribution identifier");
}

/**
  * Compute the log PDF of the primary distribution
  * @ingroup primary_distribution_log_pdfs
  *
  * @param x Value
  * @param primary_id Primary distribution identifier
  * @param params Distribution parameters
  * @param min Minimum value
  * @param max Maximum value
  *
  * @return Log PDF of the primary distribution
  *
  * @code
  * // Example: Uniform distribution
  * real x = 0.5;
  * int primary_id = 1; // Uniform
  * array[0] real params = {}; // No additional parameters for uniform
  * real min = 0;
  * real max = 1;
  * real log_pdf = primary_lpdf(x, primary_id, params, min, max);
  * @endcode
  */
real primary_lpdf(real x, int primary_id, array[] real params, real min, real max) {
  // Implement switch for different primary distributions
  if (primary_id == 1) return uniform_lpdf(x | min, max);
  if (primary_id == 2) return expgrowth_lpdf(x | min, max, params[1]);
  // Add more primary distributions as needed
  reject("Invalid primary distribution identifier");
}

/**
  * ODE system for the primary censored distribution
  * @ingroup ode
  *
  * @param t Time
  * @param y State variables
  * @param theta Parameters
  * @param x_r Real data
  * @param x_i Integer data
  *
  * @return Derivatives of the state variables
  */
vector primarycensored_ode(real t, vector y, array[] real theta,
                            array[] real x_r, array[] int x_i) {
  real d = x_r[1];
  int dist_id = x_i[1];
  int primary_id = x_i[2];
  real pwindow = x_r[2];
  int dist_params_len = x_i[3];
  int primary_params_len = x_i[4];

  // Extract distribution parameters
  array[dist_params_len] real params;
  if (dist_params_len) {
    params = theta[1:dist_params_len];
  }
  array[primary_params_len] real primary_params;
  if (primary_params_len) {
    int primary_loc = num_elements(theta);
    primary_params = theta[primary_loc - primary_params_len + 1:primary_loc];
  }

  real log_cdf = dist_lcdf(t | params, dist_id);
  real log_primary_pdf = primary_lpdf(d - t | primary_id, primary_params, 0, pwindow);

  return rep_vector(exp(log_cdf + log_primary_pdf), 1);
}
