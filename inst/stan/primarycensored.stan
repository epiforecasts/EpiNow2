// Stan functions from primarycensored version 1.3.0
/**
  * Exponential growth probability density function (PDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the PDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The PDF evaluated at x
  */
real expgrowth_pdf(real x, real min, real max, real r) {
  if (x < min || x > max) {
    return 0;
  }
  if (abs(r) < 1e-10) {
    return 1 / (max - min);
  }
  return r * exp(r * (x - min)) / (exp(r * max) - exp(r * min));
}

/**
  * Exponential growth log probability density function (log PDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the log PDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The log PDF evaluated at x
  */
real expgrowth_lpdf(real x, real min, real max, real r) {
  if (x < min || x > max) {
    return negative_infinity();
  }
  if (abs(r) < 1e-10) {
    return -log(max - min);
  }
  return log(r) + r * (x - min) - log(exp(r * max) - exp(r * min));
}

/**
  * Exponential growth cumulative distribution function (CDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the CDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The CDF evaluated at x
  */
real expgrowth_cdf(real x, real min, real max, real r) {
  if (x < min) {
    return 0;
  }
  if (x > max) {
    return 1;
  }
  if (abs(r) < 1e-10) {
    return (x - min) / (max - min);
  }
  return (exp(r * (x - min)) - exp(r * min)) / (exp(r * max) - exp(r * min));
}

/**
  * Exponential growth log cumulative distribution function (log CDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the log CDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The log CDF evaluated at x
  */
real expgrowth_lcdf(real x, real min, real max, real r) {
  if (x < min) {
    return negative_infinity();
  }
  if (x > max) {
    return 0;
  }
  return log(expgrowth_cdf(x | min, max, r));
}

/**
  * Exponential growth random number generator
  * @ingroup exponential_growth_distributions
  *
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return A random draw from the exponential growth distribution
  */
real expgrowth_rng(real min, real max, real r) {
  real u = uniform_rng(0, 1);
  if (abs(r) < 1e-10) {
    return min + u * (max - min);
  }
  return min + log(u * (exp(r * max) - exp(r * min)) + exp(r * min)) / r;
}
/**
  * Check if an analytical solution exists for the given distribution combination
  * @ingroup analytical_solution_helpers
  *
  * @param dist_id Distribution identifier for the delay distribution
  * @param primary_id Distribution identifier for the primary distribution
  *
  * @return 1 if an analytical solution exists, 0 otherwise
  */
int check_for_analytical(int dist_id, int primary_id) {
  if (dist_id == 2 && primary_id == 1) return 1; // Gamma delay with Uniform primary
  if (dist_id == 1 && primary_id == 1) return 1; // Lognormal delay with Uniform primary
  if (dist_id == 3 && primary_id == 1) return 1; // Weibull delay with Uniform primary
  return 0; // No analytical solution for other combinations
}

/**
  * Compute the primary event censored log CDF analytically for Gamma delay with Uniform primary
  * @ingroup primary_event_analytical_distributions
  *
  * @param d Delay time
  * @param q Lower bound of integration (max(d - pwindow, 0))
  * @param params Array of Gamma distribution parameters [shape, rate]
  * @param pwindow Primary event window
  *
  * @return Log of the primary event censored CDF for Gamma delay with Uniform
  * primary
  */
real primarycensored_gamma_uniform_lcdf(data real d, real q, array[] real params, data real pwindow) {
  real shape = params[1];
  real rate = params[2];
  real shape_1 = shape + 1;
  real log_window = log(pwindow);

  real log_F_T = gamma_lcdf(d | shape, rate);
  real log_F_T_kp1 = gamma_lcdf(d | shape_1, rate);

  real log_delta_F_T_kp1;
  real log_delta_F_T_k;
  real log_F_Splus;

  if (q != 0) {
    real log_F_T_q = gamma_lcdf(q | shape, rate);
    real log_F_T_q_kp1 = gamma_lcdf(q | shape_1, rate);

    // Ensure that the first argument is greater than the second
    log_delta_F_T_kp1 = log_diff_exp(log_F_T_kp1, log_F_T_q_kp1);
    log_delta_F_T_k = log_diff_exp(log_F_T, log_F_T_q);

    log_F_Splus = log_diff_exp(
      log_F_T,
      log_diff_exp(
        log(shape * inv(rate)) + log_delta_F_T_kp1,
        log(d - pwindow) + log_delta_F_T_k
      ) - log_window
    );
  } else {
    log_delta_F_T_kp1 = log_F_T_kp1;
    log_delta_F_T_k = log_F_T;

    log_F_Splus = log_diff_exp(
      log_F_T,
      log_sum_exp(
        log(shape * inv(rate)) + log_delta_F_T_kp1,
        log(pwindow - d) + log_delta_F_T_k
      ) - log_window
    );
  }

  return log_F_Splus;
}

/**
  * Compute the primary event censored log CDF analytically for Lognormal delay with Uniform primary
  * @ingroup primary_event_analytical_distributions
  *
  * @param d Delay time
  * @param q Lower bound of integration (max(d - pwindow, 0))
  * @param params Array of Lognormal distribution parameters [mu, sigma]
  * @param pwindow Primary event window
  *
  * @return Log of the primary event censored CDF for Lognormal delay with
  * Uniform primary
  */
real primarycensored_lognormal_uniform_lcdf(data real d, real q, array[] real params, data real pwindow) {
  real mu = params[1];
  real sigma = params[2];
  real mu_sigma2 = mu + square(sigma);
  real log_window = log(pwindow);

  real log_F_T = lognormal_lcdf(d | mu, sigma);
  real log_F_T_mu_sigma2 = lognormal_lcdf(d | mu_sigma2, sigma);

  real log_delta_F_T_mu_sigma;
  real log_delta_F_T;
  real log_F_Splus;

  if (q != 0) {
    real log_F_T_q = lognormal_lcdf(q | mu, sigma);
    real log_F_T_q_mu_sigma2 = lognormal_lcdf(q | mu_sigma2, sigma);

    // Ensure that the first argument is greater than the second
    log_delta_F_T_mu_sigma = log_diff_exp(
      log_F_T_mu_sigma2, log_F_T_q_mu_sigma2
    );
    log_delta_F_T = log_diff_exp(log_F_T, log_F_T_q);

    log_F_Splus = log_diff_exp(
      log_F_T,
      log_diff_exp(
        (mu + 0.5 * square(sigma)) + log_delta_F_T_mu_sigma,
        log(d - pwindow) + log_delta_F_T
      ) - log_window
    );
  } else {
    log_delta_F_T_mu_sigma = log_F_T_mu_sigma2;
    log_delta_F_T = log_F_T;

    log_F_Splus = log_diff_exp(
      log_F_T,
      log_sum_exp(
        (mu + 0.5 * square(sigma)) + log_delta_F_T_mu_sigma,
        log(pwindow - d) + log_delta_F_T
      ) - log_window
    );
  }

  return log_F_Splus;
}

/**
  * Compute the log of the lower incomplete gamma function
  * @ingroup analytical_solution_helpers
  *
  * This function is used in the analytical solution for the primary censored
  * Weibull distribution with uniform primary censoring. It corresponds to the
  * g(t; λ, k) function described in the analytic solutions document.
  *
  * @param t Upper bound of integration
  * @param shape Shape parameter (k) of the Weibull distribution
  * @param scale Scale parameter (λ) of the Weibull distribution
  *
  * @return Log of g(t; λ, k) = γ(1 + 1/k, (t/λ)^k)
  */
real log_weibull_g(real t, real shape, real scale) {
  real x = pow(t * inv(scale), shape);
  real a = 1 + inv(shape);
  return log(gamma_p(a, x)) + lgamma(a);
}

/**
  * Compute the primary event censored log CDF analytically for Weibull delay with Uniform primary
  * @ingroup primary_event_analytical_distributions
  *
  * @param d Delay time
  * @param q Lower bound of integration (max(d - pwindow, 0))
  * @param params Array of Weibull distribution parameters [shape, scale]
  * @param pwindow Primary event window
  *
  * @return Log of the primary event censored CDF for Weibull delay with
  * Uniform primary
  */
real primarycensored_weibull_uniform_lcdf(data real d, real q, array[] real params, data real pwindow) {
  real shape = params[1];
  real scale = params[2];
  real log_window = log(pwindow);

  real log_F_T = weibull_lcdf(d | shape, scale);

  real log_delta_g;
  real log_delta_F_T;
  real log_F_Splus;

  if (q != 0) {
    real log_F_T_q = weibull_lcdf(q | shape, scale);

    log_delta_g = log_diff_exp(
      log_weibull_g(d, shape, scale),
      log_weibull_g(q, shape, scale)
    );
    log_delta_F_T = log_diff_exp(log_F_T, log_F_T_q);

    log_F_Splus = log_diff_exp(
      log_F_T,
      log_diff_exp(
        log(scale) + log_delta_g,
        log(d - pwindow) + log_delta_F_T
      ) - log_window
    );
  } else {
    log_delta_g = log_weibull_g(d, shape, scale);
    log_delta_F_T = log_F_T;

    log_F_Splus = log_diff_exp(
      log_F_T,
      log_sum_exp(
        log(scale) + log_delta_g,
        log(pwindow - d) + log_delta_F_T
      ) - log_window
    );
  }

  return log_F_Splus;
}

/**
  * Compute the primary event censored log CDF analytically for a single delay
  * @ingroup primary_event_analytical_distributions
  *
  * @param d Delay
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param D Maximum delay (truncation point)
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Primary event censored log CDF, normalized by D if finite (truncation adjustment)
  */
real primarycensored_analytical_lcdf(data real d, int dist_id,
                                           array[] real params,
                                           data real pwindow, data real D,
                                           int primary_id,
                                           array[] real primary_params) {
  real result;
  real log_cdf_D;

  if (d <= 0) return negative_infinity();
  if (d >= D) return 0;

  real q = max({d - pwindow, 0});

  if (dist_id == 2 && primary_id == 1) {
    // Gamma delay with Uniform primary
    result = primarycensored_gamma_uniform_lcdf(d | q, params, pwindow);
  } else if (dist_id == 1 && primary_id == 1) {
    // Lognormal delay with Uniform primary
    result = primarycensored_lognormal_uniform_lcdf(d | q, params, pwindow);
  } else if (dist_id == 3 && primary_id == 1) {
    // Weibull delay with Uniform primary
    result = primarycensored_weibull_uniform_lcdf(d | q, params, pwindow);
  } else {
    // No analytical solution available
    return negative_infinity();
  }

  if (!is_inf(D)) {
    log_cdf_D = primarycensored_lcdf(
      D | dist_id, params, pwindow, positive_infinity(),
      primary_id, primary_params
    );
    result = result - log_cdf_D;
  }

  return result;
}

/**
  * Compute the primary event censored CDF analytically for a single delay
  * @ingroup primary_event_analytical_distributions
  *
  * @param d Delay
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param D Maximum delay (truncation point)
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Primary event censored CDF, normalized by D if finite (truncation adjustment)
  */
real primarycensored_analytical_cdf(data real d, int dist_id,
                                          array[] real params,
                                          data real pwindow, data real D,
                                          int primary_id,
                                          array[] real primary_params) {
  return exp(primarycensored_analytical_lcdf(d | dist_id, params, pwindow, D, primary_id, primary_params));
}
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
/**
  * Primary event censored distribution functions
  */

/**
  * Compute the primary event censored CDF for a single delay
  * @ingroup primary_censored_single
  *
  * @param d Delay
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param D Maximum delay (truncation point)
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Primary event censored CDF, normalized by D if finite (truncation adjustment)
  */
real primarycensored_cdf(data real d, int dist_id, array[] real params,
                               data real pwindow, data real D,
                               int primary_id,
                               array[] real primary_params) {
  real result;
  if (d <= 0) {
    return 0;
  }

  if (d >= D) {
    return 1;
  }

  // Check if an analytical solution exists
  if (check_for_analytical(dist_id, primary_id)) {
    // Use analytical solution
    result = primarycensored_analytical_cdf(
      d | dist_id, params, pwindow, D, primary_id, primary_params
    );
  } else {
    // Use numerical integration for other cases
    real lower_bound = max({d - pwindow, 1e-6});
    int n_params = num_elements(params);
    int n_primary_params = num_elements(primary_params);
    array[n_params + n_primary_params] real theta = append_array(params, primary_params);
    array[4] int ids = {dist_id, primary_id, n_params, n_primary_params};

    vector[1] y0 = rep_vector(0.0, 1);
    result = ode_rk45(primarycensored_ode, y0, lower_bound, {d}, theta, {d, pwindow}, ids)[1, 1];

    if (!is_inf(D)) {
      real log_cdf_D = primarycensored_lcdf(
        D | dist_id, params, pwindow, positive_infinity(), primary_id,primary_params
      );
      result = exp(log(result) - log_cdf_D);
    }
  }

  return result;
}

/**
  * Compute the primary event censored log CDF for a single delay
  * @ingroup primary_censored_single
  *
  * @param d Delay
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param D Maximum delay (truncation point)
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Primary event censored log CDF, normalized by D if finite (truncation adjustment)
  *
  * @code
  * // Example: Weibull delay distribution with uniform primary distribution
  * real d = 3.0;
  * int dist_id = 5; // Weibull
  * array[2] real params = {2.0, 1.5}; // shape and scale
  * real pwindow = 1.0;
  * real D = positive_infinity();
  * int primary_id = 1; // Uniform
  * array[0] real primary_params = rep_array(0.0, 0);
  * real log_cdf = primarycensored_lcdf(
  *   d, dist_id, params, pwindow, D, primary_id, primary_params
  * );
  * @endcode
  */
real primarycensored_lcdf(data real d, int dist_id, array[] real params,
                                data real pwindow, data real D,
                                int primary_id,
                                array[] real primary_params) {
  real result;

  if (d <= 0) {
    return negative_infinity();
  }

  if (d >= D) {
    return 0;
  }

  // Check if an analytical solution exists
  if (check_for_analytical(dist_id, primary_id)) {
    result = primarycensored_analytical_lcdf(
      d | dist_id, params, pwindow, positive_infinity(), primary_id, primary_params
    );
  } else {
    // Use numerical integration
    result = log(primarycensored_cdf(
      d | dist_id, params, pwindow, positive_infinity(), primary_id, primary_params
    ));
  }

  // Handle truncation
  if (!is_inf(D)) {
    real log_cdf_D = primarycensored_lcdf(
      D | dist_id, params, pwindow, positive_infinity(), primary_id, primary_params
    );
    result = result - log_cdf_D;
  }

  return result;
}

/**
  * Compute the primary event censored log PMF for a single delay
  * @ingroup primary_censored_single
  *
  * @param d Delay (integer)
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param d_upper Upper bound for the delay interval
  * @param D Maximum delay (truncation point)
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Primary event censored log PMF, normalized by D if finite (truncation adjustment)
  *
  * @code
  * // Example: Weibull delay distribution with uniform primary distribution
  * int d = 3;
  * int dist_id = 5; // Weibull
  * array[2] real params = {2.0, 1.5}; // shape and scale
  * real pwindow = 1.0;
  * real d_upper = 4.0;
  * real D = positive_infinity();
  * int primary_id = 1; // Uniform
  * array[0] real primary_params = {};
  * real log_pmf = primarycensored_lpmf(
  *   d, dist_id, params, pwindow, d_upper, D, primary_id, primary_params
  * );
  * @endcode
  */
real primarycensored_lpmf(data int d, int dist_id, array[] real params,
                                data real pwindow, data real d_upper,
                                data real D, int primary_id,
                                array[] real primary_params) {
  if (d_upper > D) {
    reject("Upper truncation point is greater than D. It is ", d_upper,
           " and D is ", D, ". Resolve this by increasing D to be greater or equal to d + swindow or decreasing swindow.");
  }
  if (d_upper <= d) {
    reject("Upper truncation point is less than or equal to d. It is ", d_upper,
           " and d is ", d, ". Resolve this by increasing d to be less than d_upper.");
  }
  real log_cdf_upper = primarycensored_lcdf(
    d_upper | dist_id, params, pwindow, positive_infinity(), primary_id, primary_params
  );
  real log_cdf_lower = primarycensored_lcdf(
    d | dist_id, params, pwindow, positive_infinity(), primary_id, primary_params
  );
  if (!is_inf(D)) {
    real log_cdf_D;

    if (d_upper == D) {
      log_cdf_D = log_cdf_upper;
    } else {
      log_cdf_D = primarycensored_lcdf(
        D | dist_id, params, pwindow, positive_infinity(), primary_id, primary_params
      );
    }
    return log_diff_exp(log_cdf_upper, log_cdf_lower) - log_cdf_D;
  } else {
    return log_diff_exp(log_cdf_upper, log_cdf_lower);
  }
}

/**
  * Compute the primary event censored PMF for a single delay
  * @ingroup primary_censored_single
  *
  * @param d Delay (integer)
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param d_upper Upper bound for the delay interval
  * @param D Maximum delay (truncation point)
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Primary event censored PMF, normalized by D if finite (truncation adjustment)
  *
  * @code
  * // Example: Weibull delay distribution with uniform primary distribution
  * int d = 3;
  * real d = 3.0;
  * int dist_id = 5; // Weibull
  * array[2] real params = {2.0, 1.5}; // shape and scale
  * real pwindow = 1.0;
  * real swindow = 0.1;
  * real D = positive_infinity();
  * int primary_id = 1; // Uniform
  * array[0] real primary_params = {};
  * real pmf = primarycensored_pmf(d, dist_id, params, pwindow, swindow, D, primary_id, primary_params);
  * @endcode
  */
real primarycensored_pmf(data int d, int dist_id, array[] real params,
                               data real pwindow, data real d_upper,
                               data real D, int primary_id,
                               array[] real primary_params) {
  return exp(
    primarycensored_lpmf(
      d | dist_id, params, pwindow, d_upper, D, primary_id, primary_params
    )
  );
}

/**
  * Compute the primary event censored log PMF for integer delays up to max_delay
  * @ingroup primary_censored_vectorized
  *
  * @param max_delay Maximum delay to compute PMF for
  * @param D Maximum delay (truncation point), must be at least max_delay + 1
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Vector of primary event censored log PMFs for delays \[0, 1\] to
  * \[max_delay, max_delay + 1\].
  *
  * This function differs from primarycensored_lpmf in that it:
  * 1. Computes PMFs for all integer delays from \[0, 1\] to \[max_delay,
  *    max_delay + 1\] in one call.
  * 2. Assumes integer delays (swindow = 1)
  * 3. Is more computationally efficient for multiple delay calculation as it
  *    reduces the number of integration calls.
  *
  * @code
  * // Example: Weibull delay distribution with uniform primary distribution
  * int max_delay = 10;
  * real D = 15.0;
  * int dist_id = 5; // Weibull
  * array[2] real params = {2.0, 1.5}; // shape and scale
  * real pwindow = 7.0;
  * int primary_id = 1; // Uniform
  * array[0] real primary_params = {};

  * vector[max_delay] log_pmf =
  *   primarycensored_sone_lpmf_vectorized(
  *      max_delay, D, dist_id, params, pwindow, primary_id,
  *      primary_params
  *   );
  * @endcode
  */
vector primarycensored_sone_lpmf_vectorized(
  int max_delay, data real D, int dist_id,
  array[] real params, data real pwindow,
  int primary_id, array[] real primary_params
) {

  int upper_interval = max_delay + 1;
  vector[upper_interval] log_pmfs;
  vector[upper_interval] log_cdfs;
  real log_normalizer;

  // Check if D is at least max_delay + 1
  if (D < upper_interval) {
    reject("D must be at least max_delay + 1");
  }

  // Compute log CDFs
  for (d in 1:upper_interval) {
    log_cdfs[d] = primarycensored_lcdf(
      d | dist_id, params, pwindow, positive_infinity(), primary_id,
      primary_params
    );
  }

  // Compute log normalizer using upper_interval
  if (D > upper_interval) {
    if (is_inf(D)) {
      log_normalizer = 0; // No normalization needed for infinite D
    } else {
      log_normalizer = primarycensored_lcdf(
        D | dist_id, params, pwindow, positive_infinity(),
        primary_id, primary_params
      );
    }
  } else {
    log_normalizer = log_cdfs[upper_interval];
  }

  // Compute log PMFs
  log_pmfs[1] = log_cdfs[1] - log_normalizer;
  for (d in 2:upper_interval) {
    log_pmfs[d] = log_diff_exp(log_cdfs[d], log_cdfs[d-1]) - log_normalizer;
  }

  return log_pmfs;
}

/**
  * Compute the primary event censored PMF for integer delays up to max_delay
  * @ingroup primary_censored_vectorized
  *
  * @param max_delay Maximum delay to compute PMF for
  * @param D Maximum delay (truncation point), must be at least max_delay + 1
  * @param dist_id Distribution identifier
  * @param params Array of distribution parameters
  * @param pwindow Primary event window
  * @param primary_id Primary distribution identifier
  * @param primary_params Primary distribution parameters
  *
  * @return Vector of primary event censored PMFs for integer delays 1 to
  * max_delay
  *
  * This function differs from primarycensored_pmf in that it:
  * 1. Computes PMFs for all integer delays from \[0, 1\] to \[max_delay,
  *    max_delay + 1\] in one call.
  * 2. Assumes integer delays (swindow = 1)
  * 3. Is more computationally efficient for multiple delay calculations
  *
  * @code
  * // Example: Weibull delay distribution with uniform primary distribution
  * int max_delay = 10;
  * real D = 15.0;
  * int dist_id = 5; // Weibull
  * array[2] real params = {2.0, 1.5}; // shape and scale
  * real pwindow = 7.0;
  * int primary_id = 1; // Uniform
  * array[0] real primary_params = {};
  * vector[max_delay] pmf =
  *   primarycensored_sone_lpmf_vectorized(
  *      max_delay, D, dist_id, params, pwindow, primary_id, primary_params
  *   );
  * @endcode
  */
vector primarycensored_sone_pmf_vectorized(
  int max_delay, data real D, int dist_id,
  array[] real params, data real pwindow,
  int primary_id,
  array[] real primary_params
) {
  return exp(
    primarycensored_sone_lpmf_vectorized(
      max_delay, D, dist_id, params, pwindow, primary_id, primary_params
    )
  );
}
