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
