// Stan functions from primarycensored version 1.3.0
real expgrowth_lpdf(real x, real min, real max, real r) {
  if (x < min || x > max) {
    return negative_infinity();
  }
  if (abs(r) < 1e-10) {
    return -log(max - min);
  }
  return log(r) + r * (x - min) - log(exp(r * max) - exp(r * min));
}
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
real primarycensored_analytical_cdf(data real d, int dist_id,
                                          array[] real params,
                                          data real pwindow, data real D,
                                          int primary_id,
                                          array[] real primary_params) {
  return exp(primarycensored_analytical_lcdf(d | dist_id, params, pwindow, D, primary_id, primary_params));
}
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
real log_weibull_g(real t, real shape, real scale) {
  real x = pow(t * inv(scale), shape);
  real a = 1 + inv(shape);
  return log(gamma_p(a, x)) + lgamma(a);
}
int check_for_analytical(int dist_id, int primary_id) {
  if (dist_id == 2 && primary_id == 1) return 1; // Gamma delay with Uniform primary
  if (dist_id == 1 && primary_id == 1) return 1; // Lognormal delay with Uniform primary
  if (dist_id == 3 && primary_id == 1) return 1; // Weibull delay with Uniform primary
  return 0; // No analytical solution for other combinations
}
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
real primary_lpdf(real x, int primary_id, array[] real params, real min, real max) {
  // Implement switch for different primary distributions
  if (primary_id == 1) return uniform_lpdf(x | min, max);
  if (primary_id == 2) return expgrowth_lpdf(x | min, max, params[1]);
  // Add more primary distributions as needed
  reject("Invalid primary distribution identifier");
}
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
