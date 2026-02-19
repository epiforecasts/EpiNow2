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
