/**
 * These functions implement approximate Gaussian processes for Stan using
 * Hilbert space methods. The functions are based on the following:
 * - https://avehtari.github.io/casestudies/Motorcycle/motorcycle_gpcourse.html (Section 4)
 * - https://doi.org/10.1007/s11222-022-10167-2
 */

/**
  * Spectral density for Exponentiated Quadratic kernel
  *
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param L Length of the interval
  * @param M Number of basis functions
  * @return A vector of spectral densities
  *
  * @ingroup estimates_smoothing
  */
vector diagSPD_EQ(real alpha, real rho, real L, int M) {
  vector[M] indices = linspaced_vector(M, 1, M);
  real factor = alpha * sqrt(sqrt(2 * pi()) * rho);
  real exponent = -0.25 * (rho * pi() / 2 / L)^2;
  return factor * exp(exponent * square(indices));
}

/**
  * Squared spectral indices shared by the Matern kernels
  *
  * Returns `square(pi() / (2 * L) * linspaced_vector(M, 1, M))`, the term that
  * appears in the denominator of every Matern spectral density. The
  * `linspaced_vector()` call uses data-only bounds and is scaled afterwards so
  * the function compiles under the data-only argument constraint in older Stan
  * versions (e.g. the one shipped with rstan).
  *
  * @param M Number of basis functions
  * @param L Length of the interval
  * @return A vector of squared spectral indices
  *
  * @ingroup estimates_smoothing
  */
vector matern_indices(int M, real L) {
  vector[M] indices = linspaced_vector(M, 1, M);
  return square(pi() / (2 * L) * indices);
}

/**
  * Spectral density for 1/2 Matern (Ornstein-Uhlenbeck) kernel
  *
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param L Length of the interval
  * @param M Number of basis functions
  * @return A vector of spectral densities
  *
  * @ingroup estimates_smoothing
  */
vector diagSPD_Matern12(real alpha, real rho, real L, int M) {
  vector[M] denom = 1 / rho + rho * matern_indices(M, L);
  return alpha * sqrt(2 ./ denom);
}

/**
  * Spectral density for 3/2 Matern kernel
  *
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param L Length of the interval
  * @param M Number of basis functions
  * @return A vector of spectral densities
  *
  * @ingroup estimates_smoothing
  */
vector diagSPD_Matern32(real alpha, real rho, real L, int M) {
  real factor = 2 * alpha * (sqrt(3) / rho)^1.5;
  vector[M] denom = 3 / square(rho) + matern_indices(M, L);
  return factor ./ denom;
}

/**
  * Spectral density for 5/2 Matern kernel
  *
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param L Length of the interval
  * @param M Number of basis functions
  * @return A vector of spectral densities
  *
  * @ingroup estimates_smoothing
  */
vector diagSPD_Matern52(real alpha, real rho, real L, int M) {
  real factor = 16 * pow(sqrt(5) / rho, 5);
  vector[M] denom = 3 * pow(5 / square(rho) + matern_indices(M, L), 3);
  return alpha * sqrt(factor ./ denom);
}

/**
  * Spectral density for periodic kernel
  *
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param M Number of basis functions
  * @return A vector of spectral densities
  *
  * @ingroup estimates_smoothing
  */
vector diagSPD_Periodic(real alpha, real rho, int M) {
  real a = inv_square(rho);
  vector[M] indices = linspaced_vector(M, 1, M);
  vector[M] q = exp(
    log(alpha) + 0.5 *
      (log(2) - a + to_vector(log_modified_bessel_first_kind(indices, a)))
  );
  return append_row(q, q);
}

/**
  * Basis functions for Gaussian Process
  *
  * @param N Number of data points
  * @param M Number of basis functions
  * @param L Length of the interval
  * @param x Vector of input data
  * @return A matrix of basis functions
  *
  * @ingroup estimates_smoothing
  */
matrix PHI(int N, int M, real L, vector x) {
  row_vector[M] k = linspaced_row_vector(M, 1, M);
  matrix[N, M] phi = sin((pi() / (2 * L) * (x + L)) * k) / sqrt(L);
  return phi;
}

/**
  * Basis functions for periodic Gaussian Process
  *
  * @param N Number of data points
  * @param M Number of basis functions
  * @param w0 Fundamental frequency
  * @param x Vector of input data
  * @return A matrix of basis functions
  *
  * @ingroup estimates_smoothing
  */
matrix PHI_periodic(int N, int M, real w0, vector x) {
  row_vector[M] k = linspaced_row_vector(M, 1, M);
  matrix[N, M] w0xk = (w0 * x) * k;
  return append_col(cos(w0xk), sin(w0xk));
}

/**
  * Setup Gaussian process noise dimensions
  *
  * @param ot_h Observation time horizon
  * @param t Total time points
  * @param horizon Forecast horizon
  * @param estimate_r Indicator if estimating r
  * @param stationary Indicator if stationary
  * @param future_fixed Indicator if future is fixed
  * @param fixed_from Fixed point from
  * @return Number of noise terms
  *
  * @ingroup estimates_smoothing
  */
int setup_noise(int ot_h, int t, int horizon, int estimate_r,
                int stationary, int future_fixed, int fixed_from) {
  int noise_time = estimate_r > 0 ? (stationary > 0 ? ot_h : ot_h - 1) : t;
  int noise_terms =
    future_fixed > 0 ? (noise_time - horizon + fixed_from) : noise_time;
  return noise_terms;
}

/**
  * Setup approximate Gaussian process
  *
  * @param M Number of basis functions
  * @param L Length of the interval
  * @param dimension Dimension of the process
  * @param is_periodic Indicator if the process is periodic
  * @param w0 Fundamental frequency for periodic process
  * @return A matrix of basis functions
  *
  * @ingroup estimates_smoothing
  */
matrix setup_gp(int M, real L, int dimension, int is_periodic, real w0) {
  vector[dimension] x = linspaced_vector(dimension, 1, dimension);
  x = 2 * (x - mean(x)) / (max(x) - 1);
  if (is_periodic) {
    return PHI_periodic(dimension, M, w0, x);
  } else {
    return PHI(dimension, M, L, x);
  }
}

/**
  * Rescale the Gaussian process lengthscale to the basis function inputs
  *
  * The inputs are rescaled to [-1, 1] in `setup_gp()`, so the lengthscale is
  * divided by the half-range of the time points.
  *
  * @param rho Length scale parameter (in days)
  * @param dimension Dimension of the process
  * @return The rescaled length scale
  *
  * @ingroup estimates_smoothing
  */
real rescale_gp_lengthscale(real rho, int dimension) {
  return 2 * rho / max(dimension - 1, 1);
}

/**
  * Spectral densities of the Gaussian process basis functions
  *
  * @param M Number of basis functions
  * @param L Length of the interval
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param type Type of kernel (0: SE, 1: Periodic, 2: Matern)
  * @param nu Smoothness parameter for Matern kernel
  * @return A vector of spectral densities, of length 2 * M for the periodic
  *   kernel and M otherwise
  *
  * @ingroup estimates_smoothing
  */
vector gp_spd(int M, real L, real alpha, real rho, int type, real nu) {
  if (type == 0) {
    return diagSPD_EQ(alpha, rho, L, M);
  } else if (type == 1) {
    return diagSPD_Periodic(alpha, rho, M);
  } else if (nu == 0.5) {
    return diagSPD_Matern12(alpha, rho, L, M);
  } else if (nu == 1.5) {
    return diagSPD_Matern32(alpha, rho, L, M);
  } else if (nu == 2.5) {
    return diagSPD_Matern52(alpha, rho, L, M);
  }
  reject("nu must be one of 0.5, 1.5, or 2.5; found nu=", nu);
}

/**
  * Update Gaussian process using spectral densities
  *
  * @param PHI Basis functions matrix
  * @param M Number of basis functions
  * @param L Length of the interval
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param eta Vector of noise terms
  * @param type Type of kernel (0: SE, 1: Periodic, 2: Matern)
  * @param nu Smoothness parameter for Matern kernel
  * @return A vector of updated noise terms
  */
vector update_gp(matrix PHI, int M, real L, real alpha,
                 real rho, vector eta, int type, real nu) {
  return PHI * (gp_spd(M, L, alpha, rho, type, nu) .* eta);
}

/**
  * Degree of centring of the Gaussian process trend
  *
  * The trend is the sum of the GP increments over the observation window.
  * Its prior sd is `scale`, which grows with the GP magnitude. The data pin
  * the trend down to roughly `sigma`. Sampling it centred is efficient when
  * `scale` is large relative to `sigma`, and non-centred when it is small,
  * which is where a centred parameterisation would form a funnel.
  *
  * @param scale Prior sd of the trend
  * @param sigma Expected posterior sd of the trend
  * @return The degree of centring, between 0 (non-centred) and 1 (centred)
  *
  * @ingroup estimates_smoothing
  */
real gp_trend_centring(real scale, real sigma) {
  return square(scale) / (square(scale) + square(sigma));
}

/**
  * Prior sd of the raw Gaussian process trend parameter
  *
  * With degree of centring `c`, the trend is
  * `sigma^c * scale^(1 - c) * raw` and `raw` has sd `(scale / sigma)^c`, so
  * the trend has sd `scale` whatever `c` is.
  *
  * @param scale Prior sd of the trend
  * @param sigma Expected posterior sd of the trend
  * @return The prior sd of the raw trend parameter
  *
  * @ingroup estimates_smoothing
  */
real gp_trend_raw_sd(real scale, real sigma) {
  return pow(scale / sigma, gp_trend_centring(scale, sigma));
}

/**
  * Set the trend of the Gaussian process noise terms
  *
  * Replaces the component of `eta` along `w` so that `dot(w, eta)`, the
  * trend, comes from `raw` instead. With `eta` standard normal and `raw`
  * normal with sd [gp_trend_raw_sd()], the result is standard normal, so the
  * Gaussian process prior is unchanged.
  *
  * @param eta Vector of noise terms
  * @param w Trend loadings: basis column sums over the observation window
  *   times the spectral densities
  * @param raw Raw trend parameter
  * @param sigma Expected posterior sd of the trend
  * @return Noise terms with the trend set from `raw`
  *
  * @ingroup estimates_smoothing
  */
vector gp_eta_with_trend(vector eta, vector w, real raw, real sigma) {
  real ww = dot_self(w);
  if (ww == 0) {
    return eta;
  }
  real scale = sqrt(ww);
  real c = gp_trend_centring(scale, sigma);
  real trend = pow(sigma, c) * pow(scale, 1 - c) * raw;
  return eta + (trend - dot_product(w, eta)) / ww * w;
}

/**
  * Priors for Gaussian process (excluding length scale)
  *
  * @param eta Vector of noise terms
  *
  * @ingroup estimates_smoothing
  */
void gaussian_process_lp(vector eta) {
  eta ~ std_normal();
}

