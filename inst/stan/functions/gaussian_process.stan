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
  vector[M] indices = linspaced_vector(M, 1, M);
  real factor = 2;
  vector[M] denom = rho * ((1 / rho)^2 + pow(pi() / 2 / L * indices, 2));
  return alpha * sqrt(factor * inv(denom));
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
  vector[M] indices = linspaced_vector(M, 1, M);
  real factor = 2 * alpha * pow(sqrt(3) / rho, 1.5);
  vector[M] denom = (sqrt(3) / rho)^2 + pow((pi() / 2 / L) * indices, 2);
  return factor * inv(denom);
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
  vector[M] indices = linspaced_vector(M, 1, M);
  real factor = 3 * pow(sqrt(5) / rho, 5);
  vector[M] denom =
    2 * pow((sqrt(5) / rho)^2 + pow((pi() / 2 / L) * indices, 2), 3);
  return alpha * sqrt(factor * inv(denom));
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
  matrix[N, M] phi = sin(
    diag_post_multiply(
      rep_matrix(pi() / (2 * L) * (x + L), M), linspaced_vector(M, 1, M)
    )
  ) / sqrt(L);
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
  matrix[N, M] mw0x = diag_post_multiply(
    rep_matrix(w0 * x, M), linspaced_vector(M, 1, M)
  );
  return append_col(cos(mw0x), sin(mw0x));
}

/**
  * Setup Gaussian process noise dimensions
  *
  * @param ot_h observed time + forecast horizon
  * @param t total modelled time (including seeding time and horizon)
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
  vector[type == 1 ? 2 * M : M] diagSPD;    // spectral density

  // GP in noise - spectral densities
  if (type == 0) {
    diagSPD = diagSPD_EQ(alpha, rho, L, M);
  } else if (type == 1) {
    diagSPD = diagSPD_Periodic(alpha, rho, M);
  } else if (type == 2) {
    if (nu == 0.5) {
      diagSPD = diagSPD_Matern12(alpha, rho, L, M);
    } else if (nu == 1.5) {
      diagSPD = diagSPD_Matern32(alpha, rho, L, M);
    } else if (nu == 2.5) {
      diagSPD = diagSPD_Matern52(alpha, rho, L, M);
    } else {
      reject("nu must be one of 1/2, 3/2 or 5/2; found nu=", nu);
    }
  }
  return PHI * (diagSPD .* eta);
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

