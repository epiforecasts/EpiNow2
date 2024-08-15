/**
 * These functions implemente approximuate Gaussian processes for Stan using
 * Hilbert space methods. The functions are based on the following:
 * - https://avehtari.github.io/casestudies/Motorcycle/motorcycle_gpcourse.html#4_Heteroskedastic_GP_with_Hilbert_basis_functions
 * - https://arxiv.org/abs/2004.11408
 */
 
/**
  * Spectral density for Exponentiated Quadratic kernel
  *
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param L Length of the interval
  * @param M Number of basis functions
  * @return A vector of spectral densities
  */
vector diagSPD_EQ(real alpha, real rho, real L, int M) {
  vector[M] indices = linspaced_vector(M, 1, M);
  real factor = alpha * sqrt(sqrt(2 * pi()) * rho);
  real exponent = -0.25 * (rho * pi() / 2 / L)^2;
  return factor * exp(exponent * square(indices));
}

/**
  * Spectral density for Matern kernel
  *
  * @param nu Smoothness parameter (1/2, 3/2, or 5/2)
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param L Length of the interval
  * @param M Number of basis functions
  * @return A vector of spectral densities
  */
vector diagSPD_Matern(real nu, real alpha, real rho, real L, int M) {
  vector[M] indices = linspaced_vector(M, 1, M);
  real factor = 2 * alpha * pow(sqrt(2 * nu) / rho, nu);
  vector[M] denom = (sqrt(2 * nu) / rho)^2 + pow((pi() / 2 / L) * indices, nu + 0.5);
  return factor * inv(denom);
}

/**
  * Spectral density for periodic kernel
  *
  * @param alpha Scaling parameter
  * @param rho Length scale parameter
  * @param M Number of basis functions
  * @return A vector of spectral densities
  */
vector diagSPD_Periodic(real alpha, real rho, int M) {
  real a = inv_square(rho);
  vector[M] indices = linspaced_vector(M, 1, M);
  vector[M] q = exp(log(alpha) + 0.5 * (log(2) - a + to_vector(log_modified_bessel_first_kind(indices, a))));
  return append_row(q, q);
}

/**
  * Spectral density for Linear kernel
  *
  * @param alpha Scaling parameter
  * @param L Length of the interval
  * @param M Number of basis functions
  * @return A vector of spectral densities
  */
vector diagSPD_Linear(real alpha, real L, int M) {
  vector[M] indices = linspaced_vector(M, 1, M);
  return alpha * square(L) / (square(pi()) * square(indices));
}

/**
  * Basis functions for Gaussian Process
  *
  * @param N Number of data points
  * @param M Number of basis functions
  * @param L Length of the interval
  * @param x Vector of input data
  * @return A matrix of basis functions
  */
matrix PHI(int N, int M, real L, vector x) {
  matrix[N, M] phi = sin(diag_post_multiply(rep_matrix(pi() / (2 * L) * (x + L), M), linspaced_vector(M, 1, M))) / sqrt(L);
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
  */
matrix PHI_periodic(int N, int M, real w0, vector x) {
  matrix[N, M] mw0x = diag_post_multiply(rep_matrix(w0 * x, M), linspaced_vector(M, 1, M));
  return append_col(cos(mw0x), sin(mw0x));
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
  */
int setup_noise(int ot_h, int t, int horizon, int estimate_r,
                int stationary, int future_fixed, int fixed_from) {
  int noise_time = estimate_r > 0 ? (stationary > 0 ? ot_h : ot_h - 1) : t;
  int noise_terms = future_fixed > 0 ? (noise_time - horizon + fixed_from) : noise_time;
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
  */
matrix setup_gp(int M, real L, int dimension, int is_periodic, real w0) {
  vector[dimension] x = linspaced_vector(dimension, 1, dimension);
  x = (x - mean(x)) / sd(x);
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
  * @param type Type of kernel (0: SE, 1: Periodic, 2: Matern, 3: Linear)
  * @param nu Smoothness parameter for Matern kernel
  * @return A vector of updated noise terms
  */
vector update_gp(matrix PHI, int M, real L, real alpha,
                  array[] real rho, vector eta, int type, real nu) {
  vector[M] diagSPD;    // spectral density

  // GP in noise - spectral densities
  if (type == 0) {
    diagSPD = diagSPD_EQ(alpha, rho[1], L, M);
  } else if (type == 1) {
    diagSPD = diagSPD_Periodic(alpha, rho[1], M);
  } else if (type == 2) {
    diagSPD = diagSPD_Matern(nu, alpha, rho[1], L, M);
  } else if (type == 3) {
    diagSPD = diagSPD_Linear(alpha, L, M);
  }
  return PHI * (diagSPD .* eta);
}

/**
  * Prior for Gaussian process length scale
  *
  * @param rho Length scale parameter
  * @param ls_meanlog Mean of the log of the length scale
  * @param ls_sdlog Standard deviation of the log of the length scale
  * @param ls_min Minimum length scale
  * @param ls_max Maximum length scale
  */
void lengthscale_lp(real rho, real ls_meanlog, real ls_sdlog,
                    real ls_min, real ls_max) {
  if (ls_sdlog > 0) {
    rho ~ lognormal(ls_meanlog, ls_sdlog) T[ls_min, ls_max];
  } else {
    rho ~ inv_gamma(1.499007, 0.057277 * ls_max) T[ls_min, ls_max];
  }
}

/**
  * Priors for Gaussian process (excluding length scale)
  *
  * @param alpha Scaling parameter
  * @param eta Vector of noise terms
  * @param alpha_sd Standard deviation of alpha
  */
void gaussian_process_lp(real alpha, vector eta, real alpha_mean, 
                         real alpha_sd) {
  alpha ~ normal(alpha_mean, alpha_sd) T[0,];
  eta ~ std_normal();
}

