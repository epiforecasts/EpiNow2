// discretised truncated gamma pmf
vector discretised_gamma_pmf(int[] y, real mu, real sigma, int max_val) {
  int n = num_elements(y);
  vector[n+1] upper_cdf;
  vector[n] pmf;
  real trunc_pmf;
  // calculate alpha and beta for gamma distribution
  real small = 1e-5;
  real large = 1e8;
  real c_sigma = fmax(small, sigma);
  real c_mu = fmax(small, mu);
  real alpha = ((c_mu) / c_sigma)^2;
  real beta = (c_mu) / (c_sigma^2);
  // account for numerical issues
  alpha = fmax(small, alpha);
  alpha = fmin(large, alpha);
  beta = fmax(small, beta);
  beta = fmin(large, beta);
  // calculate pmf
  for (i in 1:(n+1)) {
    upper_cdf[i] = gamma_cdf(i,  alpha, beta);
  }
  // discretise
  pmf[1:n] = upper_cdf[2:(n+1)] - upper_cdf[1:n];
  pmf = pmf / (upper_cdf[n+1] - upper_cdf[1]);
  return(pmf);
}

// discretised truncated lognormal pmf
vector discretised_lognormal_pmf(int[] y, real mu, real sigma, int max_val) {
  int n = num_elements(y);
  vector[n] upper_cdf;
  vector[n] pmf;
  real trunc_cdf = lognormal_cdf(max_val,  mu, sigma);
  for (i in 1:n) {
    upper_cdf[i] = lognormal_cdf(i,  mu, sigma);
  }
  // discretise
  pmf[1] = upper_cdf[1];
  pmf[2:n] = upper_cdf[2:n] - upper_cdf[1:(n-1)];
  // normalize
  pmf = pmf / trunc_cdf;
  return(pmf);
}

// reverse a mf
vector reverse_mf(vector pmf, int max_pmf) {
  vector[max_pmf] rev_pmf;
  for (d in 1:max_pmf) {
    rev_pmf[d] = pmf[max_pmf - d + 1];
  }
  return rev_pmf;
}

// discretised truncated gamma pmf
vector discretised_delta_pmf(int[] y) {
  int n = num_elements(y);
  vector[n] pmf;
  pmf[y[1]] = 1;
  if (n > 1) {
    for (i in 2:n) {
      pmf[y[i]] = 0;
    }
  }
  return(pmf);
}
