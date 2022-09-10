// discretised truncated gamma pmf
vector discretised_gamma_pmf(int[] y, real mu, real sigma, int max_val) {
  int n = num_elements(y);
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
  trunc_pmf = gamma_cdf(max_val + 1, alpha, beta) - gamma_cdf(1, alpha, beta);
  for (i in 1:n){
    pmf[i] = (gamma_cdf(y[i] + 1, alpha, beta) - gamma_cdf(y[i], alpha, beta)) /
    trunc_pmf;
  }
  return(pmf);
}

// Calculate the daily probability of reporting using parametric
// distributions up to the maximum observed delay
// Adapted from https://github.com/epiforecasts/epinowcast
// @author Sam Abbott
// @author Adrian Lison
vector discretised_pmf(real mu, real sigma, int n, int dist) {
  vector[n] pmf; 
  vector[n] upper_cdf;
  if (dist == 0) {
    for (i in 1:n) {
      upper_cdf[i] = lognormal_cdf(i , mu, sigma);
    }
  } else if (dist == 1) {
    real emu = exp(2 * (log(mu) - log(sigma)));
    real esigma = exp(log(mu) - 2 * log(sigma)); 
    for (i in 1:n) {
      upper_cdf[i] = gamma_cdf(i, emu, esigma);
    }
  } else {
    reject("Unknown distribution function provided.");
  }
  // discretise
  pmf[1] = upper_cdf[1];
  pmf[2:n] = upper_cdf[2:n] - upper_cdf[1:(n-1)];
  // normalize
  pmf = pmf / upper_cdf[n];
  return(pmf);
}

// reverse a mf
vector reverse_mf(vector pmf) {
  int max_pmf = num_elements(pmf);
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
