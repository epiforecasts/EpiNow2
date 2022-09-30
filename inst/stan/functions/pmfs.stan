// Calculate the daily probability of reporting using parametric
// distributions up to the maximum observed delay.
// If sigma is 0 all the probability mass is put on n.
// Adapted from https://github.com/epiforecasts/epinowcast
// @author Sam Abbott
// @author Adrian Lison
vector discretised_pmf(real mu, real sigma, int n, int dist) {
  vector[n] pmf; 
  if (sigma > 0) {
    vector[n] upper_cdf;
    if (dist == 0) {
      for (i in 1:n) {
        upper_cdf[i] = lognormal_cdf(i, mu, sigma);
      }
    } else if (dist == 1) {
      real alpha = mu^2 / sigma^2;
      real beta = mu / sigma^2;
      for (i in 1:n) {
        upper_cdf[i] = gamma_cdf(i, alpha, beta);
      }
    } else {
      reject("Unknown distribution function provided.");
    }
    // discretise
    pmf[1] = upper_cdf[1];
    pmf[2:n] = upper_cdf[2:n] - upper_cdf[1:(n-1)];
    // normalize
    pmf = pmf / upper_cdf[n];
  } else {
    // delta function
    pmf = rep_vector(0, n);
    pmf[n] = 1;
  }
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

