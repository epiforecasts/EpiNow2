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

// discretised delta pmf
vector discretised_delta_pmf(int n) {
  vector[n] pmf = rep_vector(0, n);
  pmf[n] = 1;
  return(pmf);
}
