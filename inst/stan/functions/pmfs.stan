// Calculate the daily probability of reporting using parametric
// distributions up to the maximum observed delay.
// Adapted from https://github.com/epiforecasts/epinowcast
// (MIT License, copyright: epinowcast authors)
vector discretised_pmf(vector params, int n, int dist) {
  vector[n] lpmf;
  vector[n] upper_lcdf;
  if (dist == 0) {
    for (i in 1:n) {
      upper_lcdf[i] = lognormal_lcdf(i | params[1], params[2]);
    }
  } else if (dist == 1) {
    for (i in 1:n) {
      upper_lcdf[i] = gamma_lcdf(i | params[1], params[2]);
    }
  } else {
    reject("Unknown distribution function provided.");
  }
  // discretise
  if (n > 1) {
    lpmf[1] = upper_lcdf[1];
    lpmf[2] = upper_lcdf[2];
    if (n > 2) {
      lpmf[3:n] = log_diff_exp(upper_lcdf[3:n], upper_lcdf[1:(n - 2)]);
    }
    // normalize
    lpmf = lpmf - log_sum_exp(upper_lcdf[(n - 1):n]);
  } else {
    lpmf[1] = 0;
  }
  return(exp(lpmf));
}
