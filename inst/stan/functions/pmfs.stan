// Calculate the daily probability of reporting using parametric
// distributions up to the maximum observed delay.
// If sigma is 0 all the probability mass is put on n.
// Adapted from https://github.com/epiforecasts/epinowcast
// @author Sam Abbott
// @author Adrian Lison
vector discretised_pmf(real mu, real sigma, int n, int dist) {
  vector[n] lpmf;
  if (sigma > 0) {
    vector[n] upper_lcdf;
    if (dist == 0) {
      for (i in 1:n) {
        upper_lcdf[i] = lognormal_lcdf(i | mu, sigma);
      }
    } else if (dist == 1) {
      real alpha = mu^2 / sigma^2;
      real beta = mu / sigma^2;
      for (i in 1:n) {
        upper_lcdf[i] = gamma_lcdf(i | alpha, beta);
      }
    } else {
      reject("Unknown distribution function provided.");
    }
    // discretise
    lpmf[1] = upper_lcdf[1];
    lpmf[2:n] = log_diff_exp(upper_lcdf[2:n], upper_lcdf[1:(n-1)]);
    // normalize
    lpmf = lpmf - upper_lcdf[n];
  } else {
    // delta function
    lpmf = rep_vector(negative_infinity(), n);
    lpmf[n] = 0;
  }
  return(exp(lpmf));
}

// reverse a mf
vector reverse_mf(vector pmf) {
  int pmf_length = num_elements(pmf);
  vector[pmf_length] rev_pmf;
  for (d in 1:pmf_length) {
    rev_pmf[d] = pmf[pmf_length - d + 1];
  }
  return rev_pmf;
}

vector rev_seq(int base, int len) {
  vector[len] seq;
  for (i in 1:len) {
    seq[i] = len + base - i;
  }
  return(seq);
}

real rev_pmf_mean(vector rev_pmf, int base) {
  int len = num_elements(rev_pmf);
  vector[len] rev_pmf_seq = rev_seq(base, len);
  return(dot_product(rev_pmf_seq, rev_pmf));
}

real rev_pmf_var(vector rev_pmf, int base, real mean) {
  int len = num_elements(rev_pmf);
  vector[len] rev_pmf_seq = rev_seq(base, len);
  for (i in 1:len) {
    rev_pmf_seq[i] = pow(rev_pmf_seq[i], 2);
  }
  return(dot_product(rev_pmf_seq, rev_pmf) - pow(mean, 2));
}
