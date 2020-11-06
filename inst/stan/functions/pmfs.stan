 // discretised truncated gamma pmf
  vector discretised_gamma_pmf(int[] y, real mu, real sigma, int max_val) {
    int n = num_elements(y);
    vector[n] pmf;
    real trunc_pmf;
    // calculate alpha and beta for gamma distribution
    real c_sigma = sigma + 1e-5;
    real alpha = ((mu)/ c_sigma)^2;
    real beta = (mu) / (c_sigma^2);
    //account for numerical issues
    alpha = alpha <= 0 ? 1e-5 : alpha;
    beta = beta <= 0 ? 1e-5 : beta;
    alpha = is_inf(alpha) ? 1e8 : alpha;
    beta = is_inf(beta) ? 1e8 : beta; 
    // calculate pmf
    trunc_pmf = gamma_cdf(max_val + 1, alpha, beta) - gamma_cdf(1, alpha, beta);
    for (i in 1:n){
      pmf[i] = (gamma_cdf(y[i] + 1, alpha, beta) - gamma_cdf(y[i], alpha, beta)) / 
        trunc_pmf;
    }
    return(pmf);
  }


  // discretised truncated lognormal pmf
  vector discretised_lognormal_pmf(int[] y, real mu, real sigma, int max_val) {
    int n = num_elements(y);
    vector[n] pmf;
    real small = 1e-5;
    vector[n] adj_y = to_vector(y) + small;
    vector[n] upper_y = (log(adj_y + 1) - mu) / sigma;
    vector[n] lower_y = (log(adj_y) - mu) / sigma;
    real max_cdf = normal_cdf((log(max_val + small) - mu) / sigma, 0.0, 1.0);
    real min_cdf = normal_cdf((log(small) - mu) / sigma, 0.0, 1.0);
    real trunc_cdf = max_cdf - min_cdf;
    for (i in 1:n) {
      pmf[i] = (normal_cdf(upper_y[i], 0.0, 1.0) - normal_cdf(lower_y[i], 0.0, 1.0)) /
                trunc_cdf;
    }
    return(pmf);
  }


// reverse a pmf
vector reverse_pmf(vector pmf, int max_pmf) {
  vector[max_pmf] rev_pmf;
  for (d in 1:max_pmf) {
    rev_pmf[d] = pmf[max_pmf - d + 1];
  }
  return rev_pmf;
}