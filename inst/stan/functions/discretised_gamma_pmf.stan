 // discretised truncated gamma pmf
  real discretised_gamma_pmf(int y, real mu, real sigma, int max_val) {
    // calculate alpha and beta for gamma distribution
    real c_sigma = sigma + 1e-5;
    real alpha = ((mu)/ c_sigma)^2;
    real beta = (mu) / (c_sigma^2);
    //account for numerical issues
    alpha = alpha <= 0 ? 1e-5 : alpha;
    beta = beta <= 0 ? 1e-5 : beta;
    alpha = is_inf(alpha) ? 1e8 : alpha;
    beta = is_inf(beta) ? 1e8 : beta; 
    return((gamma_cdf(y + 1, alpha, beta) - gamma_cdf(y, alpha, beta)) / 
    (gamma_cdf(max_val + 1, alpha, beta) - gamma_cdf(1, alpha, beta)));
  }
