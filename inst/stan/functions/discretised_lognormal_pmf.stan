  // discretised truncated lognormal pmf
  real discretised_lognormal_pmf(int y, real mu, real sigma, int max_val) {
    real adj_y = y + 1e-5;
    return((normal_cdf((log(adj_y + 1) - mu) / sigma, 0.0, 1.0) - normal_cdf((log(adj_y) - mu) / sigma, 0.0, 1.0)) / 
            normal_cdf((log(max_val) - mu) / sigma, 0.0, 1.0));
  }
  
  
  