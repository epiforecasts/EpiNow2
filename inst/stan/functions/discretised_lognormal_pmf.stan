  // discretised truncated lognormal pmf
  real discretised_lognormal_pmf(int y, real mu, real sigma, int max_val) {
    real small = 1e-5;
    real adj_y = y + small;
    return((normal_cdf((log(adj_y + 1) - mu) / sigma, 0.0, 1.0) -
            normal_cdf((log(adj_y) - mu) / sigma, 0.0, 1.0)) /
           (normal_cdf((log(max_val + small) - mu) / sigma, 0.0, 1.0) -
            normal_cdf((log(small) - mu) / sigma, 0.0, 1.0)));
  }
