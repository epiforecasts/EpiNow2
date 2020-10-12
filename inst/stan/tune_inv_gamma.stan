// Adapted from here:
// https://betanalpha.github.io/assets/case_studies/gaussian_processes.html

functions {
  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {
    vector[2] deltas;
    real alpha = exp(y[1]);
    real beta = exp(y[2]);
    alpha = alpha <= 0 ? 1e-6 : alpha;
    beta = beta <= 0 ? 1e-6 : beta;
    alpha = alpha >= 1e6 ? 1e6 : alpha;
    beta = beta >= 1e6 ? 1e6 : beta;
    deltas[1] = inv_gamma_cdf(theta[1], alpha, beta) - 0.01;
    deltas[2] = 1 - inv_gamma_cdf(theta[2], alpha, beta) - 0.01;
    return deltas;
  }
}

data {
 real u;
 real l;
}

transformed data {
  vector[2] theta = [l, u]';
    
  real delta = 1;
  real a = square(delta * (u + l) / (u - l)) + 2;
  real b =  ((u + l) / 2) * (square(delta * (u + l) / (u - l)) + 1);
  vector[2] y_guess = [log(a), log(b)]';
  
  real x_r[0];
  int x_i[0];
  
  vector[2] y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);
}

generated quantities {
  real alpha = exp(y[1]);
  real beta = exp(y[2]);
}
