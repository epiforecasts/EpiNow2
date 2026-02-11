data {
  int dist; // 0: exponential; 1: lognormal; 2: gamma
  int N;
  vector[N] low;
  vector[N] up;
  array[dist == 0] real<lower=0> lam_mean;
  array[dist > 0] real<lower=0> prior_mean;
  array[dist > 0] real<lower=0> prior_sd;
  array[dist == 2] real<lower = 0> par_sigma;
}

transformed data {
  array[dist == 2] real prior_alpha;
  array[dist == 2] real prior_beta;

  if (dist == 2) {
    prior_alpha[1] = (prior_mean[1] / prior_sd[1])^2;
    prior_beta[1] = prior_mean[1] / prior_sd[1]^2;
  }
}

parameters {
  array[dist == 0] real<lower = 0, upper = 1> lambda_raw;
  array[dist == 1] real mu;
  array[dist == 1] real<lower = 0> sigma;
  array[dist == 2] real<lower = 0> alpha_raw;
  array[dist == 2] real<lower = 0> beta_raw;
}

transformed parameters{
  array[dist == 0] real lambda;
  array[dist == 2] real<lower = 0> alpha;
  array[dist == 2] real<lower = 0> beta;

  if (dist == 0) {
    real lb = 0.2 / lam_mean[1];
    real ub = 5 / lam_mean[1];
    lambda[1] = lb + (ub - lb) * lambda_raw[1];
    // implies: lambda[1] ~ uniform(lb, ub)
  }
  
  if (dist == 2) {
    alpha[1] = prior_alpha[1] + par_sigma[1] * alpha_raw[1];
    beta[1] = prior_beta[1] + par_sigma[1] * beta_raw[1];
    // implies: alpha[1] ~ normal(prior_alpha[1], par_sigma[1])
    // implies: beta[1] ~ normal(prior_beta[1], par_sigma[1])
  }
}

model {
  if (dist == 0) {
    for (i in 1:N) {
      target += log_diff_exp(exponential_lcdf(up[i] | lambda),
			     exponential_lcdf(low[i] | lambda));
    }
  } else if (dist == 1) {
    mu[1] ~ normal(prior_mean[1], 10);
    // T[0, ] only needed if loc or scale arguments are params
    sigma[1] ~ normal(prior_sd[1], 10);
    for (i in 1:N) {
      target += log_diff_exp(lognormal_lcdf(up[i] | mu, sigma),
			     lognormal_lcdf(low[i] | mu, sigma));
    }
  } else if (dist == 2) {
    alpha_raw[1] ~ std_normal();
    beta_raw[1] ~ std_normal();
    for (i in 1:N) {
      target += log_diff_exp(gamma_lcdf(up[i] | alpha, beta),
			     gamma_lcdf(low[i] | alpha, beta));
    }
  }
}
