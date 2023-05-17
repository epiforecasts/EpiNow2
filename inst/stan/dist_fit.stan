data {
  int dist; // 0: exp; 1: lnorm; 2: gamma
  int N;
  vector[N] low;
  vector[N] up;
  real lam_mean[dist == 0];
  real prior_mean[dist > 0];
  real prior_sd[dist > 0];
  real<lower = 0> par_sigma[dist == 2];
}

transformed data {
  real prior_alpha[dist == 2];
  real prior_beta[dist == 2];

  if (dist == 2) {
    prior_alpha[1] = (prior_mean[1] / prior_sd[1])^2;
    prior_beta[1] = prior_mean[1] / prior_sd[1]^2;
  }
}

parameters {
  real<lower = 0> lambda[dist == 0];
  real mu[dist == 0];
  real<lower = 0> sigma[dist == 1];
  real<lower = 0> alpha_raw[dist == 2];
  real<lower = 0> beta_raw[dist == 2];
}

transformed parameters{
  real<lower = 0> alpha[dist == 2];
  real<lower = 0> beta[dist == 2];

  if (dist == 2) {
    alpha[1] = prior_alpha[1] + par_sigma[1] * alpha_raw[1];
    beta[1] = prior_beta[1] + par_sigma[1] * beta_raw[1];
  }
}

model {
  if (dist == 0) {
    lambda[1] ~ uniform(1 / (5. * lam_mean[1]), 1 / (0.2 * lam_mean[1]));
  } else if (dist == 1) {
    mu[1] ~ normal(prior_mean[1], 10);
    sigma[1] ~ normal(prior_sd[1], 10) T[0,];
  } else if (dist == 2) {
    alpha_raw[1] ~ normal(0, 1);
    beta_raw[1] ~ normal(0, 1);
  }

  for(i in 1:N){
    if (dist == 0) {
      target += log(
        exponential_cdf(up[i] , lambda) -
        exponential_cdf(low[i], lambda)
      );
    } else if (dist == 1) {
      target += log(
        lognormal_cdf(up[i], mu, sigma) -
        lognormal_cdf(low[i], mu, sigma)
      );
    } else if (dist == 2) {
      target += log(
        gamma_cdf(up[i], alpha, beta) -
        gamma_cdf(low[i], alpha, beta)
      );
    }
  }
}
