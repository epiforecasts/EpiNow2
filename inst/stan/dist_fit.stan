data {
  int dist; // 0: exp; 1: lnorm; 2: gamma
  int N;
  vector[N] low;
  vector[N] up;
  array[dist == 0] real lam_mean;
  array[dist > 0] real prior_mean;
  array[dist > 0] real prior_sd;
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
  array[dist == 0] real<lower = 0> lambda;
  array[dist == 1] real mu;
  array[dist == 1] real<lower = 0> sigma;
  array[dist == 2] real<lower = 0> alpha_raw;
  array[dist == 2] real<lower = 0> beta_raw;
}

transformed parameters{
  array[dist == 2] real<lower = 0> alpha;
  array[dist == 2] real<lower = 0> beta;

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
        exponential_cdf(up[i] | lambda) -
        exponential_cdf(low[i] | lambda)
      );
    } else if (dist == 1) {
      target += log(
        lognormal_cdf(up[i] | mu, sigma) -
        lognormal_cdf(low[i] | mu, sigma)
      );
    } else if (dist == 2) {
      target += log(
        gamma_cdf(up[i] | alpha, beta) -
        gamma_cdf(low[i] | alpha, beta)
      );
    }
  }
}
