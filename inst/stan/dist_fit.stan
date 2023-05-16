data {
  int dist; // 0: exp; 1: lnorm; 2: gamma
  int N;
  vector[N] low;
  vector[N] up;
  real[dist == 0] lam_mean;
  real[dist > 0] prior_mean;
  real[dist > 0] prior_sd;
  real[dist == 2] <lower = 0> par_sigma;
}

transformed data {
  real[dist == 2] prior_alpha = ((prior_mean) / prior_sd)^2;
  real[dist == 2] prior_beta = (prior_mean) / (prior_sd^2);
}

parameters {
  real[dist == 0]<lower = 0> lambda;
  real[dist == 0] mu;
  real[dist == 1]<lower = 0> sigma;
  real[dist == 2]<lower = 0> alpha_raw;
  real[dist == 2]<lower = 0> beta_raw;
}

transformed parameters{
  real[dist == 2]<lower = 0> alpha;
  real[dist == 2]<lower = 0> beta;

  if (dist == 2) {
    alpha = prior_alpha + par_sigma * alpha_raw;
    beta = prior_beta + par_sigma * beta_raw;
  }
}

model {
  if (dist == 0) {
    lambda ~ uniform(1 / (5 * lam_mean), 1 / (0.2 * lam_mean));
  } else if (dist == 1) {
    mu ~ normal(prior_mean, 10);
    sigma ~ normal(prior_sd, 10) T[0,];
  } else if (dist == 2) {
    alpha_raw ~ normal(0, 1);
    beta_raw ~ normal(0, 1);
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

generated quantities {
  real[model == 2] mu;
  real[model == 2] sigma;

  if (model == 2) {
    mu = alpha / beta;
    sigma = sqrt(alpha / (beta^2));
  }
}
