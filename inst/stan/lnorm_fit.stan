
data {
  int N;
  vector[N] low;
  vector[N] up;
  real prior_mean;
  real prior_sd;
}

parameters {
  real mu;
  real<lower = 0> sigma;
}

model {
  mu ~ normal(prior_mean, 1);
  sigma ~ normal(prior_sd, 1) T[0,];

  for(i in 1:N){
   target += log(lognormal_cdf(up[i] , mu, sigma) - lognormal_cdf(low[i] , mu, sigma));
  }
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = log(lognormal_cdf(up[n] , mu, sigma) - lognormal_cdf(low[n] , mu, sigma));
  }
}
