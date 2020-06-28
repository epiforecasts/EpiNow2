
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
  mu ~ normal(prior_mean, 10);
  sigma ~ normal(prior_sd, 10) T[0,];

  for(i in 1:N){
   target += log(lognormal_cdf(up[i] , mu, sigma) - lognormal_cdf(low[i] , mu, sigma));
  }
}

