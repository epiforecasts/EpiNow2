
data {
  int N;
  vector[N] low;
  vector[N] up;
  real prior_mean;
  real prior_sd;
}

parameters {
  real<lower = 0> mu;
  real<lower = 0> sigma;
}

transformed parameters{
  real <lower = 0> alpha;
  real <lower = 0> beta;
  
  alpha = ((mu)/ sigma)^2;
  beta = (mu) / (sigma^2);
}
model {
  mu ~ normal(prior_mean, 10) T[0,];
  sigma ~ normal(prior_sd, 10) T[0,];

  for(i in 1:N){
    target += log(gamma_cdf(up[i] , alpha, beta) - gamma_cdf(low[i] , alpha, beta));
  }
}

