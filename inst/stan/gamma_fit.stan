
data {
  int N;
  vector[N] low;
  vector[N] up;
}

parameters {
  real<lower = 0> alpha;
  real<lower = 0> beta;
}

model {
  alpha ~ uniform(0, 10);
  beta ~ uniform(0, 10);

  for(i in 1:N){
    target += log(gamma_cdf(up[i] , alpha, beta) - gamma_cdf(low[i] , alpha, beta));
  }
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = log(gamma_cdf(up[n] , alpha, beta) - gamma_cdf(low[n] , alpha, beta));
  }
}
