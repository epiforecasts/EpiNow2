data {
  int N;
  vector[N] low;
  vector[N] up;
  real lam_mean;
}
parameters {
  real<lower = 0> lambda;
}
model {
  lambda ~ uniform(1/(5*lam_mean),1/(0.2*lam_mean));

  for(i in 1:N){
    target += log(exponential_cdf(up[i] , lambda) - exponential_cdf(low[i] , lambda));
  }

}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = log(exponential_cdf(up[n] , lambda) - exponential_cdf(low[n] , lambda));
  }
}
