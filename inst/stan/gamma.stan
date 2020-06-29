
data {
  int N;
  vector[N] low;
  vector[N] up;
  real prior_mean;
  real prior_sd;
  real <lower = 0> par_sigma;
}

transformed data {
  real prior_alpha = ((prior_mean) / prior_sd)^2;
  real prior_beta = (prior_mean) / (prior_sd^2);
}

parameters {
  real <lower = 0> alpha_raw;
  real <lower = 0> beta_raw;
}

transformed parameters{
  real <lower = 0> alpha;
  real <lower = 0> beta;

  alpha = prior_alpha + par_sigma * alpha_raw;
  beta = prior_beta + par_sigma * beta_raw;
}
model {
  alpha_raw ~ normal(0, 1);
  beta_raw ~ normal(0, 1);

  for(i in 1:N){
    target += log(gamma_cdf(up[i] , alpha, beta) - gamma_cdf(low[i] , alpha, beta));
  }
}

generated quantities {
  real mu = alpha / beta;
  real sigma = sqrt(alpha / (beta^2));
}
