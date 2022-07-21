functions {
#include functions/pmfs.stan
#include functions/observation_model.stan
}
data {
  int t;
  int obs_sets;
  int obs[t, obs_sets];
  int obs_dist[obs_sets];
  int trunc_max[1];
}
parameters {
  real logmean[1];
  real<lower=0> logsd[1];
  real<lower=0> phi;
  real<lower=0> sigma;
}
transformed parameters{
  matrix[trunc_max[1], obs_sets - 1] trunc_obs;
  real sqrt_phi = 1 / sqrt(phi);
  {
  vector[t] last_obs;
  // reconstruct latest data without truncation
  last_obs = truncate(to_vector(obs[, obs_sets]), logmean, logsd, trunc_max, 1);
  // apply truncation to latest dataset to map back to previous data sets and
  // add noise term
  for (i in 1:(obs_sets - 1)) {
   int end_t = t - obs_dist[i];
   int start_t = end_t - trunc_max[1] + 1;
   trunc_obs[, i] = truncate(last_obs[start_t:end_t], logmean, logsd,
                             trunc_max, 0) + sigma;
   }
  }
}
model {
  // priors for the log normal truncation distribution
  logmean ~ normal(0, 1);
  logsd[1] ~ normal(0, 1) T[0,];
  phi ~ normal(0, 1) T[0,];
  sigma ~ normal(0, 1) T[0,];
  // log density of truncated latest data vs that observed
  for (i in 1:(obs_sets - 1)) {
    int start_t = t - obs_dist[i] - trunc_max[1];
    for (j in 1:trunc_max[1]) {
      obs[start_t + j, i] ~ neg_binomial_2(trunc_obs[j, i], sqrt_phi);
    }
  }
}
generated quantities {
  matrix[trunc_max[1], obs_sets] recon_obs;
  vector[trunc_max[1]] cmf;
  // reconstruct all truncated datasets using posterior of the truncation distribution
  for (i in 1:obs_sets) {
    int end_t = t - obs_dist[i];
    int start_t = end_t - trunc_max[1] + 1;
    recon_obs[, i] = truncate(to_vector(obs[start_t:end_t, i]), logmean, logsd, trunc_max, 1);
  }
  // generate a posterior for the cmf of the truncation distribution
  cmf = truncation_cmf(logmean[1], logsd[1], trunc_max[1]);
}

