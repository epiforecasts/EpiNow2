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
}
transformed parameters{
  matrix[trunc_max[1], obs_sets - 1] trunc_obs;
  {
  vector[t] last_obs;
  // reconstruct latest data without truncation
  last_obs = truncate(to_vector(obs[, obs_sets]), logmean, logsd, trunc_max, 1);
  // apply truncation to latest dataset to map back to previous data sets
  for (i in 1:(obs_sets - 1)) {
   int end_t = t - obs_dist[i];
   int start_t = end_t - trunc_max[1] + 1;
   trunc_obs[, i] = truncate(last_obs[start_t:end_t], logmean, logsd, trunc_max, 0);
  }
  }
}
model {
  // priors for the log normal truncation distribution
  logmean ~ normal(0, 1);
  logsd[1] ~ normal(0, 1) T[0,];
  // log density of truncated latest data vs that observed
  for (i in 1:(obs_sets - 1)) {
    int start_t = t - obs_dist[i] - trunc_max[1];
    for (j in 1:trunc_max[1]) {
      obs[start_t + j, obs_sets] ~ poisson(trunc_obs[j, i]);
    }
  }
}
// generated quantities {
//   // reconstruct all truncated datasets using posterior of the truncation distribution
//   matrix[t, obs_sets] recon_obs;
//   for (i in 1:obs_sets) {
//     int end_t = t - obs_dist[i];
//     recon_obs[1:end_t, i] = truncate(to_vector(obs[1:end_t, i]), logmean, logsd, trunc_max, 1);
//     recon_obs[(end_t + 1):t, i] = rep_vector(0, obs_dist[i]);
//   }
// }

