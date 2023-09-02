functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/observation_model.stan
}
data {
  int t;
  int obs_sets;
  array[t, obs_sets] int obs;
  array[obs_sets] int obs_dist;
  int trunc_max;
  int trunc_dist;
}
transformed data{
  array[obs_sets] int<lower = 1> end_t;
  array[obs_sets] int<lower = 1> start_t;
  for (i in 1:obs_sets) {
    end_t[i] = t - obs_dist[i];
    start_t[i] = max(1, end_t[i] - trunc_max + 1);
  }
}
parameters {
  real logmean;
  real<lower=0> logsd;
  real<lower=0> phi;
  real<lower=0> sigma;
}
transformed parameters{
  matrix[trunc_max, obs_sets - 1] trunc_obs =
    rep_matrix(0, trunc_max, obs_sets - 1);
  real sqrt_phi = 1 / sqrt(phi);
  vector[trunc_max] rev_cmf = reverse_mf(cumulative_sum(
    discretised_pmf(logmean, logsd, trunc_max, trunc_dist)
  ));
  {
  vector[t] last_obs;
  // reconstruct latest data without truncation

  last_obs = truncate(to_vector(obs[, obs_sets]), rev_cmf, 1);
  // apply truncation to latest dataset to map back to previous data sets and
  // add noise term
  for (i in 1:(obs_sets - 1)) {
    trunc_obs[1:(end_t[i] - start_t[i] + 1), i] =
      truncate(last_obs[start_t[i]:end_t[i]], rev_cmf, 0) + sigma;
   }
  }
}
model {
  // priors for the log normal truncation distribution
  logmean ~ normal(0, 1);
  logsd ~ normal(0, 1) T[0,];
  phi ~ normal(0, 1) T[0,];
  sigma ~ normal(0, 1) T[0,];
  // log density of truncated latest data vs that observed
  for (i in 1:(obs_sets - 1)) {
    for (j in 1:(end_t[i] - start_t[i] + 1)) {
      obs[start_t[i] + j - 1, i] ~ neg_binomial_2(trunc_obs[j, i], sqrt_phi);
    }
  }
}
generated quantities {
  matrix[trunc_max, obs_sets] recon_obs = rep_matrix(0, trunc_max, obs_sets);
  matrix[trunc_max, obs_sets - 1] gen_obs;
  // reconstruct all truncated datasets using posterior of the truncation distribution
  for (i in 1:obs_sets) {
    recon_obs[1:(end_t[i] - start_t[i] + 1), i] = truncate(
      to_vector(obs[start_t[i]:end_t[i], i]), rev_cmf, 1
    );
  }
 // generate observations for comparing
  for (i in 1:(obs_sets - 1)) {
    for (j in 1:trunc_max) {
      if (trunc_obs[j, i] == 0) {
        gen_obs[j, i] = 0;
      } else {
        gen_obs[j, i] = neg_binomial_2_rng(trunc_obs[j, i], sqrt_phi);
      }
    }
  }
}
