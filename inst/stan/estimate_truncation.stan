functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/observation_model.stan
#include functions/delays.stan
}
data {
  int t;
  int obs_sets;
  array[t, obs_sets] int obs;
  array[obs_sets] int obs_dist;
#include data/delays.stan
}
transformed data{
  int trunc_id = 1;
  array[obs_sets] int<lower = 1> end_t;
  array[obs_sets] int<lower = 1> start_t;

  array[delay_types] int delay_type_max;
  delay_type_max = get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  );

  for (i in 1:obs_sets) {
    end_t[i] = t - obs_dist[i];
    start_t[i] = max(1, end_t[i] - delay_type_max[trunc_id]);
  }
}
parameters {
  vector[delay_params_length] delay_params;
  real<lower=0> phi;
  real<lower=0> sigma;
}
transformed parameters{
  real sqrt_phi = 1 / sqrt(phi);
  matrix[delay_type_max[trunc_id] + 1, obs_sets - 1] trunc_obs = rep_matrix(
    0, delay_type_max[trunc_id] + 1, obs_sets - 1
  );
  vector[delay_type_max[trunc_id] + 1] trunc_rev_cmf = get_delay_rev_pmf(
    trunc_id, delay_type_max[trunc_id] + 1, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf,
    delay_np_pmf_groups, delay_params, delay_params_groups, delay_dist,
    0, 1, 1
  );
  {
  vector[t] last_obs;
  // reconstruct latest data without truncation

  last_obs = truncate(to_vector(obs[, obs_sets]), trunc_rev_cmf, 1);
  // apply truncation to latest dataset to map back to previous data sets and
  // add noise term
  for (i in 1:(obs_sets - 1)) {
    trunc_obs[1:(end_t[i] - start_t[i] + 1), i] =
      truncate(last_obs[start_t[i]:end_t[i]], trunc_rev_cmf, 0) + sigma;
   }
  }
}
model {
  // priors for the log normal truncation distribution
  delays_lp(
    delay_params, delay_params_mean, delay_params_sd, delay_params_groups,
    delay_dist, delay_weight
  );

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
  matrix[delay_type_max[trunc_id] + 1, obs_sets] recon_obs = rep_matrix(
    0, delay_type_max[trunc_id] + 1, obs_sets
  );
  matrix[delay_type_max[trunc_id] + 1, obs_sets - 1] gen_obs;
  // reconstruct all truncated datasets using posterior of the truncation distribution
  for (i in 1:obs_sets) {
    recon_obs[1:(end_t[i] - start_t[i] + 1), i] = truncate(
      to_vector(obs[start_t[i]:end_t[i], i]), trunc_rev_cmf, 1
    );
  }
 // generate observations for comparing
  for (i in 1:(obs_sets - 1)) {
    for (j in 1:(delay_type_max[trunc_id] + 1)) {
      if (trunc_obs[j, i] == 0) {
        gen_obs[j, i] = 0;
      } else {
        gen_obs[j, i] = neg_binomial_2_rng(trunc_obs[j, i], sqrt_phi);
      }
    }
  }
}
