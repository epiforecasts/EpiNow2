functions {
#include functions/convolve.stan
#include functions/pmfs.stan
#include functions/observation_model.stan
#include functions/delays.stan
#include functions/params.stan
}

data {
  int t;
  int obs_sets;
  array[t, obs_sets] int obs;
  array[obs_sets] int obs_dist;
  int model_type; // 0 = Poisson, 1 = NegBin
#include data/delays.stan
#include data/params.stan
  int<lower = 0> param_id_reporting_overdispersion;
}

transformed data{
  int delay_id_truncation = 1;
  array[obs_sets] int<lower = 1> end_t;
  array[obs_sets] int<lower = 1> start_t;

  array[delay_types] int delay_type_max;
  delay_type_max = get_delay_type_max(
    delay_types, delay_types_p, delay_types_id,
    delay_types_groups, delay_max, delay_np_pmf_groups
  );

  for (i in 1:obs_sets) {
    end_t[i] = t - obs_dist[i];
    start_t[i] = max(
      1, end_t[i] - delay_type_max[delay_id_truncation]
    );
  }
}

parameters {
  vector<lower = delay_params_lower>[delay_params_length]
    delay_params;
  vector<lower = params_lower, upper = params_upper>[
    n_params_variable
  ] params;
  real<lower = 0> sigma;
}

transformed parameters{
  matrix[delay_type_max[delay_id_truncation] + 1, obs_sets - 1]
    trunc_obs = rep_matrix(
      0, delay_type_max[delay_id_truncation] + 1, obs_sets - 1
    );
  vector[delay_type_max[delay_id_truncation] + 1]
    trunc_rev_cmf = get_delay_rev_pmf(
      delay_id_truncation,
      delay_type_max[delay_id_truncation] + 1,
      delay_types_p, delay_types_id, delay_types_groups,
      delay_max, delay_np_pmf, delay_np_pmf_groups,
      delay_params, delay_params_groups,
      delay_dist, 0, 1, 1
    );
  {
    vector[t] last_obs;
    // reconstruct latest data without truncation
    last_obs = truncate_obs(
      to_vector(obs[, obs_sets]), trunc_rev_cmf, 1
    );
    // apply truncation to latest dataset to map back to
    // previous data sets and add noise term
    for (i in 1:(obs_sets - 1)) {
      trunc_obs[1:(end_t[i] - start_t[i] + 1), i] =
        truncate_obs(
          last_obs[start_t[i]:end_t[i]], trunc_rev_cmf, 0
        ) + sigma;
    }
  }
}

model {
  // priors for the truncation distribution
  delays_lp(
    delay_params, delay_params_mean, delay_params_sd,
    delay_params_groups, delay_dist, delay_weight
  );

  // priors for params (reporting_overdispersion)
  params_lp(
    params, prior_dist, prior_dist_params,
    params_lower, params_upper
  );

  sigma ~ normal(0, 1) T[0,];

  // log density of truncated latest data vs that observed
  {
    real reporting_overdispersion = get_param(
      param_id_reporting_overdispersion,
      params_fixed_lookup, params_variable_lookup,
      params_value, params
    );
    for (i in 1:(obs_sets - 1)) {
      int n_t = end_t[i] - start_t[i] + 1;
      array[n_t] int case_times;
      for (j in 1:n_t) {
        case_times[j] = j;
      }
      report_lp(
        obs[start_t[i]:(start_t[i] + n_t - 1), i],
        case_times,
        trunc_obs[1:n_t, i],
        reporting_overdispersion, model_type, 1
      );
    }
  }
}

generated quantities {
  matrix[delay_type_max[delay_id_truncation] + 1, obs_sets]
    recon_obs = rep_matrix(
      0, delay_type_max[delay_id_truncation] + 1, obs_sets
    );
  matrix[delay_type_max[delay_id_truncation] + 1, obs_sets - 1]
    gen_obs;
  // reconstruct all truncated datasets using posterior
  for (i in 1:obs_sets) {
    recon_obs[1:(end_t[i] - start_t[i] + 1), i] = truncate_obs(
      to_vector(obs[start_t[i]:end_t[i], i]), trunc_rev_cmf, 1
    );
  }
  // generate observations for posterior predictive checks
  {
    real reporting_overdispersion = get_param(
      param_id_reporting_overdispersion,
      params_fixed_lookup, params_variable_lookup,
      params_value, params
    );
    for (i in 1:(obs_sets - 1)) {
      int n_t = end_t[i] - start_t[i] + 1;
      {
        array[n_t] int sampled = report_rng(
          trunc_obs[1:n_t, i],
          reporting_overdispersion, model_type
        );
        for (j in 1:n_t) {
          gen_obs[j, i] = sampled[j];
        }
      }
      // zero-fill remaining rows
      for (j in (n_t + 1):(delay_type_max[delay_id_truncation] + 1)) {
        gen_obs[j, i] = 0;
      }
    }
  }
}
