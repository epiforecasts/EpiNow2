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
  int model_type; // 0 = Poisson, 1 = NegBin
  real log_cases_guess; // log mean of latest snapshot for prior
#include data/delays.stan
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
  real log_cases_intercept;
  vector[t - 1] rw_noise;
  real<lower = 0> rw_sd;
  // only used when model_type == 1
  real<lower = 0> reporting_overdispersion;
}

transformed parameters{
  // latent cases via geometric random walk using cumulative_sum
  vector[t] log_cases;
  log_cases[1] = log_cases_intercept;
  log_cases[2:t] = rw_sd * rw_noise;
  log_cases = cumulative_sum(log_cases);
  vector[t] cases = exp(log_cases);

  // truncation reverse CMF
  vector[delay_type_max[delay_id_truncation] + 1]
    trunc_rev_cmf = get_delay_rev_pmf(
      delay_id_truncation,
      delay_type_max[delay_id_truncation] + 1,
      delay_types_p, delay_types_id, delay_types_groups,
      delay_max, delay_np_pmf, delay_np_pmf_groups,
      delay_params, delay_params_groups,
      delay_dist, 0, 1, 1
    );

  // expected observations per snapshot (truncated cases)
  matrix[t, obs_sets] expected_obs = rep_matrix(0, t, obs_sets);
  for (i in 1:obs_sets) {
    expected_obs[start_t[i]:end_t[i], i] = truncate_obs(
      cases[start_t[i]:end_t[i]], trunc_rev_cmf, 0
    );
  }
}

model {
  // priors for the truncation distribution
  delays_lp(
    delay_params, delay_params_mean, delay_params_sd,
    delay_params_groups, delay_dist, delay_weight
  );

  // random walk priors
  log_cases_intercept ~ normal(log_cases_guess, 2);
  rw_sd ~ normal(0, 0.1) T[0,];
  rw_noise ~ std_normal();

  // observation likelihood across all snapshots
  if (model_type) {
    reporting_overdispersion ~ normal(0, 1) T[0,];
    real phi = inv_square(reporting_overdispersion);
    for (i in 1:obs_sets) {
      for (j in start_t[i]:end_t[i]) {
        if (expected_obs[j, i] > 1e-8) {
          obs[j, i] ~ neg_binomial_2(
            expected_obs[j, i], phi
          );
        }
      }
    }
  } else {
    for (i in 1:obs_sets) {
      for (j in start_t[i]:end_t[i]) {
        if (expected_obs[j, i] > 1e-8) {
          obs[j, i] ~ poisson(expected_obs[j, i]);
        }
      }
    }
  }
}

generated quantities {
  // latent nowcast (untruncated cases)
  vector[t] cases_out = cases;
  // reconstructed observations per snapshot
  matrix[delay_type_max[delay_id_truncation] + 1, obs_sets]
    recon_obs =
      rep_matrix(
        0, delay_type_max[delay_id_truncation] + 1, obs_sets
      );
  // posterior predictive observations
  matrix[delay_type_max[delay_id_truncation] + 1, obs_sets]
    gen_obs =
      rep_matrix(
        0, delay_type_max[delay_id_truncation] + 1, obs_sets
      );

  for (i in 1:obs_sets) {
    int n_t = end_t[i] - start_t[i] + 1;
    recon_obs[1:n_t, i] = expected_obs[start_t[i]:end_t[i], i];
    {
      array[n_t] int sampled = report_rng(
        expected_obs[start_t[i]:end_t[i], i],
        reporting_overdispersion, model_type
      );
      for (j in 1:n_t) {
        gen_obs[j, i] = sampled[j];
      }
    }
  }
}
