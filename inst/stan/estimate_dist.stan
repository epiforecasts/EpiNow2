// Stan model for delay distribution estimation using
// primarycensored likelihood functions.

functions {
#include functions/primarycensored.stan
#include functions/params.stan
}

data {
  // Number of unique delay intervals
  int<lower=0> n;

  // Delay intervals [delay, delay_upper)
  array[n] int<lower=0> delay;
  array[n] real<lower=0> delay_upper;

  // Observation counts per interval
  array[n] int<lower=1> n_obs;

  // Per-observation primary window
  array[n] int<lower=0> pwindow;

  // Per-observation left truncation point
  array[n] real<lower=0> L;

  // Per-observation right truncation time
  array[n] real<lower=0> D;

  // Distribution ID: 1=lognormal, 2=gamma
  int<lower=1> dist_id;

  // Primary distribution ID: 1=uniform, 2=expgrowth
  int<lower=1> primary_id;

  // Primary distribution parameters (empty for uniform)
  int<lower=0> n_primary_params;
  array[n_primary_params] real primary_params;

  // Delay distribution parameter specification
  #include data/params.stan
}

parameters {
  vector<lower=params_lower,
         upper=params_upper>[n_params_variable] params;
}

transformed parameters {
  // Expose delay parameters under a descriptive name
  array[n_params_fixed + n_params_variable] real
    delay_params;
  for (j in 1:(n_params_fixed + n_params_variable)) {
    if (params_fixed_lookup[j] > 0) {
      delay_params[j] =
        params_value[params_fixed_lookup[j]];
    } else {
      delay_params[j] =
        params[params_variable_lookup[j]];
    }
  }
}

model {
  // Priors
  params_lp(
    params, prior_dist, prior_dist_params,
    params_lower, params_upper
  );

  // Likelihood
  for (i in 1:n) {
    target += n_obs[i] * primarycensored_lpmf(
      delay[i] | dist_id, delay_params, pwindow[i],
      delay_upper[i], L[i], D[i],
      primary_id, primary_params
    );
  }
}
