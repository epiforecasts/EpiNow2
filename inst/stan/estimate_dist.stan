// Stan model using primarycensored functions for delay distribution estimation
// Compiles with rstan using primarycensored Stan functions
//
// This model uses primarycensored's sophisticated handling of:
// - Primary event censoring (daily reporting of exposure)
// - Secondary event censoring (daily reporting of symptom onset)
// - Right truncation (observation window effects)

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

  // Primary window (daily censoring = 1)
  real<lower=0> pwindow;

  // Truncation time (maximum observation time)
  real<lower=0> D;

  // Distribution ID: 1=lognormal, 2=gamma, 3=weibull
  int<lower=1> dist_id;

  // Primary distribution ID: 1=uniform (most common)
  int<lower=1> primary_id;

  // Primary distribution parameters (empty for uniform)
  int<lower=0> n_primary_params;
  array[n_primary_params] real primary_params;

  // Parameter specification (standard EpiNow2 params interface)
  #include data/params.stan
}

parameters {
  // Distribution parameters (using standard EpiNow2 params interface)
  vector<lower=params_lower, upper=params_upper>[n_params_variable] params;
}

model {
  // Build full parameter array from fixed and variable params
  int n_params = n_params_fixed + n_params_variable;
  array[n_params] real params_array;
  for (j in 1:n_params) {
    if (params_fixed_lookup[j] > 0) {
      params_array[j] = params_value[params_fixed_lookup[j]];
    } else {
      params_array[j] = params[params_variable_lookup[j]];
    }
  }

  // Priors using EpiNow2's params_lp function
  params_lp(params, prior_dist, prior_dist_params, params_lower, params_upper);

  // Likelihood using primarycensored
  // L=0 means no left truncation
  for (i in 1:n) {
    target += n_obs[i] * primarycensored_lpmf(
      delay[i] | dist_id, params_array, pwindow, delay_upper[i], 0.0, D,
      primary_id, primary_params
    );
  }
}

// No generated quantities needed - parameter naming handled in R
