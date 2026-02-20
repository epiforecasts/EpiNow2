// Stan model using primarycensored functions for delay distribution estimation
// Compiles with rstan using primarycensored Stan functions
//
// This model uses primarycensored's sophisticated handling of:
// - Primary event censoring (daily reporting of exposure)
// - Secondary event censoring (daily reporting of symptom onset)
// - Right truncation (observation window effects)

functions {
  // Vendored from primarycensored using pcd_load_stan_functions()
  #include primarycensored.stan
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

  // Parameter bounds
  vector[2] param_lower;
  vector[2] param_upper;
}

parameters {
  // Distribution parameters (bounded for stability)
  real<lower=param_lower[1], upper=param_upper[1]> param1;
  real<lower=param_lower[2], upper=param_upper[2]> param2;
}

transformed parameters {
  array[2] real params = {param1, param2};
}

model {
  // Priors - weakly informative
  // The param_bounds keep these reasonable
  param1 ~ normal(0, 10);
  param2 ~ normal(1, 10) T[0,];

  // Likelihood using primarycensored
  for (i in 1:n) {
    target += n_obs[i] * primarycensored_lpmf(
      delay[i] | dist_id, params, pwindow, delay_upper[i], D,
      primary_id, primary_params
    );
  }
}

generated quantities {
  // Extract parameters in named format for easier extraction
  real meanlog = (dist_id == 1) ? param1 : 0;
  real sdlog = (dist_id == 1) ? param2 : 0;
  real shape = (dist_id == 2 || dist_id == 3) ? param1 : 0;
  real rate = (dist_id == 2) ? param2 : 0;
  real scale = (dist_id == 3) ? param2 : 0;
}
