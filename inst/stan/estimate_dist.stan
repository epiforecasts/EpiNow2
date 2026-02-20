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
  // EpiNow2 parameter handling
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

  // Parameter bounds
  vector[2] param_lower;
  vector[2] param_upper;

  // Prior specification (0: lognormal, 1: gamma, 2: normal)
  array[2] int prior_dist;
  vector[4] prior_dist_params;  // 2 params per distribution
}

parameters {
  // Distribution parameters (bounded)
  vector<lower=param_lower, upper=param_upper>[2] params_raw;
}

transformed parameters {
  array[2] real params = to_array_1d(params_raw);
}

model {
  // Priors using EpiNow2's params_lp function
  params_lp(params_raw, prior_dist, prior_dist_params, param_lower, param_upper);

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
  real meanlog = (dist_id == 1) ? params_raw[1] : 0;
  real sdlog = (dist_id == 1) ? params_raw[2] : 0;
  real shape = (dist_id == 2 || dist_id == 3) ? params_raw[1] : 0;
  real rate = (dist_id == 2) ? params_raw[2] : 0;
  real scale = (dist_id == 3) ? params_raw[2] : 0;
}
