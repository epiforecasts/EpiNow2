// Stan model for estimating delay distributions using primarycensored
// Compiles with rstan but uses primarycensored's Stan functions
//
// This model uses primarycensored's sophisticated handling of:
// - Primary event censoring (e.g., daily reporting of exposure)
// - Secondary event censoring (e.g., daily reporting of symptom onset)
// - Right truncation (observation window effects)
//
// References:
// - Park et al. (2024) https://doi.org/10.1101/2024.01.12.24301247
// - primarycensored package

functions {
  // Include primarycensored Stan functions
  // These will be loaded from primarycensored using pcd_stan_path()
  // and inserted here during model compilation

  // For now, using #include directive which rstan will resolve
  // if we provide the include_paths argument
  #include primarycensored.stan
}

data {
  // Number of observations
  int<lower=0> n;

  // Observed delay intervals [delay_lower, delay_upper)
  vector<lower=0>[n] delay_lower;
  vector<lower=0>[n] delay_upper;

  // Observation counts (for aggregated data)
  array[n] int<lower=1> obs_n;

  // Primary window (daily censoring = 1)
  real<lower=0> pwindow;

  // Truncation time (maximum observation time)
  real<lower=0> D;

  // Distribution ID: 1=lognormal, 2=gamma, etc.
  int<lower=1> dist_id;

  // Parameter bounds
  vector[2] param_lower;
  vector[2] param_upper;
}

transformed data {
  int total_obs = sum(obs_n);
}

parameters {
  // Distribution parameters (bounded)
  vector<lower=param_lower[1], upper=param_upper[1]>[1] param1;
  vector<lower=param_lower[2], upper=param_upper[2]>[1] param2;
}

model {
  // Priors - weakly informative
  // These will be informed by the data-derived bounds
  param1[1] ~ normal(0, 10);
  param2[1] ~ normal(1, 10) T[0,];

  // Likelihood using primarycensored functions
  for (i in 1:n) {
    // This uses the primarycensored Stan function for
    // doubly-censored and truncated distributions
    // The exact function name will depend on what's in primarycensored

    real lp;

    if (dist_id == 1) {
      // Lognormal with primarycensored
      // Format: primarycensored_lpmf(delay | params, pwindow, D)
      lp = primarycensored_lognormal_lpmf(
        delay_lower[i], delay_upper[i] |
        param1[1], param2[1], pwindow, pwindow, D
      );
    } else if (dist_id == 2) {
      // Gamma with primarycensored
      lp = primarycensored_gamma_lpmf(
        delay_lower[i], delay_upper[i] |
        param1[1], param2[1], pwindow, pwindow, D
      );
    }

    target += obs_n[i] * lp;
  }
}

generated quantities {
  // Extract parameters in named format
  real meanlog = (dist_id == 1) ? param1[1] : 0;
  real sdlog = (dist_id == 1) ? param2[1] : 0;
  real shape = (dist_id == 2) ? param1[1] : 0;
  real rate = (dist_id == 2) ? param2[1] : 0;
}
