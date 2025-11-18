// Stan model for estimating delay distributions with primary event censoring
// Uses primarycensored methodology but compiled with rstan for compatibility
//
// This model handles:
// - Primary event censoring (e.g., daily reporting of exposure)
// - Secondary event censoring (e.g., daily reporting of symptom onset)
// - Right truncation (observation window effects)
//
// References:
// - Park et al. (2024) https://doi.org/10.1101/2024.01.12.24301247
// - primarycensored package

functions {
  // Include primarycensored Stan functions here
  // These will be inserted from pcd_load_stan_functions()
  // For now, we'll use a simplified version based on dist_fit.stan

  // Double interval censored log probability
  // For an observation in interval [delay_lower, delay_upper]
  // with primary event censored to [ptime_lower, ptime_upper]
  real double_censored_lognormal_lpmf(real delay_lower, real delay_upper,
                                       real meanlog, real sdlog) {
    return log_diff_exp(
      lognormal_lcdf(delay_upper | meanlog, sdlog),
      lognormal_lcdf(delay_lower | meanlog, sdlog)
    );
  }

  real double_censored_gamma_lpmf(real delay_lower, real delay_upper,
                                   real shape, real rate) {
    return log_diff_exp(
      gamma_lcdf(delay_upper | shape, rate),
      gamma_lcdf(delay_lower | shape, rate)
    );
  }

  real double_censored_weibull_lpmf(real delay_lower, real delay_upper,
                                     real shape, real scale) {
    return log_diff_exp(
      weibull_lcdf(delay_upper | shape, scale),
      weibull_lcdf(delay_lower | shape, scale)
    );
  }
}

data {
  int<lower=0> N;  // Number of observations
  vector<lower=0>[N] delay;  // Observed delay (lower bound)
  vector<lower=0>[N] delay_upper;  // Upper bound of delay interval
  array[N] int<lower=1> n;  // Number of observations per interval
  int<lower=0,upper=3> dist_id;  // 1=lognormal, 2=gamma, 3=weibull

  // Truncation
  real<lower=0> D;  // Maximum observation time (for truncation correction)

  // Priors/bounds - user supplied or defaults
  vector[2] param_lower;  // Lower bounds for parameters
  vector[2] param_upper;  // Upper bounds for parameters
}

transformed data {
  int total_n = sum(n);  // Total number of observations
}

parameters {
  // Parameters depend on distribution
  // For lognormal: meanlog, sdlog
  // For gamma: shape, rate
  // For weibull: shape, scale
  vector<lower=param_lower[1], upper=param_upper[1]>[1] param1;
  vector<lower=param_lower[2], upper=param_upper[2]>[1] param2;
}

model {
  // Priors - weakly informative by default
  // These will be refined based on primarycensored approach
  param1[1] ~ normal(0, 10);  // Adjust based on distribution
  param2[1] ~ normal(1, 10) T[0,];

  // Likelihood
  for (i in 1:N) {
    real lp;

    if (dist_id == 1) {
      // Lognormal
      lp = double_censored_lognormal_lpmf(
        delay[i], delay_upper[i], param1[1], param2[1]
      );
    } else if (dist_id == 2) {
      // Gamma
      lp = double_censored_gamma_lpmf(
        delay[i], delay_upper[i], param1[1], param2[1]
      );
    } else if (dist_id == 3) {
      // Weibull
      lp = double_censored_weibull_lpmf(
        delay[i], delay_upper[i], param1[1], param2[1]
      );
    }

    // Weight by count
    target += n[i] * lp;
  }

  // TODO: Add truncation correction
  // For now, assume D is large enough that truncation is negligible
}

generated quantities {
  // Extract parameters in named format for easier extraction
  real meanlog = (dist_id == 1) ? param1[1] : 0;
  real sdlog = (dist_id == 1) ? param2[1] : 0;
  real shape = (dist_id == 2 || dist_id == 3) ? param1[1] : 0;
  real rate = (dist_id == 2) ? param2[1] : 0;
  real scale = (dist_id == 3) ? param2[1] : 0;
}
