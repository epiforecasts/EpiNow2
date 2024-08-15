/**
 * Update a vector of effective reproduction numbers (Rt) based on
 * an intercept, breakpoints (i.e. a random walk), and a Gaussian
 * process.
 *
 * @param t Length of the time series
 * @param log_R Logarithm of the base reproduction number
 * @param noise Vector of Gaussian process noise values
 * @param bps Array of breakpoint indices
 * @param bp_effects Vector of breakpoint effects
 * @param stationary Flag indicating whether the Gaussian process is stationary 
 * (1) or non-stationary (0)
 * @return A vector of length t containing the updated Rt values
 */
vector update_Rt(int t, real log_R, vector noise, array[] int bps,
                 vector bp_effects, int stationary) {
  int bp_n = num_elements(bp_effects);
  int gp_n = num_elements(noise);
  
  vector[t] R = rep_vector(log_R, t);
  
  if (bp_n) {
    vector[bp_n + 1] bp0;
    bp0[1] = 0;
    bp0[2:(bp_n + 1)] = cumulative_sum(bp_effects);
    R = R + bp0[bps];
  }
  
  if (gp_n) {
    vector[t] gp;
    if (stationary) {
      gp[1:gp_n] = noise;
      if (t > gp_n) {
        gp[(gp_n + 1):t] = rep_vector(noise[gp_n], t - gp_n);
      }
    } else {
      gp[1] = 0;
      gp[2:(gp_n + 1)] = noise;
      gp = cumulative_sum(gp);
    }
    R = R + gp;
  }
  
  return exp(R);
}

/**
 * Calculate the log-probability of the reproduction number (Rt) priors
 *
 * @param log_R Logarithm of the base reproduction number
 * @param initial_infections Array of initial infection values
 * @param initial_growth Array of initial growth rates
 * @param bp_effects Vector of breakpoint effects
 * @param bp_sd Array of breakpoint standard deviations
 * @param bp_n Number of breakpoints
 * @param seeding_time Time point at which seeding occurs
 * @param r_logmean Log-mean of the prior distribution for the base reproduction number
 * @param r_logsd Log-standard deviation of the prior distribution for the base reproduction number
 * @param prior_infections Prior mean for initial infections
 * @param prior_growth Prior mean for initial growth rates
 */
void rt_lp(vector log_R, array[] real initial_infections, array[] real initial_growth,
           vector bp_effects, array[] real bp_sd, int bp_n, int seeding_time,
           real r_logmean, real r_logsd, real prior_infections,
           real prior_growth) {
  log_R ~ normal(r_logmean, r_logsd);
  
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
  
  initial_infections ~ normal(prior_infections, 0.2);
  
  if (seeding_time > 1) {
    initial_growth ~ normal(prior_growth, 0.2);
  }
}
