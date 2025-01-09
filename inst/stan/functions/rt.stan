/**
 * Update a vector of effective reproduction numbers (Rt) based on
 * an intercept, breakpoints (i.e. a random walk), and a Gaussian
 * process.
 *
 * @param t Length of the time series
 * @param R0 Initial reproduction number
 * @param noise Vector of Gaussian process noise values
 * @param bps Array of breakpoint indices
 * @param bp_effects Vector of breakpoint effects
 * @param stationary Flag indicating whether the Gaussian process is stationary 
 * (1) or non-stationary (0)
 * @return A vector of length t containing the updated Rt values
 */
vector update_Rt(int t, real R0, vector noise, array[] int bps,
                 vector bp_effects, int stationary) {
  // define control parameters
  int bp_n = num_elements(bp_effects);
  int gp_n = num_elements(noise);
  // initialise intercept
  vector[t] logR = rep_vector(log(R0), t);
  //initialise breakpoints + rw
  if (bp_n) {
    vector[bp_n + 1] bp0;
    bp0[1] = 0;
    bp0[2:(bp_n + 1)] = cumulative_sum(bp_effects);
    logR = logR + bp0[bps];
  }
  //initialise gaussian process
  if (gp_n) {
    vector[t] gp = rep_vector(0, t);
    if (stationary) {
      gp[1:gp_n] = noise;
      // fix future gp based on last estimated
      if (t > gp_n) {
        gp[(gp_n + 1):t] = rep_vector(noise[gp_n], t - gp_n);
      }
    } else {
      gp[2:(gp_n + 1)] = noise;
      gp = cumulative_sum(gp);
    }
    logR = logR + gp;
  }
  
  return exp(logR);
}

/**
 * Calculate the log-probability of the reproduction number (Rt) priors
 *
 * @param initial_infections Array of initial infection values
 * @param bp_effects Vector of breakpoint effects
 * @param bp_sd Array of breakpoint standard deviations
 * @param bp_n Number of breakpoints
 * @param prior_infections Prior mean for initial infections
 */
void rt_lp(array[] real initial_infections, vector bp_effects,
           array[] real bp_sd, int bp_n, real prior_infections) {
  //breakpoint effects on Rt
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
  // initial infections
  initial_infections ~ normal(prior_infections, sqrt(prior_infections));
  
}
