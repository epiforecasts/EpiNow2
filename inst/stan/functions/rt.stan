// update a vector of Rts
vector update_Rt(int t, real log_R, vector noise, array[] int bps,
                 vector bp_effects, int stationary) {
  // define control parameters
  int bp_n = num_elements(bp_effects);
  int gp_n = num_elements(noise);
  // Set up Rt intercept
  vector[t] R = rep_vector(log_R, t);
  
  // initialise breakpoints
  if (bp_n) {
    vector[bp_n + 1] bp0;
    bp0[1] = 0;
    bp0[2:(bp_n + 1)] = bp_effects;
    R = R + cumulative_sum(bp0[bps]);
  }
  //initialise gaussian process
  if (gp_n) {
    vector[t] gp;
    if (stationary) {
      gp[1:gp_n] = noise;
      // fix future gp based on last estimated
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
  return(exp(R));
}
// Rt priors
void rt_lp(vector log_R, array[] real initial_infections, array[] real initial_growth,
           vector bp_effects, array[] real bp_sd, int bp_n, int seeding_time,
           real r_logmean, real r_logsd, real prior_infections,
           real prior_growth) {
  // prior on R
  log_R ~ normal(r_logmean, r_logsd);
  //breakpoint effects on Rt
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
  // initial infections
  initial_infections ~ normal(prior_infections, 0.2);
  if (seeding_time > 1) {
    initial_growth ~ normal(prior_growth, 0.2);
  }
}
