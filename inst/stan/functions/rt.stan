// update a vector of Rts
vector update_Rt(vector input_R, real log_R, vector noise, int[] bps,
                 real[] bp_effects, int stationary) {
  // define control parameters
  int t = num_elements(input_R);
  int bp_n = num_elements(bp_effects);
  int bp_c = 0;
  int gp_n = num_elements(noise);
  // define result vectors
  vector[t] bp = rep_vector(0, t);
  vector[t] gp = rep_vector(0, t);
  vector[t] R;
  // initialise breakpoints
  if (bp_n) {
    for (s in 1:t) {
      if (bps[s]) {
        bp_c += bps[s];
        bp[s] = bp_effects[bp_c];
      }
    }
    bp = cumulative_sum(bp);
  }
  //initialise gaussian process
  if (gp_n) {
    if (stationary) {
      gp[1:gp_n] = noise;
      // fix future gp based on last estimated
      if (t > gp_n) {
        gp[(gp_n + 1):t] = rep_vector(noise[gp_n], t - gp_n);
      }
    }else{
      gp[2:(gp_n + 1)] = noise;
      gp = cumulative_sum(gp);
    }
  }
  // Calculate Rt
  R = rep_vector(log_R, t) + bp + gp;
  R = exp(R);
  return(R);
}
// Rt priors
void rt_lp(vector log_R, real[] initial_infections, real[] initial_growth,
           real[] bp_effects, real[] bp_sd, int bp_n, int seeding_time,
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
