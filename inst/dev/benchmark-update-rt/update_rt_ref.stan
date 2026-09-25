// update_Rt() as on main before the rewrite, kept as the benchmark reference
vector update_Rt_ref(int t, real R0, vector noise, array[] int bps,
                     vector bp_effects, int stationary, int n_centre) {
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
    vector[t] bp = bp0[bps];
    // Centre over the observation window (same identifiability fix as the GP below).
    bp -= mean(bp[1:n_centre]);
    logR = logR + bp;
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
      // Centre over the observation window (same identifiability fix as the BP above).
      gp -= mean(gp[1:n_centre]);
    }
    logR = logR + gp;
  }

  return exp(logR);
}
