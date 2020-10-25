  int estimate_r;                    // should the reproduction no be estimated (1 = yes)
  real <lower = 0> r_mean;           // prior mean of reproduction number
  real <lower = 0> r_sd;             // prior standard deviation of reproduction number
  int bp_n;                      // no of breakpoints (0 = no breakpoints)
  int breakpoints[rt];               // when do breakpoints occur 
  int future_fixed;                 // is underlying future Rt assumed to be fixed
  int fixed_from;                   // Reference date for when Rt estimation should be fixed
  