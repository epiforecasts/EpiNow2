  int estimate_r;                    // should the reproduction no be estimated (1 = yes)
  real initial_infections_estimate;  // point estimate of initial infections
  real initial_growth_estimate;      // point estimate of initial growth rate
  int bp_n;                          // no of breakpoints (0 = no breakpoints)
  array[t - seeding_time] int breakpoints; // when do breakpoints occur
  int future_fixed;                  // is underlying future Rt assumed to be fixed
  int fixed_from;                    // Reference date for when Rt estimation should be fixed
  int pop;                           // Initial susceptible population
  int<lower = 0> gt_id;              // id of generation time
