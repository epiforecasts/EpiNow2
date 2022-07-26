  int estimate_r;                    // should the reproduction no be estimated (1 = yes)
  real prior_infections;             // prior for initial infections
  real prior_growth;                 // prior on initial growth rate
  int future_fixed;                  // is underlying future Rt assumed to be fixed
  int fixed_from;                    // Reference date for when Rt estimation should be fixed
  int pop;                           // Initial susceptible population
  int<lower = 0> gt_id;              // id of generation time
