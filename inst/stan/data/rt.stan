  int estimate_r;                    // should the reproduction no be estimated (1 = yes)
  real prior_infections;             // prior for initial infections
  real prior_growth;                 // prior on initial growth rate
  real <lower = 0> r_mean;           // prior mean of reproduction number
  real <lower = 0> r_sd;             // prior standard deviation of reproduction number
  int bp_n;                          // no of breakpoints (0 = no breakpoints)
  array[t - seeding_time] int breakpoints; // when do breakpoints occur
  int future_fixed;                  // is underlying future Rt assumed to be fixed
  int fixed_from;                    // Reference date for when Rt estimation should be fixed
  int pop;                           // Initial susceptible population
  int<lower = 0> gt_id;              // id of generation time
  int incidence_feedback_used;           // should incidence feedback be used (1 = yes)
  real incidence_feedback_mean;     // mean of incidence feedback
  real incidence_feedback_sd;       // sd of incidence feedback

