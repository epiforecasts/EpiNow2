  int t;                                            // unobserved time
  int seeding_time;                                 // time period used for seeding and not observed
  int horizon;                                      // forecast horizon
  int future_time;                                  // time in future for Rt
  int<lower = 0> cases[t - horizon - seeding_time]; // observed cases
  vector<lower = 0>[t] shifted_cases;               // prior infections (for backcalculation)
