  int t;                                            // unobserved time
  int lt;                          // timepoints in the likelihood
  int seeding_time;                                 // time period used for seeding and not observed
  int horizon;                                      // forecast horizon
  int future_time;                                  // time in future for Rt
  array[lt] int<lower = 0> cases; // observed cases
  array[lt] int cases_time; // time of observed cases
  vector<lower = 0>[t] shifted_cases;               // prior infections (for backcalculation)
