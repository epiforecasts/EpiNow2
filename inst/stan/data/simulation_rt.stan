  real initial_infections[seeding_time ? n : 0, 1]; // initial logged infections
  real initial_growth[seeding_time > 1 ? n : 0, 1]; //initial growth
  real<lower = 0> gt_mean[n, 1];  // mean of generation time
  real<lower = 0> gt_sd[n, 1];   // sd of generation time
  int max_gt;                    // maximum generation time
  matrix[n, t - seeding_time] R; // reproduction number
  int pop;                       // susceptible population
