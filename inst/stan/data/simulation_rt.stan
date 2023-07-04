  real initial_infections[seeding_time ? n : 0, 1]; // initial logged infections
  real initial_growth[seeding_time > 1 ? n : 0, 1]; //initial growth
  vector[n] R[t - seeding_time]; // reproduction number
  int pop;                       // susceptible population

  int<lower = 0> gt_id; // id of generation time
