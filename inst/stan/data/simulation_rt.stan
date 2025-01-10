  array[n, 1] real initial_infections; // initial logged infections

  matrix[n, t - seeding_time] R; // reproduction number
  int pop;                       // susceptible population

  int<lower = 0> gt_id; // id of generation time
