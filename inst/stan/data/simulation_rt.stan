  array[n, 1] real initial_infections; // initial logged infections
  array[n, seeding_time > 1 ? 1 : 0] real initial_growth; //initial growth

  matrix[n, t - seeding_time] R; // reproduction number
  int use_pop;                       // use population size (0 = no; 1 = forecasts)

  int<lower = 0> gt_id; // id of generation time
