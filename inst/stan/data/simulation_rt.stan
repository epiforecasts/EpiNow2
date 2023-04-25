  real initial_infections[seeding_time ? n : 0, 1]; // initial logged infections
  real initial_growth[seeding_time > 1 ? n : 0, 1]; //initial growth

  matrix[n, t - seeding_time] R; // reproduction number
  int pop;                       // susceptible population
