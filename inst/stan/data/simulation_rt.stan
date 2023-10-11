  array[seeding_time ? n : 0, 1] real initial_infections; // initial logged infections
  array[seeding_time > 1 ? n : 0, 1] real initial_growth; //initial growth

  matrix[n, t - seeding_time] R; // reproduction number
  int pop;                       // susceptible population

  int<lower = 0> gt_id; // id of generation time
  int incidence_feedback_used; // 0 = no incidence feedback, 1 = incidence feedback
  array[incidence_feedback_used] real incidence_feedback; // incidence feedback
