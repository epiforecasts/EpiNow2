array[seeding_time ? n : 0, 1] real initial_infections_samples; // initial logged infections
array[seeding_time > 1 ? n : 0, 1] real initial_growth_samples; //initial growth

array[n, t - seeding_time] real R_samples; // reproduction number
int pop;                       // susceptible population

int<lower = 0> gt_id; // id of generation time
