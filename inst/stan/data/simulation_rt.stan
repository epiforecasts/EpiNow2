array[n, 1] real initial_infections; // initial logged infections
int initial_as_scale; // whether to interpret initial infections as scaling

matrix[n, t - seeding_time] R; // reproduction number
int pop; // susceptible population

int<lower = 0> gt_id; // id of generation time

int<lower = 0, upper = 1> growth_method; // method to compute growth rate (0 = infections, 1 = infectiousness)

