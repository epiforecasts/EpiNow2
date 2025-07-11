int estimate_r; // should the reproduction no be estimated (1 = yes)
int bp_n; // no of breakpoints (0 = no breakpoints)
array[t - seeding_time] int breakpoints; // when do breakpoints occur
int future_fixed; // is underlying future Rt assumed to be fixed
int fixed_from; // Reference date for when Rt estimation should be fixed
int pop; // Initial susceptible population
int<lower = 0> gt_id; // id of generation time
int<lower = 0, upper = 1> growth_method; // method to compute growth rate (0 = infections, 1 = infectiousness)
