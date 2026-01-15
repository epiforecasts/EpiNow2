int estimate_r; // should the reproduction no be estimated (1 = yes)
int bp_n; // no of breakpoints (0 = no breakpoints)
array[t - seeding_time] int breakpoints; // when do breakpoints occur
int future_fixed; // is underlying future Rt assumed to be fixed
int fixed_from; // Reference date for when Rt estimation should be fixed
int use_pop; // use population size (0 = no; 1 = forecasts; 2 = all)
real<lower = 0> pop_floor; // Minimum susceptible population (numerical stability floor)
int<lower = 0> delay_id_generation_time; // id of generation time
int<lower = 0, upper = 1> growth_method; // method to compute growth rate (0 = infections, 1 = infectiousness)
