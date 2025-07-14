int t; // unobserved time
int lt; // timepoints in the likelihood
int it; // imputed time points
int seeding_time; // time period used for seeding and not observed
int horizon; // forecast horizon
int future_time; // time in future for Rt
array[lt] int<lower = 0> cases; // observed cases
array[lt] int case_times; // time of observed cases
array[it] int imputed_times; // time of imputed cases
int any_accumulate; // Should any missing values be accumulated?
// Should missing values be accumulated (by time)
array[t - seeding_time] int accumulate;
