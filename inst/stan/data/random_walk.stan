// Random walk states. The step standard deviation is an ordinary parameter
// (data and functions in params.stan); the arrays below reference it by
// parameter id.
int<lower = 0> n_rw_states;
array[n_rw_states] int<lower = 1> rw_sd_id; // parameter id of each step sd
int<lower = 1> state_rw_period; // time steps between random walk steps
