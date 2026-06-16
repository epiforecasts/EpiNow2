// Time-varying parameter states (see functions/state.stan).
int<lower = 0> n_states; // number of time-varying parameters
array[n_states] int<lower = 1> state_param_id; // target parameter id
array[n_states] int<lower = 0> state_type; // 0 = random walk
array[n_states] int<lower = 0> state_link; // 0 = log
// step standard deviation prior: 0 = lognormal, 1 = gamma, 2 = normal
array[n_states] int<lower = 0> state_sd_dist;
vector[2 * n_states] state_sd_dist_params; // two prior parameters each
