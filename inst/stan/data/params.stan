int<lower = 0> n_params_variable; // number of parameters
int<lower = 0> n_params_fixed; // number of parameters
vector[n_params_variable] params_lower; // lower bounds of the priors
vector[n_params_variable] params_upper; // upper bounds of the priors
// 1 for an init-anchored state's level: its prior applies to the derived
// initial value (with a Jacobian) instead, so the level itself is left free.
array[n_params_variable] int<lower = 0, upper = 1> params_skip_state_init_prior;

// fixed parameter lookup
array[n_params_fixed + n_params_variable] int<lower = 0> params_fixed_lookup;
// variable parameter lookup
array[n_params_fixed + n_params_variable] int<lower = 0> params_variable_lookup;

vector[n_params_fixed] params_value; // fixed parameter values

// 0 = lognormal; 1 = gamma; 2 = normal
array[n_params_variable] int<lower = 0> prior_dist;
// number of parameters across all parametric delay distributions
int<lower = 0> prior_dist_params_length;
vector[prior_dist_params_length] prior_dist_params;
