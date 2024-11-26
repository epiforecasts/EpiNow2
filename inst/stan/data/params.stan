int<lower = 0> n_params_variable;         // number of parameters
int<lower = 0> n_params_fixed;         // number of parameters
vector[n_params_variable] params_lower;   // lower bounds of the priors
vector[n_params_variable] params_upper;   // upper bounds of the priors

array[n_params_fixed + n_params_variable] int<lower = 0> params_fixed_lookup;   // fixed parameter lookup
array[n_params_fixed + n_params_variable] int<lower = 0> params_variable_lookup;   // variable parameter lookup

vector[n_params_fixed] params_value;   // fixed parameter values

array[n_params_variable] int<lower = 0> prior_dist; // 0 = lognormal; 1 = gamma; 2 = normal
int<lower = 0> prior_dist_params_length;            // number of parameters across all parametric delay distributions
vector[prior_dist_params_length] prior_dist_params;
