int<lower = 0> param_id_alpha; // parameter id of alpha (GP magnitude)
int<lower = 0> param_id_rho; // parameter id of rho (GP lengthscale)
int<lower = 0> param_id_R0; // parameter id of R0
int<lower = 0> param_id_fraction_observed; // parameter id of fraction_observed
int<lower = 0> param_id_reporting_overdispersion; // parameter id of reporting_overdispersion
int<lower = 0> param_id_pop; // parameter id of pop

// Priors on the initial value of a centred-GP-wrapped trajectory, applied
// to the derived initial value with a change of variables. Ragged layout
// mirrors prior_dist_params: init_dist_params is a flat vector and the
// per-prior dispatch in estimate_infections.stan advances an offset by the
// appropriate count.
int<lower = 0> n_init_priors;
array[n_init_priors] int<lower = 1> init_param_ids;
array[n_init_priors] int<lower = 0> init_dists;
vector[n_init_priors] init_lower;
vector[n_init_priors] init_upper;
int<lower = 0> init_dist_params_length;
vector[init_dist_params_length] init_dist_params;
