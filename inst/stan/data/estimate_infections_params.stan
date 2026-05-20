int<lower = 0> param_id_alpha; // parameter id of alpha (GP magnitude)
int<lower = 0> param_id_rho; // parameter id of rho (GP lengthscale)
int<lower = 0> param_id_R0; // parameter id of R0
int<lower = 0> param_id_fraction_observed; // parameter id of fraction_observed
int<lower = 0> param_id_reporting_overdispersion; // parameter id of reporting_overdispersion
int<lower = 0> param_id_pop; // parameter id of pop

// Init priors for centred-GP-wrapped parameters. Today only R0 is wrapped
// (its prior in rt_opts() is on the *initial* Rt, applied to the derived
// R[1] from the centred GP). Generic shape so additional time-varying
// parameters can be added without changing the data plumbing.
int<lower = 0> n_init_priors;
array[n_init_priors] int<lower = 1> init_param_ids;
array[n_init_priors] int<lower = 0, upper = 2> init_dists;
vector[2 * n_init_priors] init_dist_params;
