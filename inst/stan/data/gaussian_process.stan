// Gaussian process states (approximate Hilbert space GP; each state has its own
// basis sized to its own free-noise window). Pairs with the Gaussian process
// functions in functions/gaussian_process.stan.
int<lower = 0> n_gp_states;
array[n_gp_states] real<lower = 0> gp_basis_prop; // basis proportion per state
array[n_gp_states] real<lower = 0> gp_boundary_scale; // boundary scale L per state
array[n_gp_states] int<lower = 0> gp_kernel; // 0 = SE, 2 = Matern
array[n_gp_states] real gp_nu; // Matern smoothness
array[n_gp_states] int<lower = 1> gp_alpha_id; // parameter id of each GP magnitude
array[n_gp_states] int<lower = 1> gp_rho_id; // parameter id of each GP lengthscale
