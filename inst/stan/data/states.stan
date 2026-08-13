// Time-varying parameter states (see functions/state.stan).
int<lower = 0> n_states; // number of time-varying parameters
array[n_states] int<lower = 1> state_param_id; // target parameter id
array[n_states] int<lower = 0> state_type; // 0 = random walk, 1 = gaussian process
array[n_states] int<lower = 0> state_link; // 0 = log
array[n_states] int<lower = 1> state_pos; // index within its type group
array[n_states] int<lower = 0> state_anchor; // 0 = mean, 1 = init
// init-anchor prior on the derived initial value (applied with a Jacobian)
array[n_states] int<lower = 0> state_init_dist;
vector[2 * n_states] state_init_dist_params;
vector[n_states] state_init_lower;
vector[n_states] state_init_upper;
// forecast-horizon behaviour, per state: if state_future_fixed the state is held
// constant from `state_future_from` (relative to the observation end) onwards;
// otherwise it varies over the whole horizon ("project")
array[n_states] int<lower = 0, upper = 1> state_future_fixed;
array[n_states] int state_future_from;

// random walk states
int<lower = 0> n_rw_states;
// step standard deviation prior: 0 = lognormal, 1 = gamma, 2 = normal
array[n_rw_states] int<lower = 0> rw_sd_dist;
vector[2 * n_rw_states] rw_sd_dist_params; // two prior parameters each
int<lower = 1> state_rw_period; // time steps between random walk steps

// gaussian process states (approximate Hilbert space GP; each state has its own
// basis sized to its own free-noise window)
int<lower = 0> n_gp_states;
array[n_gp_states] real<lower = 0> gp_basis_prop; // basis proportion per state
array[n_gp_states] real<lower = 0> gp_boundary_scale; // boundary scale L per state
array[n_gp_states] int<lower = 0> gp_kernel; // 0 = SE, 2 = Matern
array[n_gp_states] real gp_nu; // Matern smoothness
array[n_gp_states] int<lower = 0> gp_alpha_dist; // magnitude prior
vector[2 * n_gp_states] gp_alpha_dist_params;
array[n_gp_states] int<lower = 0> gp_rho_dist; // lengthscale prior
vector[2 * n_gp_states] gp_rho_dist_params;
