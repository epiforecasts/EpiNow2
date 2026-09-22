// Time-varying parameter states (see functions/state.stan).
//
// A state's hyperparameters (random-walk step standard deviation, GP magnitude
// and lengthscale) are ordinary entries in the parameter vector (data and
// functions in params.stan): the arrays below describe only the state structure
// and reference those hyperparameters by parameter id. The prior on an
// init-anchored state's initial value is the level parameter's own prior,
// applied to the derived initial value with a Jacobian (see the model block).
int<lower = 0> n_states; // number of time-varying parameters
array[n_states] int<lower = 1> state_param_id; // target parameter id
array[n_states] int<lower = 0> state_type; // 0 = random walk, 1 = gaussian process
array[n_states] int<lower = 0> state_link; // 0 = log
array[n_states] int<lower = 1> state_pos; // index within its type group
array[n_states] int<lower = 0> state_anchor; // 0 = mean, 1 = init
// forecast-horizon behaviour, per state: if state_future_fixed the state is held
// constant from `state_future_from` (relative to the observation end) onwards;
// otherwise it varies over the whole horizon ("project")
array[n_states] int<lower = 0, upper = 1> state_future_fixed;
array[n_states] int state_future_from;

// random walk states
int<lower = 0> n_rw_states;
array[n_rw_states] int<lower = 1> rw_sd_id; // parameter id of each step sd
int<lower = 1> state_rw_period; // time steps between random walk steps

// gaussian process states (approximate Hilbert space GP; each state has its own
// basis sized to its own free-noise window)
int<lower = 0> n_gp_states;
array[n_gp_states] real<lower = 0> gp_basis_prop; // basis proportion per state
array[n_gp_states] real<lower = 0> gp_boundary_scale; // boundary scale L per state
array[n_gp_states] int<lower = 0> gp_kernel; // 0 = SE, 2 = Matern
array[n_gp_states] real gp_nu; // Matern smoothness
array[n_gp_states] int<lower = 1> gp_alpha_id; // parameter id of each GP magnitude
array[n_gp_states] int<lower = 1> gp_rho_id; // parameter id of each GP lengthscale
