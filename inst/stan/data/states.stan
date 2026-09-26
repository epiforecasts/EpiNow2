// Time-varying states (see functions/state.stan).
//
// A state's hyperparameters (random-walk step standard deviation, GP magnitude
// and lengthscale) are ordinary entries in the parameter vector (data and
// functions in params.stan): the arrays below describe only the state structure
// and reference those hyperparameters by parameter id. The prior on an
// init-anchored state's initial value is the level parameter's own prior,
// applied to the derived initial value with a Jacobian (see the model block).
int<lower = 0> n_states; // number of time-varying parameters
array[n_states] int<lower = 1> state_param_id; // target parameter id
array[n_states] int<lower = 0> state_link; // 0 = log
array[n_states] int<lower = 0> state_anchor; // 0 = mean, 1 = init
// forecast-horizon behaviour, per state: if state_future_fixed the state is held
// constant from `state_future_from` (relative to the observation end) onwards;
// otherwise it varies over the whole horizon ("project")
array[n_states] int<lower = 0, upper = 1> state_future_fixed;
array[n_states] int state_future_from;

// A state's trajectory is the sum of one or more components (random walks and
// Gaussian processes), so each state indexes a contiguous block of the component
// table below (CSR layout: components of state s are
// state_comp_offset[s] + 1 .. state_comp_offset[s] + state_comp_n[s]).
array[n_states] int<lower = 0> state_comp_offset;
array[n_states] int<lower = 0> state_comp_n;

// component table
int<lower = 0> n_components; // total components across all states
array[n_components] int<lower = 0> comp_type; // 0 = random walk, 1 = gaussian process
array[n_components] int<lower = 1> comp_pos; // index within its type group
