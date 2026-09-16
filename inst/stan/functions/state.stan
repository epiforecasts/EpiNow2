/**
 * Time-varying parameter states
 *
 * Build a parameter trajectory by combining a baseline (level) with a
 * stochastic deviation: a random walk (`rw_trajectory`) or an approximate
 * Gaussian process (`gp_trajectory`). Each builder is self-contained;
 * `get_state_trajectory` is a thin shell that dispatches to the right one for a
 * given parameter, or returns a constant trajectory when the parameter is not
 * time-varying.
 *
 * Three windows describe a trajectory:
 *  - `t`: total length (observation window + forecast horizon);
 *  - `n_free`: the window over which the state varies freely; it holds its last
 *    value constant from `n_free + 1` to `t`. Set by the `future` setting
 *    ("latest" fixes at the observation end, "project" extends over the whole
 *    horizon);
 *  - `n_centre`: the leading window over which an init-anchored (non-stationary)
 *    state is centred for identifiability. This is the observation window, so
 *    centring is invariant to how far the state is projected.
 *
 * Components combine additively on the link scale; the trajectory is returned on
 * the natural scale (the inverse link is applied here, e.g. `exp` for a log
 * link).
 *
 * @ingroup estimates_smoothing
 */

/**
 * Build a random-walk trajectory for a time-varying parameter
 *
 * The walk is the cumulative sum of `steps`, expanded so each step applies to a
 * block of `period` time points, centred over the observation window
 * (`n_centre`) so the level is identifiable, then held constant beyond the free
 * window (`n_free`) through the forecast horizon.
 *
 * @param t Total trajectory length
 * @param n_free Window over which the walk varies (holds its last value after)
 * @param n_centre Leading window used to centre the walk for identifiability
 * @param level Baseline parameter value on the natural scale
 * @param steps Random walk steps (one per period block, less one)
 * @param link Link function (0 = log)
 * @param period Number of time points between random walk steps
 * @return A vector of length t with the parameter trajectory (natural scale)
 *
 * @ingroup estimates_smoothing
 */
vector rw_trajectory(int t, int n_free, int n_centre, real level, vector steps,
                     int link, int period) {
  real intercept = link == 0 ? log(level) : level;
  vector[t] x = rep_vector(intercept, t);
  int n_steps = num_elements(steps);
  if (n_steps > 0) {
    vector[n_steps + 1] cum;
    cum[1] = 0;
    cum[2:(n_steps + 1)] = cumulative_sum(steps);
    // expand each step to a block of `period` time points over the free window
    vector[n_free] walk;
    for (i in 1:n_free) {
      walk[i] = cum[(i - 1) %/% period + 1];
    }
    // centre over the observation window for identifiability
    walk -= mean(walk[1:n_centre]);
    x[1:n_free] += walk;
    if (t > n_free) {
      x[(n_free + 1):t] = rep_vector(x[n_free], t - n_free); // hold last value
    }
  }
  return link == 0 ? exp(x) : x;
}

/**
 * Build a Gaussian process trajectory for a time-varying parameter
 *
 * For the `mean` anchor (`anchor = 0`) the GP is stationary (mean-reverting
 * around the level). For the `init` anchor (`anchor = 1`) the GP models the
 * increments, so the deviation is the cumulative sum of the GP noise, centred
 * over the observation window (`n_centre`) for identifiability. The GP varies
 * over the free window (`n_free`) and holds its last value through the remainder
 * of the trajectory. GP noise is supplied directly (computed via update_gp).
 *
 * @param t Total trajectory length
 * @param n_free Window over which the GP varies (holds its last value after)
 * @param n_centre Leading window used to centre an init-anchored GP
 * @param level Baseline parameter value on the natural scale
 * @param noise Gaussian process noise (length n_free)
 * @param link Link function (0 = log)
 * @param anchor 0 = mean (stationary), 1 = init (non-stationary)
 * @return A vector of length t with the parameter trajectory (natural scale)
 *
 * @ingroup estimates_smoothing
 */
vector gp_trajectory(int t, int n_free, int n_centre, real level, vector noise,
                     int link, int anchor) {
  real intercept = link == 0 ? log(level) : level;
  vector[n_free] dev;
  if (anchor == 0) {
    dev = noise; // stationary (mean-reverting)
  } else {
    dev = cumulative_sum(noise); // non-stationary (GP on increments)
    dev -= mean(dev[1:n_centre]); // centre over the observation window
  }
  vector[t] x;
  x[1:n_free] = intercept + dev;
  if (t > n_free) {
    x[(n_free + 1):t] = rep_vector(x[n_free], t - n_free); // hold last value
  }
  return link == 0 ? exp(x) : x;
}

/**
 * Get the trajectory of a (possibly time-varying) parameter
 *
 * Thin dispatch over the registered states: if a state is attached to the
 * parameter with the given id, builds its trajectory (random walk or Gaussian
 * process); otherwise returns a constant trajectory at `level`. This lets any
 * parameter consumed pointwise over time become time-varying with no
 * per-parameter code beyond the call site.
 *
 * Each state carries its own free-noise window and, for GP states, its own
 * basis (built here from that window); states share nothing beyond the flat
 * coefficient vectors indexed by per-state offsets.
 *
 * @param id Target parameter id
 * @param t Total trajectory length (observation window + forecast horizon)
 * @param level Parameter level on the natural scale (from get_param)
 * @param state_param_id Target parameter id of each state
 * @param state_type State type of each state (0 = RW, 1 = GP)
 * @param state_link Link of each state (0 = log)
 * @param state_pos Index of each state within its type group
 * @param state_anchor Anchor of each state (0 = mean, 1 = init)
 * @param state_n_free Free-noise window of each state (holds its last value
 *   through the remaining forecast horizon)
 * @param state_n_centre Leading window used to centre an init-anchored state
 * @param state_rw_steps Flat random walk steps across RW states
 * @param state_rw_n Number of random walk steps of each state
 * @param state_rw_offset Offset of each state into state_rw_steps
 * @param state_rw_period Number of time steps between random walk steps
 * @param state_gp_eta Flat GP basis coefficients across GP states
 * @param state_gp_M Number of GP basis functions of each state
 * @param state_gp_offset Offset of each state into state_gp_eta
 * @param gp_boundary_scale GP boundary scale of each GP state
 * @param gp_kernel Kernel of each GP state
 * @param gp_nu Matern smoothness of each GP state
 * @param state_gp_alpha GP magnitude of each GP state
 * @param state_gp_rho GP lengthscale of each GP state
 * @return A vector of length t with the parameter trajectory
 *
 * @ingroup estimates_smoothing
 */
vector get_state_trajectory(
  int id, int t, real level,
  array[] int state_param_id, array[] int state_type, array[] int state_link,
  array[] int state_pos, array[] int state_anchor,
  array[] int state_n_free, array[] int state_n_centre,
  vector state_rw_steps, array[] int state_rw_n, array[] int state_rw_offset,
  int state_rw_period,
  vector state_gp_eta, array[] int state_gp_M, array[] int state_gp_offset,
  array[] real gp_boundary_scale, array[] int gp_kernel, array[] real gp_nu,
  vector state_gp_alpha, vector state_gp_rho
) {
  for (s in 1:num_elements(state_param_id)) {
    if (state_param_id[s] == id) {
      int nf = state_n_free[s];
      int nc = state_n_centre[s];
      if (state_type[s] == 0) {
        vector[state_rw_n[s]] steps = segment(
          state_rw_steps, state_rw_offset[s] + 1, state_rw_n[s]
        );
        return rw_trajectory(
          t, nf, nc, level, steps, state_link[s], state_rw_period
        );
      } else {
        int p = state_pos[s];
        int M = state_gp_M[s];
        vector[M] eta = segment(state_gp_eta, state_gp_offset[s] + 1, M);
        // each GP state builds its own basis from its own window; the basis is
        // data-only, so it could be hoisted to transformed data if profiling
        // shows it matters
        matrix[nf, M] phi = setup_gp(M, gp_boundary_scale[p], nf, 0, 1.0);
        vector[nf] noise = update_gp(
          phi, M, gp_boundary_scale[p], state_gp_alpha[p],
          2 * state_gp_rho[p] / nf, eta, gp_kernel[p], gp_nu[p]
        );
        return gp_trajectory(
          t, nf, nc, level, noise, state_link[s], state_anchor[s]
        );
      }
    }
  }
  return rep_vector(level, t);
}
