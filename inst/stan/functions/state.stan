/**
 * Time-varying states
 *
 * Terminology follows state-space modelling: a *parameter* is a scalar that is
 * constant over time (it may still be estimated), whereas a *state* is a
 * quantity that varies over time. A parameter becomes a state when the user
 * requests it with `RW()` or `GP()`; the parameter's value is then the state's
 * baseline (level), and the state's own hyperparameters (step sd, GP magnitude
 * and lengthscale) are themselves ordinary parameters (see params.stan).
 *
 * Build a parameter trajectory by combining a baseline (level) with a
 * stochastic deviation: a random walk (`rw_dev`) or an approximate Gaussian
 * process (`gp_dev`), one per component (`component_dev`). Components share
 * `assemble_state` to combine their summed deviation with the baseline, hold
 * the last value through the forecast horizon, and return on the natural
 * scale. `get_state_trajectory` is a thin shell that dispatches to the right
 * components for a given parameter, or returns a constant trajectory when the
 * parameter is not time-varying.
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
 * Continue a positive trajectory at its latest growth rate
 *
 * Replaces the values after `n_free` (held constant by `assemble_state`)
 * with a log-linear continuation of the last increment, so the trajectory
 * keeps growing or declining at its latest rate over the forecast horizon.
 *
 * @param x Trajectory on the natural scale (positive)
 * @param n_free Last time point at which the trajectory varies freely
 * @return A vector of the same length as `x`
 *
 * @ingroup estimates_smoothing
 */
vector continue_growth(vector x, int n_free) {
  int t = num_elements(x);
  vector[t] y = x;
  if (t > n_free && n_free > 1) {
    real r = log(x[n_free]) - log(x[n_free - 1]);
    y[(n_free + 1):t] =
      x[n_free] * exp(r * linspaced_vector(t - n_free, 1, t - n_free));
  }
  return y;
}

/**
 * Assemble a trajectory from a baseline and a link-scale deviation
 *
 * Shared by every state trajectory: adds the deviation `dev` (length
 * `n_free`, on the link scale) to the baseline over the free window,
 * holds the last value constant through the remaining forecast horizon, and
 * returns on the natural scale (applying the inverse link).
 *
 * @param t Total trajectory length
 * @param n_free Window over which the state varies (holds its last value after)
 * @param level Baseline parameter value on the natural scale
 * @param dev Link-scale deviation over the free window (length n_free)
 * @param link Link function (0 = log)
 * @return A vector of length t with the parameter trajectory (natural scale)
 *
 * @ingroup estimates_smoothing
 */
vector assemble_state(int t, int n_free, real level, vector dev, int link) {
  real intercept = link == 0 ? log(level) : level;
  vector[t] x;
  x[1:n_free] = intercept + dev;
  if (t > n_free) {
    x[(n_free + 1):t] = rep_vector(x[n_free], t - n_free); // hold last value
  }
  return link == 0 ? exp(x) : x;
}

/**
 * Build a random-walk deviation for a time-varying parameter
 *
 * The walk is the cumulative sum of `steps`, expanded so each step applies to a
 * block of `period` time points, then centred over the observation window
 * (`n_centre`) so the baseline is identifiable. Returns the mean-zero link-scale
 * deviation over the free window, ready to combine additively with other
 * components before `assemble_state`.
 *
 * @param n_free Window over which the walk varies
 * @param n_centre Leading window used to centre the walk for identifiability
 * @param steps Random walk steps (one per period block, less one)
 * @param period Number of time points between random walk steps
 * @return A link-scale deviation of length n_free
 *
 * @ingroup estimates_smoothing
 */
vector rw_dev(int n_free, int n_centre, vector steps, int period) {
  vector[n_free] dev = rep_vector(0, n_free);
  int n_steps = num_elements(steps);
  if (n_steps > 0) {
    vector[n_steps + 1] cum;
    cum[1] = 0;
    cum[2:(n_steps + 1)] = cumulative_sum(steps);
    // expand each step to a block of `period` time points over the free window
    for (i in 1:n_free) {
      dev[i] = cum[(i - 1) %/% period + 1];
    }
    // centre over the observation window for identifiability
    dev -= mean(dev[1:n_centre]);
  }
  return dev;
}

/**
 * Build a random-walk deviation on explicit knots
 *
 * As `rw_dev`, but a step starts at each of `knots` (absolute, 1-indexed time
 * points within the free window, ascending) rather than on a regular grid.
 * `knots` has already been clipped to the free window by the caller, so
 * `num_elements(knots)` equals `num_elements(steps)`.
 *
 * @param n_free Window over which the walk varies
 * @param n_centre Leading window used to centre the walk for identifiability
 * @param steps Random walk steps, one per knot
 * @param knots Ascending 1-indexed time points at which a new step starts
 * @return A link-scale deviation of length n_free
 *
 * @ingroup estimates_smoothing
 */
vector rw_dev_knots(int n_free, int n_centre, vector steps,
                    array[] int knots) {
  vector[n_free] dev = rep_vector(0, n_free);
  int n_steps = num_elements(steps);
  if (n_steps > 0) {
    vector[n_steps + 1] cum;
    cum[1] = 0;
    cum[2:(n_steps + 1)] = cumulative_sum(steps);
    int kn = num_elements(knots);
    int seg = 1; // current segment (1-indexed into cum)
    int ki = 1; // next knot to cross
    for (i in 1:n_free) {
      while (ki <= kn && knots[ki] <= i) {
        seg += 1;
        ki += 1;
      }
      dev[i] = cum[seg];
    }
    // centre over the observation window for identifiability
    dev -= mean(dev[1:n_centre]);
  }
  return dev;
}

/**
 * Number of time points a Gaussian process component is evaluated at
 *
 * A mean-anchored (stationary) GP is evaluated at every point of its free
 * window. An init-anchored GP models the increments between successive
 * points, of which there is one fewer.
 *
 * @param n_free Free-noise window of the component's state
 * @param anchor 0 = mean (stationary), 1 = init (non-stationary)
 * @return The number of GP evaluation points
 *
 * @ingroup estimates_smoothing
 */
int gp_noise_points(int n_free, int anchor) {
  return anchor == 1 ? n_free - 1 : n_free;
}

/**
 * Build a Gaussian process deviation for a time-varying parameter
 *
 * For the `mean` anchor (`anchor = 0`) the GP is stationary (mean-reverting
 * around the baseline) and the deviation is the noise directly. For the `init`
 * anchor (`anchor = 1`) the GP models the `n_free - 1` increments between
 * successive time points, so the deviation starts at zero and accumulates the
 * noise, and is then centred over the observation window (`n_centre`) for
 * identifiability. Returns the link-scale deviation over the free window,
 * ready to combine additively with other components before `assemble_state`.
 * GP noise is supplied directly (computed via update_gp).
 *
 * @param n_free Window over which the GP varies
 * @param n_centre Leading window used to centre an init-anchored GP
 * @param noise Gaussian process noise (length n_free for the `mean` anchor,
 *   n_free - 1 for the `init` anchor; see `gp_noise_points`)
 * @param anchor 0 = mean (stationary), 1 = init (non-stationary)
 * @return A link-scale deviation of length n_free
 *
 * @ingroup estimates_smoothing
 */
vector gp_dev(int n_free, int n_centre, vector noise, int anchor) {
  vector[n_free] dev;
  if (anchor == 0) {
    dev = noise; // stationary (mean-reverting)
  } else {
    // non-stationary: the GP gives the increments, so the path starts at 0
    dev = append_row(0, cumulative_sum(noise));
    dev -= mean(dev[1:n_centre]); // centre over the observation window
  }
  return dev;
}

/**
 * Link-scale deviation of one trajectory component
 *
 * Builds the random-walk or Gaussian-process deviation of a single component
 * (the body of `get_state_trajectory`'s per-component loop, split out so each
 * piece is independently readable). Each component reads the flat
 * coefficient/hyperparameter vectors by its own offset into `comp_pos`.
 *
 * @param c Index of the component in the flat component table
 * @param nf Free-noise window of the component's state
 * @param nc Centring window of the component's state
 * @param anchor Anchor of the component's state (0 = mean, 1 = init)
 * @param comp_type Type of each component (0 = RW, 1 = GP)
 * @param comp_pos Index of each component within its type group
 * @param comp_rw_n Number of random walk steps of each component
 * @param comp_rw_offset Offset of each component into state_rw_steps
 * @param state_rw_period Number of time steps between random walk steps (the
 *   regular grid; ignored by a component with its own knots)
 * @param rw_knots_n Number of knots of each RW component (0 = regular grid)
 * @param rw_knots_offset Offset of each RW component into rw_knots
 * @param rw_knots Ascending 1-indexed knots, clipped to each component's own
 *   free window in transformed data
 * @param state_rw_steps Flat random walk steps across RW components
 * @param comp_gp_M Number of GP basis functions of each component
 * @param comp_gp_offset Offset of each component into state_gp_eta
 * @param state_gp_eta Flat GP basis coefficients across GP components
 * @param gp_boundary_scale GP boundary scale of each GP component
 * @param gp_kernel Kernel of each GP component
 * @param gp_nu Matern smoothness of each GP component
 * @param state_gp_alpha GP magnitude of each GP component
 * @param state_gp_rho GP lengthscale of each GP component
 * @param gp_phi Precomputed GP basis of each GP component (built once in
 *   transformed data over its `gp_noise_points()`)
 * @return A vector of length `nf`, the component's link-scale deviation
 *
 * @ingroup estimates_smoothing
 */
vector component_dev(
  int c, int nf, int nc, int anchor,
  array[] int comp_type, array[] int comp_pos,
  array[] int comp_rw_n, array[] int comp_rw_offset, int state_rw_period,
  array[] int rw_knots_n, array[] int rw_knots_offset, array[] int rw_knots,
  vector state_rw_steps,
  array[] int comp_gp_M, array[] int comp_gp_offset, vector state_gp_eta,
  array[] real gp_boundary_scale, array[] int gp_kernel, array[] real gp_nu,
  vector state_gp_alpha, vector state_gp_rho, array[] matrix gp_phi
) {
  if (comp_type[c] == 0) {
    vector[comp_rw_n[c]] steps = segment(
      state_rw_steps, comp_rw_offset[c] + 1, comp_rw_n[c]
    );
    int p = comp_pos[c];
    if (rw_knots_n[p] > 0) {
      // knots are clipped to comp_rw_n[c] entries by transformed data
      return rw_dev_knots(
        nf, nc, steps, segment(rw_knots, rw_knots_offset[p] + 1, comp_rw_n[c])
      );
    }
    return rw_dev(nf, nc, steps, state_rw_period);
  }
  int p = comp_pos[c];
  int M = comp_gp_M[c]; // total eta/phi columns (the periodic kernel's are doubled)
  vector[M] eta = segment(state_gp_eta, comp_gp_offset[c] + 1, M);
  // the basis is built once in transformed data (it is data-only); here we
  // just apply the per-iteration hyperparameters through update_gp. Its own
  // M argument is the base (pre-doubling) basis count: it doubles it itself
  // for the periodic kernel, matching the doubled eta/phi columns above.
  int M_base = gp_kernel[p] == 1 ? M %/% 2 : M;
  int ng = gp_noise_points(nf, anchor);
  if (ng == 0) {
    return rep_vector(0, nf);
  }
  matrix[ng, M] phi = gp_phi[p][1:ng, 1:M];
  return gp_dev(nf, nc, update_gp(
    phi, M_base, gp_boundary_scale[p], state_gp_alpha[p],
    rescale_gp_lengthscale(state_gp_rho[p], ng), eta, gp_kernel[p], gp_nu[p]
  ), anchor);
}

/**
 * Get the trajectory of a (possibly time-varying) parameter
 *
 * Thin dispatch over the registered states: if a state is attached to the
 * parameter with the given id, builds its trajectory by summing the link-scale
 * deviations of its components (via `component_dev`) onto the baseline;
 * otherwise returns a constant trajectory at `level`. This lets any parameter
 * consumed pointwise over time become time-varying with no per-parameter code
 * beyond the call site.
 *
 * A state's free-noise and centring windows are shared by all its components.
 *
 * @param id Target parameter id
 * @param t Total trajectory length (observation window + forecast horizon)
 * @param level Parameter level on the natural scale (from get_param)
 * @param state_param_id Target parameter id of each state
 * @param state_link Link of each state (0 = log)
 * @param state_anchor Anchor of each state (0 = mean, 1 = init)
 * @param state_comp_offset Offset of each state into the component table
 * @param state_comp_n Number of components of each state
 * @param state_n_free Free-noise window of each state (holds its last value
 *   through the remaining forecast horizon)
 * @param state_n_centre Leading window used to centre an init-anchored state
 * @param comp_type Type of each component (0 = RW, 1 = GP)
 * @param comp_pos Index of each component within its type group
 * @param comp_rw_n Number of random walk steps of each component
 * @param comp_rw_offset Offset of each component into state_rw_steps
 * @param state_rw_period Number of time steps between random walk steps (the
 *   regular grid; ignored by a component with its own knots)
 * @param rw_knots_n Number of knots of each RW component (0 = regular grid)
 * @param rw_knots_offset Offset of each RW component into rw_knots
 * @param rw_knots Ascending 1-indexed knots, clipped to each component's own
 *   free window in transformed data
 * @param state_rw_steps Flat random walk steps across RW components
 * @param comp_gp_M Number of GP basis functions of each component
 * @param comp_gp_offset Offset of each component into state_gp_eta
 * @param state_gp_eta Flat GP basis coefficients across GP components
 * @param gp_boundary_scale GP boundary scale of each GP component
 * @param gp_kernel Kernel of each GP component
 * @param gp_nu Matern smoothness of each GP component
 * @param state_gp_alpha GP magnitude of each GP component
 * @param state_gp_rho GP lengthscale of each GP component
 * @param gp_phi Precomputed GP basis of each GP component (built once in
 *   transformed data over its `gp_noise_points()`)
 * @return A vector of length t with the parameter trajectory
 *
 * @ingroup estimates_smoothing
 */
vector get_state_trajectory(
  int id, int t, real level,
  array[] int state_param_id, array[] int state_link, array[] int state_anchor,
  array[] int state_comp_offset, array[] int state_comp_n,
  array[] int state_n_free, array[] int state_n_centre,
  array[] int comp_type, array[] int comp_pos,
  array[] int comp_rw_n, array[] int comp_rw_offset, int state_rw_period,
  array[] int rw_knots_n, array[] int rw_knots_offset, array[] int rw_knots,
  vector state_rw_steps,
  array[] int comp_gp_M, array[] int comp_gp_offset, vector state_gp_eta,
  array[] real gp_boundary_scale, array[] int gp_kernel, array[] real gp_nu,
  vector state_gp_alpha, vector state_gp_rho, array[] matrix gp_phi
) {
  for (s in 1:num_elements(state_param_id)) {
    if (state_param_id[s] == id) {
      int nf = state_n_free[s];
      int nc = state_n_centre[s];
      vector[nf] dev = rep_vector(0, nf);
      for (k in 1:state_comp_n[s]) {
        int c = state_comp_offset[s] + k;
        dev += component_dev(
          c, nf, nc, state_anchor[s],
          comp_type, comp_pos,
          comp_rw_n, comp_rw_offset, state_rw_period,
          rw_knots_n, rw_knots_offset, rw_knots, state_rw_steps,
          comp_gp_M, comp_gp_offset, state_gp_eta,
          gp_boundary_scale, gp_kernel, gp_nu,
          state_gp_alpha, state_gp_rho, gp_phi
        );
      }
      return assemble_state(t, nf, level, dev, state_link[s]);
    }
  }
  return rep_vector(level, t);
}
