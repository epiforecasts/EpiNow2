/**
 * Time-varying parameter states
 *
 * General mechanism underlying time-varying parameters: build a trajectory by
 * combining a baseline (intercept) with a breakpoint random walk and a Gaussian
 * process. Components combine additively on the scale on which the trajectory is
 * built (the link scale); callers apply the inverse link (e.g. `exp` for a log
 * link) as appropriate.
 *
 * @ingroup estimates_smoothing
 */

/**
 * Build a time-varying trajectory on the link scale
 *
 * @param t Length of the time series
 * @param intercept Vector of length t giving the baseline (link-scale) value.
 *   Constant for a static mean, or a known mean trajectory the state fits
 *   deviations around.
 * @param noise Vector of Gaussian process noise values (empty for no GP)
 * @param bps Array of breakpoint indices
 * @param bp_effects Vector of breakpoint effects (empty for no random walk)
 * @param stationary Whether the Gaussian process is stationary (1) or
 *   non-stationary (0)
 * @param n_centre Number of leading positions over which to centre the
 *   non-stationary GP trajectory and the breakpoint random walk. Set to the
 *   observation window length so the centring is invariant to the forecast
 *   horizon. Ignored for the GP branch when `stationary` is 1; the breakpoint
 *   path is centred whenever breakpoints are present.
 * @return A vector of length t with the combined link-scale trajectory
 *
 * @ingroup estimates_smoothing
 */
vector update_state(int t, vector intercept, vector noise, array[] int bps,
                      vector bp_effects, int stationary, int n_centre) {
  int bp_n = num_elements(bp_effects);
  int gp_n = num_elements(noise);
  vector[t] x = intercept;
  // breakpoints + random walk
  if (bp_n) {
    vector[bp_n + 1] bp0;
    bp0[1] = 0;
    bp0[2:(bp_n + 1)] = cumulative_sum(bp_effects);
    vector[t] bp = bp0[bps];
    // Centre over the observation window for identifiability.
    bp -= mean(bp[1:n_centre]);
    x += bp;
  }
  // gaussian process
  if (gp_n) {
    vector[t] gp = rep_vector(0, t);
    if (stationary) {
      gp[1:gp_n] = noise;
      // fix future gp based on last estimated
      if (t > gp_n) {
        gp[(gp_n + 1):t] = rep_vector(noise[gp_n], t - gp_n);
      }
    } else {
      gp[2:(gp_n + 1)] = noise;
      gp = cumulative_sum(gp);
      // Centre over the observation window for identifiability.
      gp -= mean(gp[1:n_centre]);
    }
    x += gp;
  }
  return x;
}

/**
 * Build a random-walk trajectory for a time-varying parameter
 *
 * Anchors a centred random walk around a parameter's level and maps it through
 * the inverse link. The centring (over the observation window) makes the level
 * identifiable, mirroring the mean-reverting Rt parameterisation.
 *
 * @param t Length of the trajectory
 * @param level Baseline parameter value on the natural scale
 * @param steps Random walk steps (length t - 1)
 * @param link Link function (0 = log)
 * @param n_centre Number of leading positions over which to centre the walk
 * @return A vector of length t with the parameter trajectory on the natural scale
 *
 * @ingroup estimates_smoothing
 */
vector rw_trajectory(int t, real level, vector steps, int link, int n_centre) {
  array[t] int bps;
  for (i in 1:t) {
    bps[i] = i;
  }
  real intercept_value = link == 0 ? log(level) : level;
  vector[t] x = update_state(
    t, rep_vector(intercept_value, t), rep_vector(0.0, 0), bps, steps, 0, n_centre
  );
  if (link == 0) {
    return exp(x);
  }
  return x;
}

/**
 * Get the trajectory of a (possibly time-varying) parameter
 *
 * Returns the time-varying trajectory of the parameter with the given id if a
 * state is attached to it, otherwise a constant trajectory at `level`.
 *
 * @param id Target parameter id
 * @param t Length of the trajectory
 * @param n_centre Centring window for the state
 * @param level Parameter level on the natural scale (from get_param)
 * @param state_param_id Target parameter id of each state
 * @param state_link Link function of each state
 * @param state_rw_steps Flat vector of random walk steps across states
 * @param state_rw_n Number of random walk steps per state
 * @return A vector of length t with the parameter trajectory
 *
 * @ingroup estimates_smoothing
 */
vector get_param_trajectory(int id, int t, int n_centre, real level,
                            array[] int state_param_id,
                            array[] int state_link,
                            vector state_rw_steps, int state_rw_n) {
  for (p in 1:num_elements(state_param_id)) {
    if (state_param_id[p] == id) {
      vector[state_rw_n] steps = segment(
        state_rw_steps, (p - 1) * state_rw_n + 1, state_rw_n
      );
      return rw_trajectory(t, level, steps, state_link[p], n_centre);
    }
  }
  return rep_vector(level, t);
}
