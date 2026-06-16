/**
 * Time-varying parameter processes
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
 *   Constant for a static mean, or a known mean trajectory the process fits
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
vector update_process(int t, vector intercept, vector noise, array[] int bps,
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
