/**
 * Reproduction Number (Rt) Functions
 *
 * This group of functions handles the calculation, updating, and conversion of
 * reproduction numbers in the model. The reproduction number represents the average
 * number of secondary infections caused by a single infected individual.
 *
 * @ingroup rt_estimation
 */

/**
 * @ingroup rt_estimation
 * @brief Weights mapping breakpoint effects to the mean of the breakpoint
 * random walk over the centring window.
 *
 * The random walk at day j is the sum of `bp_effects[1:(bps[j] - 1)]`, so its
 * mean over days `1:n_centre` is `dot_product(bp_effects, weights)` where
 * `weights[i]` is the proportion of those days with `bps[j] > i`.
 *
 * @param bps Array of breakpoint indices
 * @param bp_n Number of breakpoint effects
 * @param n_centre Number of leading days in the centring window
 * @param first_level Whether to subtract the level of the first day, so the
 *   intercept starts at `bps[1]` rather than the first level
 * @return A vector of weights, one per breakpoint effect
 */
vector bp_centring_weights(array[] int bps, int bp_n, int n_centre,
                           int first_level) {
  vector[bp_n] w = rep_vector(0, bp_n);
  for (j in 1:n_centre) {
    if (bps[j] > 1) {
      w[bps[j] - 1] += 1;
    }
  }
  w = reverse(cumulative_sum(reverse(w))) / n_centre;
  for (i in 1:(first_level ? bps[1] - 1 : 0)) {
    w[i] -= 1;
  }
  return w;
}

/**
 * @ingroup rt_estimation
 * @brief Update a vector of effective reproduction numbers (Rt) based on
 * an intercept, breakpoints (i.e. a random walk), and a Gaussian
 * process.
 *
 * @param t Length of the time series
 * @param R0 Initial reproduction number
 * @param noise Vector of Gaussian process noise values
 * @param bps Array of breakpoint indices
 * @param bp_effects Vector of breakpoint effects
 * @param stationary Whether the Gaussian process is stationary (1) or non-stationary (0)
 * @param n_centre Number of leading positions over which to centre the
 *   non-stationary GP trajectory and the breakpoint random walk. Set to the
 *   observation window length so the centring is invariant to the forecast
 *   horizon. Ignored for the GP branch when `stationary` is 1; the breakpoint
 *   path is centred whenever breakpoints are present.
 * @return A vector of length t containing the updated Rt values
 */
vector update_Rt(int t, real R0, vector noise, array[] int bps,
                 vector bp_effects, int stationary, int n_centre) {
  int bp_n = num_elements(bp_effects);
  int gp_n = num_elements(noise);
  // Intercept on the log scale. The centring means are linear in the
  // increments, so they are subtracted here as weighted sums rather than from
  // full length paths.
  real c = log(R0);

  if (gp_n == 0) {
    if (bp_n == 0) {
      return rep_vector(R0, t);
    }
    // One value per breakpoint level, then expand by index
    real c_bp = c - dot_product(
      bp_effects, bp_centring_weights(bps, bp_n, n_centre, 0)
    );
    vector[bp_n + 1] R_bp = exp(cumulative_sum(append_row(c_bp, bp_effects)));
    return R_bp[bps];
  }

  if (stationary) {
    vector[t] R;
    if (bp_n == 0) {
      R[1:gp_n] = exp(c + noise);
      // Hold the last estimate into the forecast horizon
      if (t > gp_n) {
        R[(gp_n + 1):t] = rep_vector(R[gp_n], t - gp_n);
      }
    } else {
      // Breakpoint levels still change beyond the GP, so the last GP value
      // is held and multiplied by the breakpoint level on each day
      real c_bp = c - dot_product(
        bp_effects, bp_centring_weights(bps, bp_n, n_centre, 0)
      );
      vector[bp_n + 1] R_bp = exp(
        cumulative_sum(append_row(c_bp, bp_effects))
      );
      vector[gp_n] R_gp = exp(noise);
      R[1:gp_n] = R_bp[bps[1:gp_n]] .* R_gp;
      if (t > gp_n) {
        R[(gp_n + 1):t] = R_bp[bps[(gp_n + 1):t]] * R_gp[gp_n];
      }
    }
    return R;
  }

  // Non-stationary GP: log Rt is one cumulative sum of daily increments with
  // the (centred) intercept as the first element. noise[i] enters days
  // i + 1 to n_centre of the centring window.
  int m = min(gp_n, n_centre - 1);
  if (m > 0) {
    c -= dot_product(
      noise[1:m],
      reverse(linspaced_vector(m, n_centre - m, n_centre - 1)) / n_centre
    );
  }
  if (bp_n == 0) {
    vector[gp_n + 1] R_gp = exp(cumulative_sum(append_row(c, noise)));
    if (t > gp_n + 1) {
      return append_row(R_gp, rep_vector(R_gp[gp_n + 1], t - gp_n - 1));
    }
    return R_gp;
  }
  // Breakpoints add their jumps to the increments on the days they occur.
  vector[t] inc = rep_vector(0, t);
  inc[1] = c - dot_product(
    bp_effects, bp_centring_weights(bps, bp_n, n_centre, 1)
  );
  inc[2:(gp_n + 1)] = noise;
  for (j in 2:t) {
    int from = bps[j - 1];
    int to = bps[j];
    if (to != from) {
      real jump;
      if (to == from + 1) {
        jump = bp_effects[from];
      } else if (to > from) {
        jump = sum(bp_effects[from:(to - 1)]);
      } else {
        jump = -sum(bp_effects[to:(from - 1)]);
      }
      if (j <= gp_n + 1) {
        inc[j] += jump;
      } else {
        inc[j] = jump;
      }
    }
  }
  return exp(cumulative_sum(inc));
}

/**
 * Calculate the log-probability of the reproduction number (Rt) priors
 *
 * This function adds the log density contributions from priors on initial infections
 * and breakpoint effects to the target.
 *
 * @param initial_infections_scale Array of initial infection values
 * @param bp_effects Vector of breakpoint effects
 * @param bp_sd Array of breakpoint standard deviations
 * @param bp_n Number of breakpoints
 * @param cases Array of observed case counts
 * @param initial_infections_guess Initial guess for infections based on cases
 *
 * @ingroup rt_estimation
 */
void rt_lp(array[] real initial_infections_scale, vector bp_effects,
           array[] real bp_sd, int bp_n, array[] int cases,
           real initial_infections_guess) {
  //breakpoint effects on Rt
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
  initial_infections_scale ~ normal(initial_infections_guess, 2);
}

/**
 * Helper function for calculating r from R using Newton's method
 *
 * This function performs a single Newton step in the iterative calculation
 * of the growth rate r from the reproduction number R.
 *
 * Code is based on Julia code from
 * https://github.com/CDCgov/Rt-without-renewal/blob/d6344cc6e451e3e6c4188e4984247f890ae60795/EpiAware/test/predictive_checking/fast_approx_for_r.jl
 * under Apache license 2.0.
 *
 * @param R Reproduction number
 * @param r Current estimate of the growth rate
 * @param pmf Generation time probability mass function (first index: 0)
 * @return The Newton step for updating r
 *
 * @ingroup rt_estimation
 */
real R_to_r_newton_step(real R, real r, vector pmf) {
  int len = num_elements(pmf);
  vector[len] zero_series = linspaced_vector(len, 0, len - 1);
  vector[len] exp_r = exp(-r * zero_series);
  real ret = (R * dot_product(pmf, exp_r) - 1) /
    (- R * dot_product(pmf .* zero_series, exp_r));
  return(ret);
}

/**
 * Estimate the growth rate r from reproduction number R
 *
 * This function uses the Newton method to solve for the growth rate r
 * that corresponds to a given reproduction number R, using the generation
 * time distribution.
 *
 * Code is based on Julia code from
 * https://github.com/CDCgov/Rt-without-renewal/blob/d6344cc6e451e3e6c4188e4984247f890ae60795/EpiAware/test/predictive_checking/fast_approx_for_r.jl
 * under Apache license 2.0.
 *
 * @param R Reproduction number
 * @param gt_rev_pmf Reversed probability mass function of the generation time
 * @param abs_tol Absolute tolerance for the Newton solver
 * @return The estimated growth rate r
 *
 * @ingroup rt_estimation
 */
real R_to_r(real R, vector gt_rev_pmf, real abs_tol) {
  int gt_len = num_elements(gt_rev_pmf);
  vector[gt_len] gt_pmf = reverse(gt_rev_pmf);
  real mean_gt = dot_product(gt_pmf, linspaced_vector(gt_len, 0, gt_len - 1));
  real r = fmax((R - 1) / (R * mean_gt), -1);
  real step = abs_tol + 1;
  while (abs(step) > abs_tol) {
    step = R_to_r_newton_step(R, r, gt_pmf);
    r -= step;
  }

  return(r);
}
