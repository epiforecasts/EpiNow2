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
  // define control parameters
  int bp_n = num_elements(bp_effects);
  int gp_n = num_elements(noise);
  // initialise intercept
  vector[t] logR = rep_vector(log(R0), t);
  //initialise breakpoints + rw
  if (bp_n) {
    vector[bp_n + 1] bp0;
    bp0[1] = 0;
    bp0[2:(bp_n + 1)] = cumulative_sum(bp_effects);
    vector[t] bp = bp0[bps];
    // Centre over the observation window (same identifiability fix as the GP below).
    bp -= mean(bp[1:n_centre]);
    logR = logR + bp;
  }
  //initialise gaussian process
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
      // Centre over the observation window (same identifiability fix as the BP above).
      gp -= mean(gp[1:n_centre]);
    }
    logR = logR + gp;
  }

  return exp(logR);
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
 * Estimate the growth rate r from reproduction number R
 *
 * This function uses the Newton method to solve for the growth rate r
 * that corresponds to a given reproduction number R, using the generation
 * time distribution. It is implemented in C++ in
 * inst/include/epinow2/R_to_r.hpp, with a gradient from the implicit
 * function theorem.
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
real R_to_r(real R, vector gt_rev_pmf, data real abs_tol);
