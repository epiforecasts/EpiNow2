/**
 * Update a vector of effective reproduction numbers (Rt) based on
 * an intercept, breakpoints (i.e. a random walk), and a Gaussian
 * process.
 *
 * @param t Length of the time series
 * @param R0 Initial reproduction number
 * @param noise Vector of Gaussian process noise values
 * @param bps Array of breakpoint indices
 * @param bp_effects Vector of breakpoint effects
 * @param stationary Flag indicating whether the Gaussian process is stationary 
 * (1) or non-stationary (0)
 * @return A vector of length t containing the updated Rt values
 */
vector update_Rt(int t, real R0, vector noise, array[] int bps,
                 vector bp_effects, int stationary) {
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
    logR = logR + bp0[bps];
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
    }
    logR = logR + gp;
  }
  
  return exp(logR);
}

/**
 * Calculate the log-probability of the reproduction number (Rt) priors
 *
 * @param initial_infections Array of initial infection values
 * @param bp_effects Vector of breakpoint effects
 * @param bp_sd Array of breakpoint standard deviations
 * @param bp_n Number of breakpoints
 */
void rt_lp(array[] real initial_infections, vector bp_effects,
           array[] real bp_sd, int bp_n, array[] int cases,
           real infections_guess) {
  //breakpoint effects on Rt
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
  initial_infections ~ normal(log(infections_guess), 2);
}

/**
 * Calculate the negative moment generating function (nMGF) of a probability
 * distribution with given probability mass function.
 *
 * @param r function argument of the nMGF
 * @param pmf probability mass function as vector (first index: 0)
 */
  real neg_MGF(real r, vector pmf) {
    int len = num_elements(pmf);
    vector[len] exp_r = exp(-r * linspaced_vector(len, 0, len - 1));
    return(dot_product(pmf, exp_r));
  }

/**
 * Helper function for calculating r from R using Newton's method
 *
 * @param R Reproduction number
 * @param r growth rate
 * @param pmf generation time probability mass function (first index: 0)
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
 * Helper function used by the [R_to_r()] function to estimate r from R
 *
 * @param r A vector of length 1, the growth rate
 * @param gt_pmf probability mass function of the generation time
 * @param R reproduction number
 */
vector eq(vector r, vector gt_pmf, real R) {
  return([ neg_MGF(r[1], gt_pmf) - 1 / R ]');
}

/**
 * Estimate the growth rate r from reproduction number R. Used in the model to
 * estimate the initial growth rate using stan's algebraic solver
 *
 * @param R reproduction number
 * @param gt_rev_pmf reverse probability mass function of the generation time
 */
real R_to_r(real R, vector gt_rev_pmf, int newton_steps) {
  int gt_len = num_elements(gt_rev_pmf);
  vector[gt_len] gt_pmf = reverse(gt_rev_pmf);
  real mean_gt = dot_product(
    gt_pmf,
    linspaced_vector(gt_len, 0, gt_len - 1)
  );
  vector[1] r_approx = [ (R - 1) / (R * mean_gt) ]';
  vector[1] r = solve_powell(eq, r_approx, gt_pmf, R);

  return(r[1]);
}

/**
 * Estimate the growth rate r from reproduction number R. Used in the model to
 * estimate the initial growth rate using Newton's method.
 *
 * @param R reproduction number
 * @param gt_rev_pmf reverse probability mass function of the generation time
 * @param abs_tol absolute tolerance of the solver
 */
real R_to_r_manual(real R, vector gt_rev_pmf, real abs_tol) {
  int gt_len = num_elements(gt_rev_pmf);
  vector[gt_len] gt_pmf = reverse(gt_rev_pmf);
  real mean_gt = dot_product(gt_pmf, linspaced_vector(gt_len, 0, gt_len - 1));
  real r = (R - 1) / (R * mean_gt);
  real step = abs_tol + 1;
  while (abs(step) > abs_tol) {
    step = R_to_r_newton_step(R, r, gt_pmf);
    r -= step;
  }

  return(r);
}
