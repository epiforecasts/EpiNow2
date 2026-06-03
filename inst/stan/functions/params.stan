/**
 * Parameter Handlers
 *
 * This group of functions handles parameter access, retrieval, and prior
 * specification in the model. Parameters can be either fixed (specified in advance)
 * or variable (estimated during inference).
 */

/**
 * Get a parameter value from either fixed or variable parameters
 *
 * This function retrieves a parameter value based on its ID, checking first if it's
 * a fixed parameter and then if it's a variable parameter.
 *
 * @param id Parameter ID
 * @param params_fixed_lookup Array of fixed parameter lookup indices
 * @param params_variable_lookup Array of variable parameter lookup indices
 * @param params_value Vector of fixed parameter values
 * @param params Vector of variable parameter values
 * @return The parameter value (scalar)
 *
 * @ingroup parameter_handlers
 */
real get_param(int id,
               array[] int params_fixed_lookup,
               array[] int params_variable_lookup,
               vector params_value, vector params) {
  if (id == 0) {
    return 0; // parameter not used
  } else if (params_fixed_lookup[id]) {
    return params_value[params_fixed_lookup[id]];
  } else {
    return params[params_variable_lookup[id]];
  }
}

/**
 * Get a parameter value from either fixed or variable parameters (matrix version)
 *
 * This function is an overloaded version of get_param that works with a matrix of
 * parameter values, returning a vector of parameter values for multiple samples.
 *
 * @param id Parameter ID
 * @param params_fixed_lookup Array of fixed parameter lookup indices
 * @param params_variable_lookup Array of variable parameter lookup indices
 * @param params_value Vector of fixed parameter values
 * @param params Matrix of variable parameter values (rows are samples)
 * @return A vector of parameter values across samples
 *
 * @ingroup parameter_handlers
 */
vector get_param(int id,
                 array[] int params_fixed_lookup,
                 array[] int params_variable_lookup,
                 vector params_value, matrix params) {
  int n_samples = rows(params);
  if (id == 0) {
    return rep_vector(0, n_samples) ; // parameter not used
  } else if (params_fixed_lookup[id]) {
    return rep_vector(params_value[params_fixed_lookup[id]], n_samples);
  } else {
    return params[, params_variable_lookup[id]];
  }
}

/**
 * Apply a truncated prior to a value
 *
 * Adds the log density of the chosen distribution, truncated to
 * `[lower, upper]`, to the target.
 *
 * @param value Value to apply the prior to (sampled parameter or derived
 *   quantity).
 * @param dist Prior distribution type (0: lognormal, 1: gamma, 2: normal).
 * @param p1 First distribution parameter.
 * @param p2 Second distribution parameter.
 * @param lower Lower bound of the parameter's support.
 * @param upper Upper bound of the parameter's support.
 *
 * @ingroup parameter_handlers
 */
void apply_prior_lp(real value, int dist,
                    real p1, real p2,
                    real lower, real upper) {
  if (dist == 0) {
    value ~ lognormal(p1, p2) T[lower, upper];
  } else if (dist == 1) {
    value ~ gamma(p1, p2) T[lower, upper];
  } else if (dist == 2) {
    value ~ normal(p1, p2) T[lower, upper];
  } else {
    reject("dist must be <= 2");
  }
}

/**
 * Update log density for parameter priors
 *
 * Adds the log density contributions from parameter priors to the target.
 *
 * @param params Vector of parameter values
 * @param prior_dist Array of prior distribution types (0: lognormal, 1: gamma, 2: normal)
 * @param prior_dist_params Vector of prior distribution parameters
 * @param params_lower Vector of lower bounds for parameters
 * @param params_upper Vector of upper bounds for parameters
 *
 * @ingroup parameter_handlers
 */
void params_lp(vector params, array[] int prior_dist,
              vector prior_dist_params, vector params_lower,
              vector params_upper) {
  int params_id = 1;
  int num_params = num_elements(params);
  for (id in 1:num_params) {
    apply_prior_lp(
      params[id], prior_dist[id],
      prior_dist_params[params_id], prior_dist_params[params_id + 1],
      params_lower[id], params_upper[id]
    );
    params_id += 2;
  }
}


