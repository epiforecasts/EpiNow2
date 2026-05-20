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
 * Update log density for parameter priors
 *
 * This function adds the log density contributions from parameter priors
 * to the target, supporting multiple prior distribution types.
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
    if (prior_dist[id] == 0) { // lognormal
      params[id] ~
        lognormal(
          prior_dist_params[params_id], prior_dist_params[params_id + 1]
        )
        T[params_lower[id], params_upper[id]];
      params_id += 2;
    } else if (prior_dist[id] == 1) {
      params[id] ~
        gamma(prior_dist_params[params_id], prior_dist_params[params_id + 1])
        T[params_lower[id], params_upper[id]];
      params_id += 2;
    } else if (prior_dist[id] == 2) {
      params[id] ~
        normal(prior_dist_params[params_id], prior_dist_params[params_id + 1])
        T[params_lower[id], params_upper[id]];
      params_id += 2;
    } else {
      reject("dist must be <= 2");
    }
  }
}

/**
 * Apply user prior on the initial value of a centred-GP-wrapped parameter.
 *
 * When a parameter is wrapped by a centred non-stationary GP, the user-facing
 * prior is on the initial value of the trajectory (X[1]) rather than on the
 * sampled internal log-mean. This helper applies the prior to the derived
 * initial value with the Jacobian correction for the linear-shift transform
 * from log-mean to log-initial.
 *
 * Generic over the parameter — used by R0 today, lifts to any future
 * time-varying parameter (alpha, dispersion, ...) via the same call.
 *
 * @param init_value Derived initial value of the trajectory (e.g. R[1]).
 * @param dist_type Prior distribution type (0 = lognormal, 1 = gamma, 2 = normal).
 * @param p1 First distribution parameter.
 * @param p2 Second distribution parameter.
 * @return Log-density contribution to the target, Jacobian included.
 *
 * @ingroup parameter_handlers
 */
real centred_gp_init_lpdf(real init_value, int dist_type, real p1, real p2) {
  if (dist_type == 0) {
    return lognormal_lpdf(init_value | p1, p2) + log(init_value);
  } else if (dist_type == 1) {
    return gamma_lpdf(init_value | p1, p2) + log(init_value);
  } else if (dist_type == 2) {
    return normal_lpdf(init_value | p1, p2) + log(init_value);
  } else {
    reject("centred_gp_init_lpdf: dist_type must be 0, 1, or 2");
  }
  return 0;
}

