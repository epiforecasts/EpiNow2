/**
 * Parameter Management Functions
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
