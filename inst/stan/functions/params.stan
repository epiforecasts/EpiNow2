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
 * Apply a user prior on the derived initial value of a centred-GP trajectory.
 *
 * The trajectory's mean is sampled as the internal parameter and the initial
 * value is derived by a pure shift, which contributes Jacobian determinant
 * one for the linear transform; the user prior is then evaluated at the
 * derived natural-scale initial value with the log Jacobian for the
 * natural-to-log transform. The same `T[lower, upper]` truncation correction
 * applied by `params_lp` is included so the prior's meaning matches an
 * equivalently bounded sampled parameter.
 *
 * @param init_value Derived initial value of the trajectory (e.g. R[1]).
 * @param dist_type Prior distribution type (0 = lognormal, 1 = gamma,
 *   2 = normal).
 * @param dist_params Distribution parameters, sliced for this prior from the
 *   ragged `init_dist_params` vector.
 * @param lower Lower bound of the parameter's support.
 * @param upper Upper bound of the parameter's support.
 * @return Log-density contribution to the target, Jacobian and truncation
 *   correction included.
 *
 * @ingroup parameter_handlers
 */
real gp_init_lpdf(real init_value, int dist_type, vector dist_params,
                  real lower, real upper) {
  real p1 = dist_params[1];
  real p2 = dist_params[2];
  real lpdf;
  real log_norm = 0;
  if (dist_type == 0) {
    lpdf = lognormal_lpdf(init_value | p1, p2);
    if (lower > 0 && !is_inf(upper)) {
      log_norm = log_diff_exp(
        lognormal_lcdf(upper | p1, p2), lognormal_lcdf(lower | p1, p2)
      );
    } else if (lower > 0) {
      log_norm = lognormal_lccdf(lower | p1, p2);
    } else if (!is_inf(upper)) {
      log_norm = lognormal_lcdf(upper | p1, p2);
    }
  } else if (dist_type == 1) {
    lpdf = gamma_lpdf(init_value | p1, p2);
    if (lower > 0 && !is_inf(upper)) {
      log_norm = log_diff_exp(
        gamma_lcdf(upper | p1, p2), gamma_lcdf(lower | p1, p2)
      );
    } else if (lower > 0) {
      log_norm = gamma_lccdf(lower | p1, p2);
    } else if (!is_inf(upper)) {
      log_norm = gamma_lcdf(upper | p1, p2);
    }
  } else if (dist_type == 2) {
    lpdf = normal_lpdf(init_value | p1, p2);
    if (!is_inf(lower) && !is_inf(upper)) {
      log_norm = log_diff_exp(
        normal_lcdf(upper | p1, p2), normal_lcdf(lower | p1, p2)
      );
    } else if (!is_inf(lower)) {
      log_norm = normal_lccdf(lower | p1, p2);
    } else if (!is_inf(upper)) {
      log_norm = normal_lcdf(upper | p1, p2);
    }
  } else {
    reject("gp_init_lpdf: dist_type must be 0, 1, or 2");
  }
  return lpdf - log_norm + log(init_value);
}

/**
 * Number of parameters for a prior distribution type, used to advance an
 * offset into the ragged `init_dist_params` vector during dispatch.
 *
 * @param dist_type Prior distribution type (0 = lognormal, 1 = gamma,
 *   2 = normal).
 * @return Number of distribution parameters.
 *
 * @ingroup parameter_handlers
 */
int init_dist_n_params(int dist_type) {
  if (dist_type == 0) return 2;
  else if (dist_type == 1) return 2;
  else if (dist_type == 2) return 2;
  else reject("init_dist_n_params: dist_type must be 0, 1, or 2");
  return 0;
}

