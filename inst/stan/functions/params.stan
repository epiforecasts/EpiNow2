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
 * Apply a prior to a value, truncating only where a bound is finite
 *
 * Adds the log density of the chosen distribution to the target, truncated
 * to `[lb, ub]` where those bounds are finite. Truncation is only necessary
 * when a bound constrains the value beyond what the distribution's own
 * support already implies (e.g. a physical constraint such as a probability
 * lying in `[0, 1]`); otherwise it is dropped, since evaluating a
 * distribution's (c)cdf at an infinite argument is needless work and, for
 * some distributions, can be numerically unstable.
 *
 * @param value Value to apply the prior to (sampled parameter or derived
 *   quantity).
 * @param dist Prior distribution type (0: lognormal, 1: gamma, 2: normal).
 * @param p1 First distribution parameter.
 * @param p2 Second distribution parameter.
 * @param lb Lower bound of the parameter's support; `negative_infinity()`
 *   if none.
 * @param ub Upper bound of the parameter's support; `positive_infinity()`
 *   if none.
 *
 * @ingroup parameter_handlers
 */
void apply_prior_lp(real value, int dist,
                    real p1, real p2,
                    real lb, real ub) {
  int truncate_lb = lb > negative_infinity();
  int truncate_ub = ub < positive_infinity();
  if (dist == 0) {
    if (truncate_lb && truncate_ub) {
      value ~ lognormal(p1, p2) T[lb, ub];
    } else if (truncate_lb) {
      value ~ lognormal(p1, p2) T[lb, ];
    } else if (truncate_ub) {
      value ~ lognormal(p1, p2) T[, ub];
    } else {
      value ~ lognormal(p1, p2);
    }
  } else if (dist == 1) {
    if (truncate_lb && truncate_ub) {
      value ~ gamma(p1, p2) T[lb, ub];
    } else if (truncate_lb) {
      value ~ gamma(p1, p2) T[lb, ];
    } else if (truncate_ub) {
      value ~ gamma(p1, p2) T[, ub];
    } else {
      value ~ gamma(p1, p2);
    }
  } else if (dist == 2) {
    if (truncate_lb && truncate_ub) {
      value ~ normal(p1, p2) T[lb, ub];
    } else if (truncate_lb) {
      value ~ normal(p1, p2) T[lb, ];
    } else if (truncate_ub) {
      value ~ normal(p1, p2) T[, ub];
    } else {
      value ~ normal(p1, p2);
    }
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

/**
 * Apply user priors on the initial values of centred-GP-wrapped trajectories
 *
 * For each registered init prior, dispatches on the parameter id to the
 * corresponding derived initial value and to the parameter actually sampled,
 * then applies the user's truncated prior via `apply_prior_lp`.
 *
 * The prior is declared on a derived value (e.g. `R[1]`) while the sampler
 * moves on a different parameter (e.g. `R_mean`), whose `<lower = 0>`
 * transform already contributes `log(sampled_value)` to the target. The
 * Jacobian of the sampled-to-derived map is therefore taken relative to the
 * sampled value, `log(init_value) - log(sampled_value)`.
 *
 * @param init_param_ids Per-prior id of the parameter the prior applies to.
 * @param init_dists Per-prior distribution code (0: lognormal, 1: gamma,
 *   2: normal).
 * @param init_dist_params Flat ragged vector of distribution parameters,
 *   two per prior.
 * @param init_lower Per-prior lower bound on the parameter's support.
 * @param init_upper Per-prior upper bound on the parameter's support.
 * @param param_id_R0 Registered id of R0.
 * @param R Reproduction-number trajectory.
 * @param R_mean Sampled mean reproduction number over the centring window.
 *
 * @ingroup parameter_handlers
 */
void init_priors_lp(array[] int init_param_ids, array[] int init_dists,
                    vector init_dist_params,
                    vector init_lower, vector init_upper,
                    int param_id_R0, vector R, array[] real R_mean) {
  int params_id = 1;
  for (i in 1:num_elements(init_param_ids)) {
    real init_value;
    real sampled_value;
    if (init_param_ids[i] == param_id_R0) {
      init_value = R[1];
      sampled_value = R_mean[1];
    } else {
      reject("no init param registered for id ", init_param_ids[i]);
    }
    apply_prior_lp(
      init_value, init_dists[i],
      init_dist_params[params_id], init_dist_params[params_id + 1],
      init_lower[i], init_upper[i]
    );
    target += log(init_value) - log(sampled_value);
    params_id += 2;
  }
}


