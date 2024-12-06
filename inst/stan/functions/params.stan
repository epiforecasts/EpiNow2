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

void params_lp(vector params, array[] int prior_dist,
              vector prior_dist_params, vector params_lower,
              vector params_upper) {
  int params_id = 1;
  int num_params = num_elements(params);
  for (id in 1:num_params) {
    if (prior_dist[id] == 0) { // lognormal
      params[id] ~
        lognormal(prior_dist_params[params_id], prior_dist_params[params_id + 1])
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
