// Stan functions from primarycensored version 1.5.2
real expgrowth_cdf(real x, real xmin, real xmax, real r) {
  if (x < xmin) {
    return 0;
  }
  if (x > xmax) {
    return 1;
  }
  if (abs(r) < 1e-10) {
    return (x - xmin) / (xmax - xmin);
  }
  return (exp(r * x) - exp(r * xmin)) / (exp(r * xmax) - exp(r * xmin));
}
real expgrowth_lcdf(real x, real xmin, real xmax, real r) {
  if (x < xmin) {
    return negative_infinity();
  }
  if (x > xmax) {
    return 0;
  }
  return log(expgrowth_cdf(x | xmin, xmax, r));
}
real expgrowth_lpdf(real x, real xmin, real xmax, real r) {
  if (x < xmin || x > xmax) {
    return negative_infinity();
  }
  if (abs(r) < 1e-10) {
    return -log(xmax - xmin);
  }
  return log(abs(r)) + r * x -
    log(abs(exp(r * xmax) - exp(r * xmin)));
}
vector primary_lcdf_vec(vector p, int primary_id,
                        array[] real primary_params, data real pwindow) {
  int N = num_elements(p);
  vector[N] out;
  for (i in 1:N) {
    out[i] = primary_lcdf(p[i] | primary_id, primary_params, pwindow);
  }
  return out;
}
real discretestep_lcdf(
  data real d, vector boundaries, vector pmf,
  int primary_id, array[] real primary_params, data real pwindow
) {
  int K = num_elements(pmf);
  // Integration support in u = d - p for p in [0, pwindow]. It is not
  // clipped at 0 so boundaries that start below zero (delays with negative
  // support) are handled; for non-negative boundaries the per-bin clip to
  // [boundaries[k], boundaries[k + 1]] below gives the same result.
  real u_min = d - pwindow;
  real u_max = d;

  // Structural-zero short-circuit. Below `boundaries[2]` F_step is zero
  // and the bin-1 contribution carries `cum_before = 0`, so the integral
  // collapses to 0. Returning `negative_infinity()` directly keeps
  // `log(0)` off the autodiff tape so downstream `log_diff_exp(a, -inf)`
  // evaluates cleanly with a zero gradient w.r.t. `pmf`.
  if (u_max <= boundaries[2]) return negative_infinity();

  // Sub-interval endpoints in u-space, clipped to [u_min, u_max].
  vector[K] lo = fmax(u_min, head(boundaries, K));
  vector[K] hi = fmin(u_max, tail(boundaries, K));

  // F_step is right-continuous and on [b_k, b_{k+1}) takes the value
  // sum_{j < k} pmf[j] (mass before bin k). cumulative_sum(pmf) gives
  // the mass through and including bin k, so we shift right by one.
  vector[K] cum_before;
  cum_before[1] = 0;
  if (K > 1) cum_before[2:K] = head(cumulative_sum(pmf), K - 1);

  // 0/1 mask drops bins with `hi <= lo` from the reduction without a
  // branch in the inner expression. Built on `data`-level inputs.
  vector[K] active;
  for (k in 1:K) active[k] = hi[k] > lo[k] ? 1 : 0;

  // F_primary at lo/hi via two vectorised calls; one masked subtraction
  // gives the per-bin difference for the dot product.
  vector[K] f_lo = primary_lcdf_vec(d - lo, primary_id, primary_params,
                                    pwindow);
  vector[K] f_hi = primary_lcdf_vec(d - hi, primary_id, primary_params,
                                    pwindow);
  vector[K] f_diff = (exp(f_lo) - exp(f_hi)) .* active;

  real integral = dot_product(cum_before, f_diff);

  // Tail region [boundaries[K+1], u_max]: F_step = 1, contributing
  // F_primary(d - tail_start) - F_primary(d - u_max).
  real tail_start = fmax(boundaries[K + 1], u_min);
  if (tail_start < u_max) {
    real fp_tail = exp(primary_lcdf(d - tail_start | primary_id,
                                    primary_params, pwindow));
    real fp_end = exp(primary_lcdf(d - u_max | primary_id,
                                   primary_params, pwindow));
    integral += fp_tail - fp_end;
  }

  return log(integral);
}
vector hazards_to_pmf(vector hazards) {
  int K = num_elements(hazards);
  vector[K] log_surv;
  log_surv[1] = 0;
  if (K > 1) {
    log_surv[2:K] = cumulative_sum(log1m(hazards[1:(K - 1)]));
  }
  return hazards .* exp(log_surv);
}
real discretehazard_lcdf(
  data real d, vector boundaries, vector hazards,
  int primary_id, array[] real primary_params, data real pwindow
) {
  return discretestep_lcdf(
    d | boundaries, hazards_to_pmf(hazards), primary_id, primary_params,
    pwindow
  );
}
real pstep_lcdf(real t, vector boundaries, vector pmf) {
  int K = num_elements(pmf);
  if (t < boundaries[2]) return negative_infinity();
  if (t >= boundaries[K + 1]) return 0;
  // Right-continuous CDF with jumps at the right edges
  // boundaries[2], ..., boundaries[K + 1]. F(t) = cum_pmf[k] for
  // t in [boundaries[k + 1], boundaries[k + 2]); equivalently the
  // largest k with boundaries[k + 1] <= t. Boundary-on-jump cases
  // (t == boundaries[k + 1]) advance k, matching R's
  // `findInterval(left.open = FALSE)`.
  int k = 1;
  while (k < K && boundaries[k + 2] <= t) k += 1;
  return log(cumulative_sum(pmf)[k]);
}
real phazard_lcdf(real t, vector boundaries, vector hazards) {
  return pstep_lcdf(t | boundaries, hazards_to_pmf(hazards));
}
int check_for_analytical(int dist_id, int primary_id) {
  if (dist_id == 2 && primary_id == 1) return 1; // Gamma, Uniform
  if (dist_id == 1 && primary_id == 1) return 1; // Lognormal, Uniform
  if (dist_id == 3 && primary_id == 1) return 1; // Weibull, Uniform
  if (dist_id == 5 && primary_id == 1) return 1; // Generalised gamma, Uniform
  // Keep this primary list in sync with `primary_lcdf`; see the note above.
  if (dist_id == 26 || dist_id == 27 || dist_id == 28) {
    return primary_id == 1 || primary_id == 2;
  }
  return 0; // No analytical solution for other combinations
}
real primarycensored_gamma_uniform_lcdf(data real d, real q, array[] real params, data real pwindow) {
  real shape = params[1];
  real rate = params[2];
  real log_window = log(pwindow);
  // log E where E = k * theta = shape / rate is the mean of the delay
  real log_E = log(shape) - log(rate);

  // F_T(d; k) and the recursion to F_T(d; k+1):
  // P(k+1, y) = P(k, y) - y^k e^{-y} / Gamma(k+1), with y = rate * d
  real log_F_T_d_k = gamma_lcdf(d | shape, rate);
  real gamma_kp1_pdf_log_d
    = shape * log(rate * d) - rate * d - lgamma(shape + 1);
  real log_F_T_d_kp1 = log_diff_exp(log_F_T_d_k, gamma_kp1_pdf_log_d);

  // q-dependent terms. Final algebra is unified; only a guard to avoid
  // log_diff_exp(-inf, -inf) and log(0) when q == 0 (q is data, so autodiff
  // is unaffected by this branch).
  real log_q_F_T_q;    // log(q * F_T(q; k))
  real log_E_tF_T_q;   // log(E * F_T(q; k+1))
  if (q > 0) {
    real log_F_T_q_k = gamma_lcdf(q | shape, rate);
    real gamma_kp1_pdf_log_q
      = shape * log(rate * q) - rate * q - lgamma(shape + 1);
    real log_F_T_q_kp1 = log_diff_exp(log_F_T_q_k, gamma_kp1_pdf_log_q);
    log_q_F_T_q = log(q) + log_F_T_q_k;
    log_E_tF_T_q = log_E + log_F_T_q_kp1;
  } else {
    log_q_F_T_q = negative_infinity();
    log_E_tF_T_q = negative_infinity();
  }

  // Unified form: F_{S+}(d) = (A - B) / w_P with A, B sums of positives:
  //   A = d * F_T(d; k)   + E * F_T(q; k+1)
  //   B = q * F_T(q; k)   + E * F_T(d; k+1)
  // Ordering A >= B is guaranteed by F_{S+}(d) >= 0.
  real log_A = log_sum_exp(log(d) + log_F_T_d_k, log_E_tF_T_q);
  real log_B = log_sum_exp(log_q_F_T_q, log_E + log_F_T_d_kp1);

  return log_diff_exp(log_A, log_B) - log_window;
}
real primarycensored_lognormal_uniform_lcdf(data real d, real q, array[] real params, data real pwindow) {
  real mu = params[1];
  real sigma = params[2];
  real mu_sigma2 = mu + square(sigma);
  real log_window = log(pwindow);
  // log E where E = exp(mu + sigma^2/2) is the mean of the delay
  real log_E = mu + 0.5 * square(sigma);

  // Each term is formed whole and dropped whole. Adding a `-inf` log CDF to
  // the parameter-dependent `log(d)` or `log_E` first would leave an edge
  // back to the parameters that `log_sum_exp` differentiates to
  // `exp(-inf - -inf)`. `q <= 0` underflows on the same test, so it needs no
  // separate branch.
  real log_d_F_T_d = lognormal_lcdf_underflows(d, mu, sigma)
                     ? negative_infinity()
                     : log(d) + lognormal_lcdf(d | mu, sigma);
  real log_E_tF_T_d = lognormal_lcdf_underflows(d, mu_sigma2, sigma)
                      ? negative_infinity()
                      : log_E + lognormal_lcdf(d | mu_sigma2, sigma);
  real log_q_F_T_q = lognormal_lcdf_underflows(q, mu, sigma)
                     ? negative_infinity()
                     : log(q) + lognormal_lcdf(q | mu, sigma);
  real log_E_tF_T_q = lognormal_lcdf_underflows(q, mu_sigma2, sigma)
                      ? negative_infinity()
                      : log_E + lognormal_lcdf(q | mu_sigma2, sigma);

  // Unified form: F_{S+}(d) = (A - B) / w_P with
  //   A = d * F_T(d) + E * tilde F_T(q)
  //   B = q * F_T(q) + E * tilde F_T(d)
  // Ordering A >= B is guaranteed by F_{S+}(d) >= 0.
  real log_A = log_sum_exp(log_d_F_T_d, log_E_tF_T_q);
  real log_B = log_sum_exp(log_q_F_T_q, log_E_tF_T_d);

  // Deep enough into the lower tail every term underflows together. Both
  // are then constant `-inf` and `log_diff_exp` would give NaN, so return
  // the limit directly.
  if (is_inf(log_A)) {
    return negative_infinity();
  }

  return log_diff_exp(log_A, log_B) - log_window;
}
real log_weibull_g(real t, real shape, real scale) {
  real x = pow(t * inv(scale), shape);
  real a = 1 + inv(shape);
  return log(gamma_p(a, x)) + lgamma(a);
}
real primarycensored_weibull_uniform_lcdf(data real d, real q, array[] real params, data real pwindow) {
  real shape = params[1];
  real scale = params[2];
  real log_window = log(pwindow);
  real log_scale = log(scale);

  // For Weibull: E = scale (lambda) and tilde F_T(t) = g(t; lambda, k), so
  // log(E * tilde F_T(t)) = log(scale) + log_weibull_g(t, shape, scale).
  real log_F_T_d = weibull_lcdf(d | shape, scale);
  real log_E_tF_T_d = log_scale + log_weibull_g(d, shape, scale);

  // q-dependent terms (guard only to avoid log(0); final algebra is unified).
  real log_q_F_T_q;    // log(q * F_T(q))
  real log_E_tF_T_q;   // log(E * tilde F_T(q)) = log(scale * g(q; lambda, k))
  if (q > 0) {
    log_q_F_T_q = log(q) + weibull_lcdf(q | shape, scale);
    log_E_tF_T_q = log_scale + log_weibull_g(q, shape, scale);
  } else {
    log_q_F_T_q = negative_infinity();
    log_E_tF_T_q = negative_infinity();
  }

  // Unified form: F_{S+}(d) = (A - B) / w_P with
  //   A = d * F_T(d)    + scale * g(q; lambda, k)
  //   B = q * F_T(q)    + scale * g(d; lambda, k)
  // Ordering A >= B is guaranteed by F_{S+}(d) >= 0.
  real log_A = log_sum_exp(log(d) + log_F_T_d, log_E_tF_T_q);
  real log_B = log_sum_exp(log_q_F_T_q, log_E_tF_T_d);

  return log_diff_exp(log_A, log_B) - log_window;
}
real primarycensored_gengamma_uniform_lcdf(data real d, real q, array[] real params, data real pwindow) {
  real shape = params[1];
  real scale = params[2];
  real k = params[3];
  real k_shift = k + inv(shape);
  real log_window = log(pwindow);
  // log E where E = scale * Gamma(k + 1/shape) / Gamma(k) is the mean of the
  // delay
  real log_E = log(scale) + lgamma(k_shift) - lgamma(k);

  real log_F_T_d = gengamma_lcdf(d | shape, scale, k);
  real log_tF_T_d = gengamma_lcdf(d | shape, scale, k_shift);

  // q-dependent terms (guard only to avoid log(0); final algebra is unified).
  real log_q_F_T_q;    // log(q * F_T(q))
  real log_E_tF_T_q;   // log(E * tilde F_T(q))
  if (q > 0) {
    log_q_F_T_q = log(q) + gengamma_lcdf(q | shape, scale, k);
    log_E_tF_T_q = log_E + gengamma_lcdf(q | shape, scale, k_shift);
  } else {
    log_q_F_T_q = negative_infinity();
    log_E_tF_T_q = negative_infinity();
  }

  // Unified form: F_{S+}(d) = (A - B) / w_P with
  //   A = d * F_T(d) + E * tilde F_T(q)
  //   B = q * F_T(q) + E * tilde F_T(d)
  // Ordering A >= B is guaranteed by F_{S+}(d) >= 0.
  real log_A = log_sum_exp(log(d) + log_F_T_d, log_E_tF_T_q);
  real log_B = log_sum_exp(log_q_F_T_q, log_E + log_tF_T_d);

  return log_diff_exp(log_A, log_B) - log_window;
}
real primarycensored_analytical_lcdf_raw(data real d, int dist_id,
                                         array[] real params,
                                         data real pwindow,
                                         int primary_id,
                                         array[] real primary_params) {
  real q = max({d - pwindow, 0});

  if (dist_id == 2 && primary_id == 1) {
    return primarycensored_gamma_uniform_lcdf(d | q, params, pwindow);
  } else if (dist_id == 1 && primary_id == 1) {
    return primarycensored_lognormal_uniform_lcdf(d | q, params, pwindow);
  } else if (dist_id == 3 && primary_id == 1) {
    return primarycensored_weibull_uniform_lcdf(d | q, params, pwindow);
  } else if (dist_id == 5 && primary_id == 1) {
    return primarycensored_gengamma_uniform_lcdf(d | q, params, pwindow);
  } else if (dist_id == 26) {
    // params = [boundaries (K+1), pmf (K)]; length 2*K + 1.
    int K = (size(params) - 1) %/% 2;
    return discretestep_lcdf(
      d | to_vector(segment(params, 1, K + 1)),
          to_vector(segment(params, K + 2, K)),
          primary_id, primary_params, pwindow
    );
  } else if (dist_id == 27 || dist_id == 28) {
    // params = [boundaries (K+1), hazards (K)]; length 2*K + 1. The last
    // hazard must equal 1. RW (27) and RE (28) only differ in their
    // prior so they share this likelihood dispatch.
    int K = (size(params) - 1) %/% 2;
    return discretehazard_lcdf(
      d | to_vector(segment(params, 1, K + 1)),
          to_vector(segment(params, K + 2, K)),
          primary_id, primary_params, pwindow
    );
  }
  return negative_infinity();
}
real primarycensored_analytical_lcdf(data real d, int dist_id,
                                           array[] real params,
                                           data real pwindow, data real L,
                                           data real D, int primary_id,
                                           array[] real primary_params) {
  if (d <= L) return negative_infinity();
  if (d >= D) return 0;

  real result = primarycensored_analytical_lcdf_raw(
    d, dist_id, params, pwindow, primary_id, primary_params
  );

  // Apply truncation normalization
  if (!is_inf(D) || L > 0) {
    vector[2] bounds = primarycensored_truncation_bounds(
      L, D, dist_id, params, pwindow, primary_id, primary_params
    );
    real log_cdf_L = bounds[1];
    real log_cdf_D = bounds[2];

    real log_normalizer = primarycensored_log_normalizer(log_cdf_D, log_cdf_L, L);
    result = primarycensored_apply_truncation(result, log_cdf_L, log_normalizer, L);
  }

  return result;
}
real primarycensored_analytical_cdf(data real d, int dist_id,
                                          array[] real params,
                                          data real pwindow, data real L,
                                          data real D, int primary_id,
                                          array[] real primary_params) {
  return exp(primarycensored_analytical_lcdf(d | dist_id, params, pwindow, L, D, primary_id, primary_params));
}
int dist_has_positive_support(data int dist_id) {
  if (dist_id == 1) return 1;   // Lognormal
  if (dist_id == 2) return 1;   // Gamma
  if (dist_id == 3) return 1;   // Weibull
  if (dist_id == 4) return 1;   // Exponential
  if (dist_id == 5) return 1;   // Generalised gamma
  if (dist_id == 9) return 1;   // Beta (support on [0, 1])
  if (dist_id == 13) return 1;  // Chi-square
  if (dist_id == 16) return 1;  // Inverse Gamma
  if (dist_id == 19) return 1;  // Inverse Chi-square
  if (dist_id == 21) return 1;  // Pareto
  if (dist_id == 22) return 1;  // Scaled inverse Chi-square
  return 0;
}
real primary_lcdf(real p, int primary_id, array[] real primary_params,
                  data real pwindow) {
  if (primary_id == 1) {
    // Uniform on [0, pwindow]: built-in uniform_lcdf matches the package
    // primary semantics over [0, pwindow].
    if (p <= 0) return negative_infinity();
    if (p >= pwindow) return 0;
    return uniform_lcdf(p | 0, pwindow);
  } else if (primary_id == 2) {
    return expgrowth_lcdf(p | 0, pwindow, primary_params[1]);
  }
  reject("primary_lcdf: unsupported primary_id ", primary_id);
}
int lognormal_lcdf_underflows(real y, real mu, real sigma) {
  return (y <= 0 || (log(y) - mu) / sigma < -38) ? 1 : 0;
}
real gengamma_lcdf(real y, real shape, real scale, real k) {
  return gamma_lcdf(pow(y / scale, shape) | k, 1);
}
real dist_lcdf(real delay, array[] real params, int dist_id) {
  if (dist_has_positive_support(dist_id) && delay <= 0) {
    return negative_infinity();
  }

  // IDs match pcd_distributions$stan_id in R
  // Guarded so a lower-tail underflow cannot put a NaN partial on the tape.
  // The downstream `exp(-inf)` differentiates to 0.
  if (dist_id == 1) {
    return lognormal_lcdf_underflows(delay, params[1], params[2])
           ? negative_infinity()
           : lognormal_lcdf(delay | params[1], params[2]);
  }
  else if (dist_id == 2) return gamma_lcdf(delay | params[1], params[2]);
  else if (dist_id == 3) return weibull_lcdf(delay | params[1], params[2]);
  else if (dist_id == 4) return exponential_lcdf(delay | params[1]);
  else if (dist_id == 5) return gengamma_lcdf(delay | params[1], params[2], params[3]);
  else if (dist_id == 9) return beta_lcdf(delay | params[1], params[2]);
  else if (dist_id == 12) return cauchy_lcdf(delay | params[1], params[2]);
  else if (dist_id == 13) return chi_square_lcdf(delay | params[1]);
  else if (dist_id == 15) return gumbel_lcdf(delay | params[1], params[2]);
  else if (dist_id == 16) return inv_gamma_lcdf(delay | params[1], params[2]);
  else if (dist_id == 17) return logistic_lcdf(delay | params[1], params[2]);
  else if (dist_id == 18) return normal_lcdf(delay | params[1], params[2]);
  else if (dist_id == 19) return inv_chi_square_lcdf(delay | params[1]);
  else if (dist_id == 20) return double_exponential_lcdf(delay | params[1], params[2]);
  else if (dist_id == 21) return pareto_lcdf(delay | params[1], params[2]);
  else if (dist_id == 22) return scaled_inv_chi_square_lcdf(delay | params[1], params[2]);
  else if (dist_id == 23) return student_t_lcdf(delay | params[1], params[2], params[3]);
  else if (dist_id == 24) return uniform_lcdf(delay | params[1], params[2]);
  else if (dist_id == 25) return von_mises_lcdf(delay | params[1], params[2]);
  else if (dist_id == 26) {
    // Non-parametric step: params = [boundaries (K+1), pmf (K)].
    int K = (size(params) - 1) %/% 2;
    return pstep_lcdf(
      delay | to_vector(segment(params, 1, K + 1)),
              to_vector(segment(params, K + 2, K))
    );
  }
  else if (dist_id == 27 || dist_id == 28) {
    // Non-parametric discrete hazard: params = [boundaries (K+1),
    // hazards (K)] with hazards[K] = 1. RW (27) and RE (28) share the
    // same likelihood; they only differ in the prior.
    int K = (size(params) - 1) %/% 2;
    return phazard_lcdf(
      delay | to_vector(segment(params, 1, K + 1)),
              to_vector(segment(params, K + 2, K))
    );
  }
  else reject("Invalid distribution identifier: ", dist_id);
}
real primary_lpdf(real x, int primary_id, array[] real params, real xmin, real xmax) {
  // Implement switch for different primary distributions
  if (primary_id == 1) return uniform_lpdf(x | xmin, xmax);
  if (primary_id == 2) return expgrowth_lpdf(x | xmin, xmax, params[1]);
  // Add more primary distributions as needed
  reject("Invalid primary distribution identifier");
}
vector primarycensored_ode(real t, vector y, array[] real theta,
                            array[] real x_r, array[] int x_i) {
  real d = x_r[1];
  int dist_id = x_i[1];
  int primary_id = x_i[2];
  real pwindow = x_r[2];
  int dist_params_len = x_i[3];
  int primary_params_len = x_i[4];

  // Extract distribution parameters
  array[dist_params_len] real params;
  if (dist_params_len) {
    params = theta[1:dist_params_len];
  }
  array[primary_params_len] real primary_params;
  if (primary_params_len) {
    int primary_loc = num_elements(theta);
    primary_params = theta[primary_loc - primary_params_len + 1:primary_loc];
  }

  real log_cdf = dist_lcdf(t | params, dist_id);
  real log_primary_pdf = primary_lpdf(d - t | primary_id, primary_params, 0, pwindow);

  return rep_vector(exp(log_cdf + log_primary_pdf), 1);
}
real primarycensored_log_normalizer(real log_cdf_D, real log_cdf_L, real L) {
  if (!is_inf(L)) {
    return log_diff_exp(log_cdf_D, log_cdf_L);
  } else {
    return log_cdf_D;
  }
}
real primarycensored_apply_truncation(real log_cdf, real log_cdf_L,
                                      real log_normalizer, real L) {
  if (!is_inf(L)) {
    return log_diff_exp(log_cdf, log_cdf_L) - log_normalizer;
  } else {
    return log_cdf - log_normalizer;
  }
}
vector primarycensored_truncation_bounds(
  data real L, data real D,
  data int dist_id, array[] real params, data real pwindow,
  data int primary_id, array[] real primary_params
) {
  vector[2] result;
  // Internal lower bound for the un-truncated distribution: 0 lets the
  // `d <= L` early-exit in primarycensored_lcdf return -inf for delays below
  // the natural support of positive-support distributions; -inf disables that
  // short-circuit so distributions with support on the reals are integrated.
  // Expression is inlined (rather than bound to a local) so Stan's data-flow
  // checker recognises it as data-only.

  // Get CDF at lower truncation point L
  if (is_inf(L)) {
    result[1] = negative_infinity();
  } else {
    result[1] = primarycensored_lcdf(
      L | dist_id, params, pwindow,
      dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
      positive_infinity(), primary_id, primary_params
    );
  }

  // Get CDF at upper truncation point D
  if (is_inf(D)) {
    result[2] = 0;
  } else {
    result[2] = primarycensored_lcdf(
      D | dist_id, params, pwindow,
      dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
      positive_infinity(), primary_id, primary_params
    );
  }

  return result;
}
real primarycensored_cdf(data real d, data int dist_id, array[] real params,
                               data real pwindow, data real L, data real D,
                               data int primary_id,
                               array[] real primary_params) {
  real result;
  if (d <= L) {
    return 0;
  }

  if (d >= D) {
    return 1;
  }

  // Check if an analytical solution exists
  if (check_for_analytical(dist_id, primary_id)) {
    // Use analytical solution
    result = primarycensored_analytical_cdf(
      d | dist_id, params, pwindow, L, D, primary_id, primary_params
    );
  } else {
    // Use numerical integration for other cases. The integration variable
    // ranges over the primary-event time, so the natural lower bound is
    // d - pwindow. For positive-support delays the integrand `F_delay(t)` is
    // 0 for t <= 0, so an unclipped lower bound just adds a flat zero region
    // for negative t. Distributions with support on the reals also accept the
    // unclipped lower bound directly.
    real lower_bound = d - pwindow;
    int n_params = num_elements(params);
    int n_primary_params = num_elements(primary_params);
    array[n_params + n_primary_params] real theta = append_array(params, primary_params);
    array[4] int ids = {dist_id, primary_id, n_params, n_primary_params};

    vector[1] y0 = rep_vector(0.0, 1);
    result = ode_rk45(primarycensored_ode, y0, lower_bound, {d}, theta, {d, pwindow}, ids)[1, 1];

    // Apply truncation normalization on log scale for numerical stability.
    // Skip when F(L) = 0 makes it a no-op (positive support, L <= 0).
    if (!is_inf(D) || L > 0 ||
        (!is_inf(L) && !dist_has_positive_support(dist_id))) {
      real log_result = log(result);
      vector[2] bounds = primarycensored_truncation_bounds(
        L, D, dist_id, params, pwindow, primary_id, primary_params
      );
      real log_cdf_L = bounds[1];
      real log_cdf_D = bounds[2];

      real log_normalizer = primarycensored_log_normalizer(log_cdf_D, log_cdf_L, L);
      log_result = primarycensored_apply_truncation(
        log_result, log_cdf_L, log_normalizer, L
      );
      result = exp(log_result);
    }
  }

  return result;
}
real primarycensored_lcdf(data real d, data int dist_id, array[] real params,
                                data real pwindow, data real L, data real D,
                                data int primary_id,
                                array[] real primary_params) {
  real result;

  if (d <= L) {
    return negative_infinity();
  }

  if (d >= D) {
    return 0;
  }

  // Check if an analytical solution exists. The internal lower bound is 0 for
  // positive-support delays (lets the d <= L early-exit return -inf for d <= 0)
  // and -inf for distributions with support on the reals.
  if (check_for_analytical(dist_id, primary_id)) {
    result = primarycensored_analytical_lcdf(
      d | dist_id, params, pwindow,
      dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
      positive_infinity(), primary_id, primary_params
    );
  } else {
    // Use numerical integration
    result = log(primarycensored_cdf(
      d | dist_id, params, pwindow,
      dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
      positive_infinity(), primary_id, primary_params
    ));
  }

  // Handle truncation normalization. Skip when F(L) = 0 makes it a no-op
  // (positive support, L <= 0) to avoid the cancelling log_diff_exp.
  if (!is_inf(D) || L > 0 ||
      (!is_inf(L) && !dist_has_positive_support(dist_id))) {
    vector[2] bounds = primarycensored_truncation_bounds(
      L, D, dist_id, params, pwindow, primary_id, primary_params
    );
    real log_cdf_L = bounds[1];
    real log_cdf_D = bounds[2];

    real log_normalizer = primarycensored_log_normalizer(log_cdf_D, log_cdf_L, L);
    result = primarycensored_apply_truncation(result, log_cdf_L, log_normalizer, L);
  }

  return result;
}
real primarycensored_lpmf(data int d, data int dist_id, array[] real params,
                                data real pwindow, data real d_upper,
                                data real L, data real D, data int primary_id,
                                array[] real primary_params) {
  if (d_upper > D) {
    reject("Upper truncation point is greater than D. It is ", d_upper,
           " and D is ", D, ". Resolve this by increasing D to be greater or equal to d + swindow or decreasing swindow.");
  }
  if (d_upper <= d) {
    reject("Upper truncation point is less than or equal to d. It is ", d_upper,
           " and d is ", d, ". Resolve this by increasing d to be less than d_upper.");
  }
  if (d < L) {
    return negative_infinity();
  }
  real log_cdf_upper = primarycensored_lcdf(
    d_upper | dist_id, params, pwindow,
    dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
    positive_infinity(), primary_id, primary_params
  );
  real log_cdf_lower = primarycensored_lcdf(
    d | dist_id, params, pwindow,
    dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
    positive_infinity(), primary_id, primary_params
  );

  // Apply truncation normalization: log((F(d_upper) - F(d)) / (F(D) - F(L))).
  // Skip when F(L) = 0 makes it a no-op (positive support, L <= 0).
  if (!is_inf(D) || L > 0 ||
      (!is_inf(L) && !dist_has_positive_support(dist_id))) {
    real log_cdf_D;
    real log_cdf_L;

    // Get CDF at lower truncation point L
    if (is_inf(L)) {
      // No left truncation (L = -inf sentinel)
      log_cdf_L = negative_infinity();
    } else if (d == L) {
      // Reuse already computed CDF at d
      log_cdf_L = log_cdf_lower;
    } else {
      // Compute CDF at L directly
      log_cdf_L = primarycensored_lcdf(
        L | dist_id, params, pwindow,
        dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
        positive_infinity(), primary_id, primary_params
      );
    }

    // Get CDF at upper truncation point D
    if (d_upper == D) {
      log_cdf_D = log_cdf_upper;
    } else if (is_inf(D)) {
      log_cdf_D = 0;
    } else {
      log_cdf_D = primarycensored_lcdf(
        D | dist_id, params, pwindow,
        dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
        positive_infinity(), primary_id, primary_params
      );
    }

    real log_normalizer = primarycensored_log_normalizer(log_cdf_D, log_cdf_L, L);
    return log_diff_exp(log_cdf_upper, log_cdf_lower) - log_normalizer;
  } else {
    return log_diff_exp(log_cdf_upper, log_cdf_lower);
  }
}
vector primarycensored_sone_lpmf_vectorized(
  data int max_delay, data real L, data real D, data int dist_id,
  array[] real params, data real pwindow,
  data int primary_id, array[] real primary_params
) {

  int upper_interval = max_delay + 1;
  vector[upper_interval] log_pmfs;
  vector[upper_interval] log_cdfs;
  real log_normalizer;

  // Check if D is at least max_delay + 1
  if (D < upper_interval) {
    reject("D must be at least max_delay + 1");
  }

  // Compute log CDFs (without truncation normalization). The internal lower
  // bound below is 0 for positive-support delays and -inf otherwise; it is
  // inlined rather than bound to a local so Stan's type checker treats it as
  // data-only.
  // Start from max(1, floor(L)) to avoid computing unused CDFs when L > 0;
  // for L <= 0 (including -inf) start at 1 since F(d) = 0 for d <= 0.
  int start_idx = (!is_inf(L) && L > 0) ? max(1, to_int(floor(L))) : 1;
  for (d in start_idx:upper_interval) {
    log_cdfs[d] = primarycensored_lcdf(
      d | dist_id, params, pwindow,
      dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
      positive_infinity(), primary_id, primary_params
    );
  }

  // Get CDF at lower truncation point L
  real log_cdf_L;
  if (is_inf(L)) {
    // No left truncation (L = -inf sentinel)
    log_cdf_L = negative_infinity();
  } else if (L >= 1 && L <= upper_interval && floor(L) == L) {
    // L is a positive integer within computed range, reuse cached value
    log_cdf_L = log_cdfs[to_int(L)];
  } else {
    // L is outside computed range or non-integer, compute directly
    log_cdf_L = primarycensored_lcdf(
      L | dist_id, params, pwindow,
      dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
      positive_infinity(), primary_id, primary_params
    );
  }

  // Compute log normalizer: log(F(D) - F(L))
  real log_cdf_D;
  if (D > upper_interval) {
    if (is_inf(D)) {
      log_cdf_D = 0; // log(1) = 0 for infinite D
    } else {
      log_cdf_D = primarycensored_lcdf(
        D | dist_id, params, pwindow,
        dist_has_positive_support(dist_id) ? 0.0 : negative_infinity(),
        positive_infinity(), primary_id, primary_params
      );
    }
  } else {
    log_cdf_D = log_cdfs[upper_interval];
  }

  log_normalizer = primarycensored_log_normalizer(log_cdf_D, log_cdf_L, L);

  // Compute log PMFs: log((F(d) - F(d-1)) / (F(D) - F(L)))
  for (d in 1:upper_interval) {
    if (d <= L) {
      // Delay interval [d-1, d) is entirely at or below L
      log_pmfs[d] = negative_infinity();
    } else if (d - 1 < L) {
      // L falls within interval [d-1, d), so compute mass in [L, d)
      log_pmfs[d] = log_diff_exp(log_cdfs[d], log_cdf_L) - log_normalizer;
    } else if (d == 1 && dist_has_positive_support(dist_id)) {
      // First interval [0, 1) with L <= 0 and positive-support delay:
      // F(0) = 0, so PMF = F(1) / normalizer
      log_pmfs[d] = log_cdfs[d] - log_normalizer;
    } else if (d == 1) {
      // First interval [0, 1) with L <= 0 and real-support delay: F(0) is
      // non-zero in general, so compute it explicitly.
      real log_cdf_0 = primarycensored_lcdf(
        0.0 | dist_id, params, pwindow,
        negative_infinity(), positive_infinity(),
        primary_id, primary_params
      );
      log_pmfs[d] = log_diff_exp(log_cdfs[d], log_cdf_0) - log_normalizer;
    } else {
      // Standard case: PMF = (F(d) - F(d-1)) / normalizer
      log_pmfs[d] = log_diff_exp(log_cdfs[d], log_cdfs[d-1]) - log_normalizer;
    }
  }

  return log_pmfs;
}
vector primarycensored_sone_pmf_vectorized(
  data int max_delay, data real L, data real D, data int dist_id,
  array[] real params, data real pwindow,
  data int primary_id,
  array[] real primary_params
) {
  return exp(
    primarycensored_sone_lpmf_vectorized(
      max_delay, L, D, dist_id, params, pwindow, primary_id, primary_params
    )
  );
}
