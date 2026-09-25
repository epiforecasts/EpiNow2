// Plain Stan rewrite of update_Rt() from the update-rt-stan branch (commit
// 135b3d0e), renamed, for timing against the C++ version.
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

vector update_Rt_rewrite(int t, real R0, vector noise, array[] int bps,
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
    } else {
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
      return R;
    }
    // Hold the last estimate into the forecast horizon
    if (t > gp_n) {
      R[(gp_n + 1):t] = rep_vector(R[gp_n], t - gp_n);
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
