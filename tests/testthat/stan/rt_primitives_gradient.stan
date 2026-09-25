// Test model comparing the C++ primitives used by update_Rt() with pure
// Stan versions. Each of c, levels and x is either a parameter or data, so
// every var/double combination is reached. fn picks the primitive:
// 1 exp_add(), 2 exp_add_indexed(), 3 cumsum_hold().
functions {
#include functions/rt.stan

  vector exp_add_stan(real c, vector x) {
    return exp(c + x);
  }

  vector exp_add_indexed_stan(real c, vector levels, array[] int idx,
                              vector x) {
    return exp(c + levels[idx] + x);
  }

  vector cumsum_hold_stan(vector x, int t) {
    return hold_forward(append_row(0, cumulative_sum(x)), t);
  }

  // ctrl holds fn, t and use_cpp
  vector prim(real c, vector levels, array[] int idx, vector x,
              array[] int ctrl) {
    if (ctrl[1] == 1) {
      if (ctrl[3]) {
        return exp_add(c, x);
      }
      return exp_add_stan(c, x);
    }
    if (ctrl[1] == 2) {
      if (ctrl[3]) {
        return exp_add_indexed(c, levels, idx, x);
      }
      return exp_add_indexed_stan(c, levels, idx, x);
    }
    if (ctrl[3]) {
      return cumsum_hold(x, ctrl[2]);
    }
    return cumsum_hold_stan(x, ctrl[2]);
  }
}

data {
  int fn;
  int t;
  int n;
  int L;
  int len;
  array[n] int idx;
  real c_data;
  vector[L] levels_data;
  vector[n] x_data;
  vector[len] r;
  int<lower = 0, upper = 1> c_param;
  int<lower = 0, upper = 1> levels_param;
  int<lower = 0, upper = 1> x_param;
  int<lower = 0, upper = 1> use_cpp;
}

transformed data {
  array[3] int ctrl = {fn, t, use_cpp};
}

parameters {
  array[c_param] real c_p;
  vector[levels_param ? L : 0] levels_p;
  vector[x_param ? n : 0] x_p;
}

model {
  vector[len] z;
  if (c_param && levels_param && x_param) {
    z = prim(c_p[1], levels_p, idx, x_p, ctrl);
  } else if (c_param && levels_param) {
    z = prim(c_p[1], levels_p, idx, x_data, ctrl);
  } else if (c_param && x_param) {
    z = prim(c_p[1], levels_data, idx, x_p, ctrl);
  } else if (c_param) {
    z = prim(c_p[1], levels_data, idx, x_data, ctrl);
  } else if (levels_param && x_param) {
    z = prim(c_data, levels_p, idx, x_p, ctrl);
  } else if (levels_param) {
    z = prim(c_data, levels_p, idx, x_data, ctrl);
  } else {
    z = prim(c_data, levels_data, idx, x_p, ctrl);
  }
  target += dot_product(r, z);
}
