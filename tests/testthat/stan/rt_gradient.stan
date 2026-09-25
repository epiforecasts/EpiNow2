// Test model comparing the C++ update_Rt() with the pure Stan reference.
// Each of R0 (on the log scale), noise and bp_effects is a parameter or
// data, so every var/double combination is reached.
functions {
#include functions/rt.stan
#include rt_reference.stan

  // ctrl holds t, stationary, n_centre and use_cpp.
  vector urt(real R0, vector noise, array[] int bps, vector bp_effects,
             array[] int ctrl) {
    if (ctrl[4]) {
      return update_Rt(ctrl[1], R0, noise, bps, bp_effects, ctrl[2], ctrl[3]);
    }
    return update_Rt_stan(
      ctrl[1], R0, noise, bps, bp_effects, ctrl[2], ctrl[3]
    );
  }
}

data {
  int t;
  int stationary;
  int n_centre;
  int gp_n;
  int bp_n;
  array[t] int bps;
  real R0_data;
  vector[gp_n] noise_data;
  vector[bp_n] bp_data;
  vector[t] r;
  int<lower = 0, upper = 1> R0_param;
  int<lower = 0, upper = 1> noise_param;
  int<lower = 0, upper = 1> bp_param;
  int<lower = 0, upper = 1> use_cpp;
}

transformed data {
  array[4] int ctrl = {t, stationary, n_centre, use_cpp};
}

parameters {
  array[R0_param] real log_R0;
  vector[noise_param ? gp_n : 0] noise_p;
  vector[bp_param ? bp_n : 0] bp_p;
}

model {
  vector[t] R;
  if (R0_param && noise_param && bp_param) {
    R = urt(exp(log_R0[1]), noise_p, bps, bp_p, ctrl);
  } else if (R0_param && noise_param && !bp_param) {
    R = urt(exp(log_R0[1]), noise_p, bps, bp_data, ctrl);
  } else if (R0_param && !noise_param && bp_param) {
    R = urt(exp(log_R0[1]), noise_data, bps, bp_p, ctrl);
  } else if (R0_param && !noise_param && !bp_param) {
    R = urt(exp(log_R0[1]), noise_data, bps, bp_data, ctrl);
  } else if (!R0_param && noise_param && bp_param) {
    R = urt(R0_data, noise_p, bps, bp_p, ctrl);
  } else if (!R0_param && noise_param && !bp_param) {
    R = urt(R0_data, noise_p, bps, bp_data, ctrl);
  } else if (!R0_param && !noise_param && bp_param) {
    R = urt(R0_data, noise_data, bps, bp_p, ctrl);
  }
  target += dot_product(r, R);
}

generated quantities {
  vector[t] R_data = urt(R0_data, noise_data, bps, bp_data, ctrl);
}
