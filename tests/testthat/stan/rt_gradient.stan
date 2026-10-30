// Test model comparing the C++ R_to_r() with the pure Stan reference.
// R and gt are each a parameter (on the log scale) or data, so every
// var/double combination is reached.
functions {
#include functions/rt.stan
#include rt_reference.stan

  real r2r(real R, vector gt, data real abs_tol, int use_cpp) {
    if (use_cpp) {
      return R_to_r(R, gt, abs_tol);
    }
    return R_to_r_stan(R, gt, abs_tol);
  }
}

data {
  int G;
  real R_data;
  vector[G] gt_data;
  real abs_tol;
  real w;
  int<lower = 0, upper = 1> R_param;
  int<lower = 0, upper = 1> gt_param;
  int<lower = 0, upper = 1> use_cpp;
}

parameters {
  array[R_param] real log_R;
  vector[gt_param ? G : 0] log_gt;
}

model {
  real r;
  if (R_param && gt_param) {
    r = r2r(exp(log_R[1]), exp(log_gt), abs_tol, use_cpp);
  } else if (R_param) {
    r = r2r(exp(log_R[1]), gt_data, abs_tol, use_cpp);
  } else {
    r = r2r(R_data, exp(log_gt), abs_tol, use_cpp);
  }
  target += w * r;
}
