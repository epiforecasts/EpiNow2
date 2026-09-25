// Log density through update_Rt_ref() (version = 0) or update_Rt()
// (version = 1), for comparing gradients with $grad_log_prob().
functions {
#include update_rt_ref.stan
#include rt.stan
}
data {
  int t;
  int n_centre;
  int gp_n;
  int bp_n;
  int stationary;
  array[t] int bps;
  vector[t] w;
  int version;
}
parameters {
  real<lower=0> R0;
  vector[gp_n] noise;
  vector[bp_n] bp_effects;
}
model {
  vector[t] R;
  if (version) {
    R = update_Rt(t, R0, noise, bps, bp_effects, stationary, n_centre);
  } else {
    R = update_Rt_ref(t, R0, noise, bps, bp_effects, stationary, n_centre);
  }
  target += dot_product(w, log(R)) + dot_product(w, R);
}
