// Profiles the pure Stan update_Rt() against the C++ version in one model.
// Compile with include paths inst/stan and tests/testthat/stan and the
// package header as user_header.
functions {
#include functions/rt.stan
#include rt_reference.stan
}

data {
  int t;
  int n_centre;
  int stationary;
  int gp_n;
  int bp_n;
  array[t] int bps;
  int reps;
}

parameters {
  real<lower = 0> R0;
  vector[gp_n] noise;
  vector[bp_n] bp_effects;
}

transformed parameters {
  real acc = 0;
  profile("stan") {
    for (k in 1:reps) {
      acc += sum(update_Rt_stan(
        t, R0, noise, bps, bp_effects, stationary, n_centre
      )) * 1e-9;
    }
  }
  profile("cpp") {
    for (k in 1:reps) {
      acc += sum(update_Rt(
        t, R0, noise, bps, bp_effects, stationary, n_centre
      )) * 1e-9;
    }
  }
}

model {
  R0 ~ lognormal(0, 0.2);
  noise ~ normal(0, 0.01);
  bp_effects ~ normal(0, 0.05);
  target += acc;
}

generated quantities {
  real maxdiff = max(abs(
    update_Rt_stan(t, R0, noise, bps, bp_effects, stationary, n_centre) -
      update_Rt(t, R0, noise, bps, bp_effects, stationary, n_centre)
  ));
}
