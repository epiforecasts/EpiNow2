// Profiles update_Rt_ref() (the reference) against update_Rt() from
// inst/stan/functions/rt.stan in the same model.
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
  int reps;
}
parameters {
  real<lower=0> R0;
  vector[gp_n] noise;
  vector[bp_n] bp_effects;
}
transformed parameters {
  real acc = 0;
  profile("current") {
    for (k in 1:reps) {
      acc += sum(update_Rt_ref(
        t, R0, noise, bps, bp_effects, stationary, n_centre
      )) * 1e-9;
    }
  }
  profile("rewrite") {
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
  bp_effects ~ normal(0, 0.1);
  target += acc;
}
generated quantities {
  real maxdiff = max(abs(
    update_Rt_ref(t, R0, noise, bps, bp_effects, stationary, n_centre) -
      update_Rt(t, R0, noise, bps, bp_effects, stationary, n_centre)
  ));
}
