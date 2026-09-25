// Test model comparing the C++ renewal_infections() with the pure Stan
// reference. Each of seed, R, gt and pop is a parameter (on the log scale)
// or data, so every var/double combination is reached.
functions {
#include functions/rt.stan
#include functions/infections.stan
#include renewal_reference.stan

  // ctrl holds use_pop, nht and use_cpp.
  vector ren(vector seed, vector R, vector gt, real pop, data real pop_floor,
             array[] int ctrl) {
    if (ctrl[3]) {
      return renewal_infections(seed, R, gt, pop, ctrl[1], pop_floor, ctrl[2]);
    }
    return renewal_infections_stan(
      seed, R, gt, pop, ctrl[1], pop_floor, ctrl[2]
    );
  }
}

data {
  int uot;
  int ot;
  int G;
  int use_pop;
  real pop_floor;
  int nht;
  vector[uot] seed_data;
  vector[ot] R_data;
  vector[G] gt_data;
  real pop_data;
  vector[uot + ot] r;
  int<lower = 0, upper = 1> seed_param;
  int<lower = 0, upper = 1> R_param;
  int<lower = 0, upper = 1> gt_param;
  int<lower = 0, upper = 1> pop_param;
  int<lower = 0, upper = 1> use_cpp;
}

transformed data {
  array[3] int ctrl = {use_pop, nht, use_cpp};
}

parameters {
  vector[seed_param ? uot : 0] log_seed;
  vector[R_param ? ot : 0] log_R;
  vector[gt_param ? G : 0] log_gt;
  array[pop_param] real log_pop;
}

model {
  vector[uot + ot] z;
  if (seed_param && R_param && gt_param && pop_param) {
    z = ren(exp(log_seed), exp(log_R), exp(log_gt), exp(log_pop[1]),
            pop_floor, ctrl);
  } else if (seed_param && R_param && gt_param && !pop_param) {
    z = ren(exp(log_seed), exp(log_R), exp(log_gt), pop_data,
            pop_floor, ctrl);
  } else if (seed_param && R_param && !gt_param && pop_param) {
    z = ren(exp(log_seed), exp(log_R), gt_data, exp(log_pop[1]),
            pop_floor, ctrl);
  } else if (seed_param && R_param && !gt_param && !pop_param) {
    z = ren(exp(log_seed), exp(log_R), gt_data, pop_data,
            pop_floor, ctrl);
  } else if (seed_param && !R_param && gt_param && pop_param) {
    z = ren(exp(log_seed), R_data, exp(log_gt), exp(log_pop[1]),
            pop_floor, ctrl);
  } else if (seed_param && !R_param && gt_param && !pop_param) {
    z = ren(exp(log_seed), R_data, exp(log_gt), pop_data,
            pop_floor, ctrl);
  } else if (seed_param && !R_param && !gt_param && pop_param) {
    z = ren(exp(log_seed), R_data, gt_data, exp(log_pop[1]),
            pop_floor, ctrl);
  } else if (seed_param && !R_param && !gt_param && !pop_param) {
    z = ren(exp(log_seed), R_data, gt_data, pop_data,
            pop_floor, ctrl);
  } else if (!seed_param && R_param && gt_param && pop_param) {
    z = ren(seed_data, exp(log_R), exp(log_gt), exp(log_pop[1]),
            pop_floor, ctrl);
  } else if (!seed_param && R_param && gt_param && !pop_param) {
    z = ren(seed_data, exp(log_R), exp(log_gt), pop_data,
            pop_floor, ctrl);
  } else if (!seed_param && R_param && !gt_param && pop_param) {
    z = ren(seed_data, exp(log_R), gt_data, exp(log_pop[1]),
            pop_floor, ctrl);
  } else if (!seed_param && R_param && !gt_param && !pop_param) {
    z = ren(seed_data, exp(log_R), gt_data, pop_data,
            pop_floor, ctrl);
  } else if (!seed_param && !R_param && gt_param && pop_param) {
    z = ren(seed_data, R_data, exp(log_gt), exp(log_pop[1]),
            pop_floor, ctrl);
  } else if (!seed_param && !R_param && gt_param && !pop_param) {
    z = ren(seed_data, R_data, exp(log_gt), pop_data,
            pop_floor, ctrl);
  } else if (!seed_param && !R_param && !gt_param && pop_param) {
    z = ren(seed_data, R_data, gt_data, exp(log_pop[1]),
            pop_floor, ctrl);
  }
  target += dot_product(r, log1p(z));
}

generated quantities {
  vector[uot + ot] z_data = ren(
    seed_data, R_data, gt_data, pop_data, pop_floor, ctrl
  );
}
