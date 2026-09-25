// Pure Stan reference for renewal_infections(), used only by the tests.

/**
 * The renewal loop that generate_infections() ran before the C++ version.
 *
 * Takes the same arguments and returns the same vector as
 * renewal_infections().
 */
vector renewal_infections_stan(vector seed, vector R, vector gt_rev_pmf,
                               real pop, int use_pop, real pop_floor,
                               int nht) {
  int uot = num_elements(seed);
  int ot = num_elements(R);
  int t = ot + uot;
  real exp_adj_Rt;
  vector[t] infections = rep_vector(0, t);
  vector[ot] cum_infections;
  vector[ot] infectiousness;
  infections[1:uot] = seed;
  if (use_pop) {
    cum_infections[1] = sum(infections[1:uot]);
  }
  for (s in 1:ot) {
    infectiousness[s] = update_infectiousness(infections, gt_rev_pmf, uot, s);
    if ((use_pop == 1 && s > nht) || use_pop == 2) {
      real susceptible = fmax(pop_floor, pop - cum_infections[s]);
      exp_adj_Rt = exp(-R[s] * infectiousness[s] / susceptible);
      infections[s + uot] = susceptible * fmax(0, 1 - exp_adj_Rt);
    } else {
      infections[s + uot] = R[s] * infectiousness[s];
    }
    if (use_pop && s < ot) {
      cum_infections[s + 1] = cum_infections[s] + infections[s + uot];
    }
  }
  return(infections);
}
