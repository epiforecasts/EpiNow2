// calculate Rt directly from inferred infections
vector calculate_Rt(vector infections, int seeding_time,
                    vector gt_rev_pmf) {
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[ot] R;
  vector[ot] sR;
  vector[ot] infectiousness = rep_vector(1e-5, ot);
  // calculate Rt using Cori et al. approach
  for (s in 1:ot) {
    infectiousness[s] += update_infectiousness(
      infections, gt_rev_pmf, seeding_time, s
    );
    R[s] = infections[s + seeding_time] / infectiousness[s];
  }
  return(R);
}

// Calculate growth rate
vector calculate_growth(vector infections, int seeding_time) {
  int t = num_elements(infections);
  int ot = t - seeding_time - 1;
  vector[t] log_inf = log(infections);
  vector[ot] growth =
    log_inf[(seeding_time + 2):t] - log_inf[(seeding_time + 1):(t - 1)];
  return(growth);
}
