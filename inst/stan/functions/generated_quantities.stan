// calculate Rt directly from inferred infections
vector calculate_Rt(vector infections, int seeding_time,
                    vector gt_rev_pmf, int smooth) {
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
  if (smooth) {
    for (s in 1:ot) {
      real window = 0;
      sR[s] = 0;
      for (i in max(1, s - smooth):min(ot, s + smooth)) {
        sR[s] += R[i];
        window += 1;
      }
      sR[s] = sR[s] / window;
    }
  }else{
    sR = R;
  }
  return(sR);
}

// Calculate growth rate
array[] real calculate_growth(vector infections, int seeding_time) {
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[t] log_inf = log(infections);
  vector[ot] growth = log_inf[(seeding_time + 1):t] - log_inf[seeding_time:(t - 1)];
  return(to_array_1d(growth));
}
