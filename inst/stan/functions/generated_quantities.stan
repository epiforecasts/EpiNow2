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
// Convert an estimate of Rt to growth
real[] R_to_growth(vector R, real gt_mean, real gt_var) {
  int t = num_elements(R);
  real r[t];
  if (gt_var > 0) {
    real k = gt_var / pow(gt_mean, 2);
    for (s in 1:t) {
      r[s] = (pow(R[s], k) - 1) / (k * gt_mean);
    }
  } else {
    // limit as gt_sd -> 0
    for (s in 1:t) {
      r[s] = log(R[s]) / gt_mean;
    }
  }
  return(r);
}
