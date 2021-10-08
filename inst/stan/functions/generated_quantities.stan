// calculate Rt directly from inferred infections
vector calculate_Rt(vector infections, int seeding_time,
                    real gt_mean, real gt_sd, int max_gt,
                    int smooth) {
  vector[max_gt] gt_pmf;
  int gt_indexes[max_gt];
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[ot] R;
  vector[ot] sR;
  vector[ot] infectiousness = rep_vector(1e-5, ot);
  // calculate PMF of the generation time
  for (i in 1:(max_gt)) {
    gt_indexes[i] = max_gt - i + 1;
  }
  if (gt_sd > 0) {
    gt_pmf = discretised_gamma_pmf(gt_indexes, gt_mean, gt_sd, max_gt);
  } else {
    gt_pmf = discretised_delta_pmf(gt_indexes);
  }
  // calculate Rt using Cori et al. approach
  for (s in 1:ot) {
    infectiousness[s] += update_infectiousness(infections, gt_pmf, seeding_time, max_gt, s);
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
real[] R_to_growth(vector R, real gt_mean, real gt_sd) {
  int t = num_elements(R);
  real r[t];
  if (gt_sd > 0) {
    real k = pow(gt_sd / gt_mean, 2);
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
