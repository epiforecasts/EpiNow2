


vector R_to_growth(vector R, real[] gt_mean, real[] gt_sd) {
  real k = pow(gt_sd / gt_mean, 2);
  int t = num_elements(R);
  real r[t];
  for (s in 1:t) {
    r[s] = (pow(R[s], k) - 1) / (k * gt_mean);
  } 
  return(r)
}