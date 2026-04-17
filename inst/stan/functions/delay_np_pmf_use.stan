// Combine fixed and estimated nonparametric delay PMFs.
// For estimated delays, normalise gamma draws and place them at
// the mapped positions; fixed entries and structural zeros are
// left unchanged from delay_np_pmf.
vector[delay_np_pmf_length] delay_np_pmf_use
  = to_vector(delay_np_pmf);
for (i in 1:delay_np_est_n) {
  int es = delay_np_est_groups[i];
  int ee = delay_np_est_groups[i + 1] - 1;
  vector[ee - es + 1] normed =
    delay_np_est_raw[es:ee]
    / sum(delay_np_est_raw[es:ee]);
  for (j in es:ee) {
    delay_np_pmf_use[delay_np_est_pos[j]] =
      normed[j - es + 1];
  }
}
