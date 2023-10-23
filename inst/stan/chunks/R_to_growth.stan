real gt_mean = rev_pmf_mean(gt_rev_pmf, 1);
real gt_var = rev_pmf_var(gt_rev_pmf, 1, gt_mean);
r = R_to_growth(R, gt_mean, gt_var);
