  real gt_mean_sd;                   // prior sd of mean generation time
  real gt_mean_mean;                 // prior mean of mean generation time
  real gt_sd_mean;                   // prior mean of sd of generation time
  real gt_sd_sd;                     // prior sd of sd of generation time
  int<lower = 1> max_gt[1];                        // maximum generation time
  int gt_fixed;                    // 0 = variable gt; 1 = fixed gt
