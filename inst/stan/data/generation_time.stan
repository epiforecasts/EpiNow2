  real gt_mean_sd[1];                   // prior sd of mean generation time
  real gt_mean_mean[1];                 // prior mean of mean generation time
  real gt_sd_mean[1];                   // prior mean of sd of generation time
  real gt_sd_sd[1];                     // prior sd of sd of generation time
  int<lower = 1> gt_max[1];                        // maximum generation time
  int gt_fixed[1];                      // 0 = variable gt; 1 = fixed gt
  int gt_dist[1];                    // distribution (0 = lognormal, 1 = gamma)
