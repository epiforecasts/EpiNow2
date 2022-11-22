  int gt_param;                         // 0 = nonparametric, 1 = parametric
  real gt_mean_sd[gt_param];                   // prior sd of mean generation time
  real gt_mean_mean[gt_param];                 // prior mean of mean generation time
  real gt_sd_mean[gt_param];                   // prior mean of sd of generation time
  real gt_sd_sd[gt_param];                     // prior sd of sd of generation time
  int<lower = 1> gt_max[1];                        // maximum generation time
  int gt_fixed[gt_param];                      // 0 = variable gt; 1 = fixed gt
  int gt_dist[gt_param];                    // distribution (0 = lognormal, 1 = gamma)
  int gt_weight;                    // prior weight on generation times

