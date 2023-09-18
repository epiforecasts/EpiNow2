  array[1] real gt_mean_sd;                   // prior sd of mean generation time
  array[1] real gt_mean_mean;                 // prior mean of mean generation time
  array[1] real gt_sd_mean;                   // prior mean of sd of generation time
  array[1] real gt_sd_sd;                     // prior sd of sd of generation time
  array[1] int<lower = 1> gt_max;                        // maximum generation time
  array[1] int gt_fixed;                      // 0 = variable gt; 1 = fixed gt
  array[1] int gt_dist;                    // distribution (0 = lognormal, 1 = gamma)
  int gt_weight;                   
