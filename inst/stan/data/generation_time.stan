  int<lower = 0> gt_n;                  // number of generation time distributions
  int<lower = 0> gt_n_p;                // number of parametric generation times
  int<lower = 0> gt_n_np;                // number of nonparametric generation times
  real gt_mean_mean[gt_n_p]; // prior mean of mean generation time
  real<lower = 0> gt_mean_sd[gt_n_p];   // prior sd of mean generation time
  real<lower = 0> gt_sd_mean[gt_n_p];   // prior sd of sd of generation time
  real<lower = 0> gt_sd_sd[gt_n_p];     // prior sd of sd of generation time
  int<lower = 1> gt_max[gt_n_p];          // maximum generation time
  int<lower = 0> gt_dist[gt_n_p];       // 0 = lognormal; 1 = gamma
  int<lower = 0> gt_np_pmf_max;          // number of nonparametric pmf elements
  vector<lower = 0, upper = 1>[gt_np_pmf_max] gt_np_pmf; // ragged array of fixed PMFs
  int<lower = 1> gt_np_pmf_groups[gt_n_np];      // links to ragged array
  int gt_weight;
  int gt_zeroes;                     // 1 = truncate, 2 = shift; 3 = add to 1
