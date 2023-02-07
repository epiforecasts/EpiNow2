  real initial_infections[seeding_time ? n : 0, 1]; // initial logged infections
  real initial_growth[seeding_time > 1 ? n : 0, 1]; //initial growth

  int<lower = 0> gt_n;                  // number of generation time distributions
  int<lower = 0> gt_n_p;                // number of parametric generation times
  int<lower = 0> gt_n_np;                // number of nonparametric generation times
  real<lower = 0> gt_mean[n, gt_n_p]; // prior mean of mean generation time
  real<lower = 0> gt_sd[n, gt_n_p];     // prior sd of sd of generation time
  int<lower = 1> gt_max[gt_n_p];          // maximum generation time
  int<lower = 0> gt_dist[gt_n_p];       // 0 = lognormal; 1 = gamma
  int<lower = 0> gt_np_pmf_max;          // number of nonparametric pmf elements
  vector<lower = 0, upper = 1>[gt_np_pmf_max] gt_np_pmf; // ragged array of fixed PMFs
  int<lower = 1> gt_np_pmf_groups[gt_n_np];      // links to ragged array

  matrix[n, t - seeding_time] R; // reproduction number
  int pop;                       // susceptible population
