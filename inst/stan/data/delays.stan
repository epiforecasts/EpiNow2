  int<lower = 0> delay_n;                     // number of delay distributions
  int<lower = 0> delay_n_p;                   // number of parametric delay distributions
  int<lower = 0> delay_n_np;                  // number of nonparametric delay distributions
  real delay_mean_mean[delay_n_p];            // prior mean of mean delay distribution
  real<lower = 0> delay_mean_sd[delay_n_p];   // prior sd of mean delay distribution
  real<lower = 0> delay_sd_mean[delay_n_p];   // prior sd of sd of delay distribution
  real<lower = 0> delay_sd_sd[delay_n_p];     // prior sd of sd of delay distribution
  int<lower = 1> delay_max[delay_n_p];        // maximum delay distribution
  int<lower = 0> delay_dist[delay_n_p];       // 0 = lognormal; 1 = gamma
  int<lower = 0> delay_np_pmf_max;            // number of nonparametric pmf elements
  vector<lower = 0, upper = 1>[delay_np_pmf_max] delay_np_pmf; // ragged array of fixed PMFs
  int<lower = 1> delay_np_pmf_groups[delay_n_np + 1];              // links to ragged array
  int<lower = 0> delay_weight[delay_n_p];

  int<lower = 0> delay_types;                     // number of delay types
  int<lower = 0> delay_types_p[delay_n];          // whether delay types are parametric
  int<lower = 0> delay_types_id[delay_n];          // whether delay types are parametric
  int<lower = 0> delay_types_groups[delay_types + 1]; // index of each delay (parametric or non)

  int<lower = 0> delay_id; // id of generation time
