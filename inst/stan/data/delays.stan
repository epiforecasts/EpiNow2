  int<lower = 0> delay_n;                     // number of delay distributions
  int<lower = 0> delay_n_p;                   // number of parametric delay distributions
  int<lower = 0> delay_n_np;                  // number of nonparametric delay distributions
  array[delay_n_p] real delay_mean_mean;            // prior mean of mean delay distribution
  array[delay_n_p] real<lower = 0> delay_mean_sd;   // prior sd of mean delay distribution
  array[delay_n_p] real<lower = 0> delay_sd_mean;   // prior sd of sd of delay distribution
  array[delay_n_p] real<lower = 0> delay_sd_sd;     // prior sd of sd of delay distribution
  array[delay_n_p] int<lower = 1> delay_max;        // maximum delay distribution
  array[delay_n_p] int<lower = 0> delay_dist;       // 0 = lognormal; 1 = gamma

  int<lower = 0> delay_np_pmf_length;            // number of nonparametric pmf elements
  vector<lower = 0, upper = 1>[delay_np_pmf_length] delay_np_pmf; // ragged array of fixed PMFs
  array[delay_n_np + 1] int<lower = 1> delay_np_pmf_groups;              // links to ragged array

  int<lower = 0> delay_params_length;            // number of parameters across all parametric delay distributions
  vector[delay_params_length] delay_params_mean;      // ragged array of mean parameters for parametric delay distributions
  vector[delay_params_length] delay_params_sd;   // ragged array of sd of parameters for parametric delay distributions
  array[delay_n_p + 1] int<lower = 0> delay_params_groups;  // links to ragged array

  array[delay_n_p] int<lower = 0> delay_weight; // delay weights
  int<lower = 0> delay_types;                     // number of delay types
  array[delay_n] int<lower = 0> delay_types_p;          // whether delay types are parametric
  array[delay_n] int<lower = 0> delay_types_id;          // whether delay types are parametric
  array[delay_types + 1] int<lower = 0> delay_types_groups; // index of each delay (parametric or non)
