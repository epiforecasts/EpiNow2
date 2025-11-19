int<lower = 0> delay_n; // number of delay distribution distributions
int<lower = 0> delay_n_p; // number of parametric delay distributions
int<lower = 0> delay_n_np; // number of nonparametric delay distributions
array[delay_n_p] int<lower = 1> delay_max; // maximum delay distribution
array[delay_n_p] int<lower = 0> delay_dist; // 0 = lognormal; 1 = gamma
int<lower = 0> delay_np_pmf_length; // number of nonparametric pmf elements
// ragged array of fixed PMFs
vector<lower = 0, upper = 1>[delay_np_pmf_length] delay_np_pmf;
// links to ragged array
array[delay_n_np + 1] int<lower = 1> delay_np_pmf_groups;
array[delay_n_p] int delay_weight;

// number of parameters across all parametric delay distributions
int<lower = 0> delay_params_length;
// ragged array of mean parameters for parametric delay distributions
array[n] vector[delay_params_length] delay_params;
// links to ragged array
array[delay_n_p + 1] int<lower = 0> delay_params_groups;

int<lower = 0> delay_types; // number of delay types
// whether delay types are parametric
array[delay_n] int<lower = 0> delay_types_p;
// whether delay types are parametric
array[delay_n] int<lower = 0> delay_types_id;
// index of each delay (parametric or non)
array[delay_types + 1] int<lower = 0> delay_types_groups;
