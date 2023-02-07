  int day_of_week[t - seeding_time]; // day of the week indicator (1 - 7)
  int model_type;                    // type of model: 0 = poisson otherwise negative binomial
  real phi_mean;                      // Mean and sd of the normal prior for the
  real phi_sd;                        // reporting process
  int week_effect;                   // length of week effect
  int obs_scale;                     // logical controlling scaling of observations
  real obs_scale_mean;               // mean scaling factor for observations
  real obs_scale_sd;                 // standard deviation of observation scaling
  real obs_weight;                   // weight given to observation in log density
  int likelihood;                    // Should the likelihood be included in the model
  int return_likelihood; // Should the likehood be returned by the model

  int<lower = 0> trunc_n;                  // number of trunc distribution distributions
  int<lower = 0> trunc_n_p;                // number of parametric trunc distributions
  int<lower = 0> trunc_n_np;                // number of nonparametric trunc distributions
  real<lower = 0> trunc_mean_mean[trunc_n_p]; // prior mean of mean trunc distribution
  real<lower = 0> trunc_mean_sd[trunc_n_p];   // prior sd of mean trunc distribution
  real<lower = 0> trunc_sd_mean[trunc_n_p];   // prior sd of sd of trunc distribution
  real<lower = 0> trunc_sd_sd[trunc_n_p];     // prior sd of sd of trunc distribution
  int<lower = 1> trunc_max[trunc_n_p];          // maximum trunc distribution
  int<lower = 0> trunc_dist[trunc_n_p];       // 0 = lognormal; 1 = gamma
  int<lower = 0> trunc_np_pmf_max;          // number of nonparametric pmf elements
  vector<lower = 0, upper = 1>[trunc_np_pmf_max] trunc_np_pmf; // ragged array of fixed PMFs
  int<lower = 1> trunc_np_pmf_groups[trunc_n_np];      // links to ragged array
