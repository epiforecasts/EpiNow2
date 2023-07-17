  int day_of_week[t - seeding_time]; // day of the week indicator (1 - 7)
  int model_type;                    // type of model: 0 = poisson otherwise negative binomial
  real phi_mean;                      // Mean and sd of the normal prior for the
  real phi_sd;                        // reporting process
  int week_effect;                   // length of week effect
  int truncation;                    // 1/0 indicating if truncation should be adjusted for
  real trunc_mean_mean[truncation];  // truncation mean of mean
  real trunc_mean_sd[truncation];    // truncation sd of mean
  real trunc_sd_mean[truncation];    // truncation mean of sd
  real trunc_sd_sd[truncation];      // truncation sd of sd
  int<lower = 1> trunc_max[truncation];    // maximum truncation supported
  int trunc_fixed[truncation];    // whether the truncation distribution is fixed
  int trunc_dist[truncation];    // 0 = lognormal; 1 = gamma
  int obs_scale;                     // logical controlling scaling of observations
  real obs_scale_mean;               // mean scaling factor for observations
  real obs_scale_sd;                 // standard deviation of observation scaling
  real obs_weight;                   // weight given to observation in log density
  int likelihood;                    // Should the likelihood be included in the model
  int return_likelihood; // Should the likehood be returned by the model
