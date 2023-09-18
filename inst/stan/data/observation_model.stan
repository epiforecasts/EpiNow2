  array[t - seeding_time] int day_of_week; // day of the week indicator (1 - 7)
  int model_type;                    // type of model: 0 = poisson otherwise negative binomial
  real phi_mean;                     // Mean and sd of the normal prior for the
  real phi_sd;                       // reporting process
  int week_effect;                   // length of week effect
  int obs_scale;                     // logical controlling scaling of observations
  real obs_scale_mean;               // mean scaling factor for observations
  real obs_scale_sd;                 // standard deviation of observation scaling
  real obs_weight;                   // weight given to observation in log density
  int likelihood;                    // Should the likelihood be included in the model
  int return_likelihood; // Should the likehood be returned by the model
  int<lower = 0> trunc_id; // id of truncation
  int<lower = 0> delay_id; // id of delay
