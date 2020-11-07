  int day_of_week[t - seeding_time]; // day of the week indicator (1 - 7)
  int model_type;                    // type of model: 0 = poisson otherwise negative binomial
  int week_effect;                   // should a day of the week effect be estimated
  int obs_scale;                                    // logical controlling scaling of observations
  real obs_scale_mean;                              // mean scaling factor for observations
  real obs_scale_sd;                                // standard deviation of observation scaling
  real obs_weight;                                  // weight given to observation in log density
