  int delays;                           // no. of delay distributions
  real delay_mean[n, delays];           // mean of delays
  real delay_sd[n, delays];             // sd of delays
  int delay_max[delays];                // maximum delay
  int delay_dist[delays];                // 0 = lognormal, 1 = gamma
