  int delays;                                    // no. of delay distributions
  real delay_mean_sd[delays == 0 ? 1 : delays];  // prior sd of mean incubation period
  real delay_mean_mean[delays == 0 ? 1 : delays];// prior mean of mean incubation period
  real delay_sd_mean[delays == 0 ? 1 : delays];  // prior sd of sd of incubation period
  real delay_sd_sd[delays == 0 ? 1 : delays];    // prior sd of sd of incubation period
  int max_delay[delays == 0 ? 1 : delays];       // maximum incubation period
  