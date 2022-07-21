  int delays;                  // no. of delay distributions
  int uncertain_mean_delays;   // no. of delay distributions with uncertain mean
  int uncertain_sd_delays;     // no. of delay distributions with uncertain sd
  // indices of delay distributions with uncertainty
  int<lower = 1, upper = delays> uncertain_mean_delay_indices[uncertain_mean_delays];
  int<lower = 1, upper = delays> uncertain_sd_delay_indices[uncertain_sd_delays];
  real delay_mean_sd[delays];  // prior sd of mean incubation period
  real delay_mean_mean[delays];// prior mean of mean incubation period
  real delay_sd_mean[delays];  // prior sd of sd of incubation period
  real delay_sd_sd[delays];    // prior sd of sd of incubation period
  int max_delay[delays];       // maximum incubation period
