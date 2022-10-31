  int delays;                  // no. of delay distributions
  int n_uncertain_mean_delays;   // no. of delay distributions with uncertain mean
  int n_uncertain_sd_delays;     // no. of delay distributions with uncertain sd
  int n_fixed_delays;     // no. of delay distributions with uncertain sd
  // indices of delay distributions with uncertainty
  int<lower = 1, upper = delays> uncertain_mean_delays[n_uncertain_mean_delays];
  int<lower = 1, upper = delays> uncertain_sd_delays[n_uncertain_sd_delays];
  int<lower = 1, upper = delays> fixed_delays[n_fixed_delays];
  real delay_mean_sd[delays];  // prior sd of mean incubation period
  real delay_mean_mean[delays];// prior mean of mean incubation period
  real delay_sd_mean[delays];  // prior sd of sd of incubation period
  real delay_sd_sd[delays];    // prior sd of sd of incubation period
  int<lower = 1> delay_max[delays];       // maximum incubation period
  int delay_dist[delays];       // 0 = lognormal; 1 = gamma
