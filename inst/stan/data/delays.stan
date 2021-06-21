  int delays;                  // no. of delay distributions
  real delay_mean_sd[delays];  // prior sd of mean incubation period
  real delay_mean_mean[delays];// prior mean of mean incubation period
  real delay_sd_mean[delays];  // prior sd of sd of incubation period
  real delay_sd_sd[delays];    // prior sd of sd of incubation period
  int max_delay[delays];       // maximum incubation period
  int total_delay;
  int dmean_gp[delays];
  int dsd_gp[delays];
  int dmean_gps;
  int dsd_gps;
  int delay_gps;
  int delay_type;
