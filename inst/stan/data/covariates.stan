int process_model;                 // 0 = infections; 1 = growth; 2 = rt
int bp_n;                          // no of breakpoints (0 = no breakpoints)
int breakpoints[t - seeding_time]; // when do breakpoints occur
int cov_mean_const;                // 0 = not const mean; 1 = const mean
real<lower = 0> cov_mean_mean[cov_mean_const]; // const covariate mean
real<lower = 0> cov_mean_sd[cov_mean_const];   // const covariate sd
vector<lower = 0>[cov_mean_const ? 0 : t] cov_t;  // time-varying covariate mean
