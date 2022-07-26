// update combined covariates
vector update_covariate(array[] real log_cov_mean, vector cov_t,
                        vector noise, array[] int bps,
                        array[] real bp_effects, int stationary, int t) {
  // define control parameters
  int bp_n = num_elements(bp_effects);
  int bp_c = 0;
  int gp_n = num_elements(noise);
  // define result vectors
  vector[t] bp = rep_vector(0, t);
  vector[t] gp = rep_vector(0, t);
  vector[t] cov;
  // initialise breakpoints
  if (bp_n) {
    for (s in 1:t) {
      if (bps[s]) {
        bp_c += bps[s];
        bp[s] = bp_effects[bp_c];
      }
    }
    bp = cumulative_sum(bp);
  }
  //initialise gaussian process
  if (gp_n) {
    if (stationary) {
      gp[1:gp_n] = noise;
      // fix future gp based on last estimated
      if (t > gp_n) {
        gp[(gp_n + 1):t] = rep_vector(noise[gp_n], t - gp_n);
      }
    }else{
      gp[2:(gp_n + 1)] = noise;
      gp = cumulative_sum(gp);
    }
  }
  if (num_elements(log_cov_mean) > 0) {
    cov = rep_vector(log_cov_mean[1], t);
  } else {
    cov = log(cov_t);
  }
  // Calculate combined covariates
  cov = cov + bp + gp;
  cov = exp(cov);
  return(cov);
}
void covariate_lp(real[] log_cov_mean,
                  real[] bp_effects, real[] bp_sd, int bp_n,
                  real[] cov_mean_logmean, real[] cov_mean_logsd) {
  // initial prior
  if (num_elements(log_cov_mean) > 0) {
    log_cov_mean ~ normal(cov_mean_logmean[1], cov_mean_logsd[1]);
  }
  // breakpoint effects
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
}
