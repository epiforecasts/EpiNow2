functions {
#include functions/pmfs.stan
#include functions/convolve.stan
#include functions/gaussian_process.stan
#include functions/rt.stan
#include functions/infections.stan
#include functions/observation_model.stan
#include functions/generated_quantities.stan
}


data {
#include data/observations.stan
#include data/delays.stan
#include data/gaussian_process.stan
#include data/generation_time.stan
#include data/observation_model.stan
#include data/rt.stan
}


transformed data{
  real r_logmean;                             // Initial R mean in log space
  real r_logsd;                              // Iniital R sd in log space
  int seeding_time;                          // time without estimating Rt
  int rt_h;                                  // rt estimation time minus the forecasting horizon
  int noise_time = estimate_r > 0 ? (stationary > 0 ? rt : rt - 1) : t;
  //Update number of noise terms based on furure Rt assumption  
  int noise_terms =  future_fixed > 0 ? (noise_time - horizon + fixed_from) : noise_time;                                      // no. of noise terms
  matrix[noise_terms, M] PHI = setup_gp(M, L, noise_terms);  // basis function 
  
  //Update time varables
  rt_h = rt - horizon;
  
  // calculate alpha and beta for gamma distribution
  r_logmean = log(r_mean^2 / sqrt(r_sd^2 + r_mean^2));
  r_logsd = sqrt(log(1 + (r_sd^2 / r_mean^2)));

  // time without estimating Rt is the differrence of t and rt
  seeding_time = t - rt;
}
parameters{
  simplex[est_week_eff ? 7 : 1] day_of_week_simplex;  // day of week reporting effect + control parameters
  real<lower = 0> delay_mean[delays];                 // mean of delays
  real<lower = 0> delay_sd[delays];                   // sd of delays
  real<lower = 0> rep_phi[model_type];                // overdispersion of the reporting process
  real<lower = 0> rho[fixed ? 0 : 1];                 // length scale of noise GP
  real<lower = 0> alpha[fixed ? 0 : 1];               // scale of of noise GP
  vector[fixed ? 0 : M] eta;                          // unconstrained noise
  vector[estimate_r] logR;                            // baseline reproduction number estimate
  vector[estimate_r > 0 ? seeding_time : 0] initial_infections;
                                                      // seed infections adjustment when estimating Rt
  real<lower = 0> gt_mean[estimate_r];                // mean of generation time
  real <lower = 0> gt_sd[estimate_r];                 // sd of generation time
  real bp_effects[bp_n];                          // Rt breakpoint effects
}

transformed parameters {
  // stored transformed parameters
  vector[fixed ? 0 : noise_terms] noise;                  // noise on the mean shifted observed cases
  vector[t] infections;                                   // infections over time
  vector[rt] reports;                                     // reports over time
  vector[estimate_r > 0 ? rt : 0] R;                      // reproduction number over time
  
  // GP in noise - spectral densities
  if (!fixed) {
    noise = update_gp(PHI, M, L, alpha[1], rho[1], eta);
  }

  // Estimate Rt and use this estimate to generate infections
  if (estimate_r) {
    R = update_Rt(R, logR[estimate_r], noise, breakpoints, bp_effects, stationary);
    infections = generate_infections(R, seeding_time, gt_mean, gt_sd, max_gt, shifted_cases, initial_infections);
  }else{
    infections = deconvolve_infections(shifted_cases, noise, fixed);
  }

  // reports from onsets
  reports = convolve_to_report(infections, delay_mean, delay_sd, max_delay, seeding_time);

 // weekly reporting effect
 if (est_week_eff) {
   reports = day_of_week_effect(reports, day_of_week, day_of_week_simplex);
  }
}

model {
  // priors for noise GP
  if (!fixed) {
    gaussian_process_lp(rho, alpha, eta, lengthscale_alpha, lengthscale_beta, alpha_sd);
  }

  // penalised priors for delay distributions
  delays_lp(delay_mean, delay_mean_mean, delay_mean_sd, delay_sd, delay_sd_mean, delay_sd_sd, t);

  // estimate rt
  if (estimate_r) {
    // prior on R
    logR[estimate_r] ~ normal(r_logmean, r_logsd);
    //breakpoint effects on Rt
    if (bp_n > 0) {
      bp_effects ~ normal(0, 0.1);
    }
    // initial infections
    initial_infections ~ lognormal(0, 0.1);
    // penalised_prior on generation interval
    generation_time_lp(gt_mean, gt_mean_mean, gt_mean_sd, gt_sd, gt_sd_mean, gt_sd_sd, rt);

  }
  // evaluate simulated reports compared to observed
  report_lp(cases, reports, rep_phi, 1, model_type, horizon, 1);
}
  
generated quantities {
  int imputed_reports[rt]; 
  real r[estimate_r > 0 ? rt : 0];
  
  if (estimate_r) {
    // Estimate growth rate from reproduction number and generation time
    r = R_to_growth(R, gt_mean[1], gt_sd[1]);
  }
  //simulate reported cases
  imputed_reports = report_rng(reports, rep_phi, model_type);
}

