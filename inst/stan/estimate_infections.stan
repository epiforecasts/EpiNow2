functions {
#include functions/convolve.stan
#include functions/discretised_lognormal_pmf.stan
#include functions/discretised_gamma_pmf.stan
#include functions/approximate_gp_functions.stan
}


data {
  int t;                             // number of time steps
  int rt;                            // time over which to estimate rt
  int horizon;                       // forecast horizon
  int day_of_week[rt];               // day of the week indicator (1 - 7)
  int<lower = 0> cases[rt - horizon];// observed cases
  vector<lower = 0>[t] shifted_cases;// median shifted smoothed cases
  int delays;                        // no. of delay distributions
  real delay_mean_sd[delays == 0 ? 1 : delays];  // prior sd of mean incubation period
  real delay_mean_mean[delays == 0 ? 1 : delays];// prior mean of mean incubation period
  real delay_sd_mean[delays == 0 ? 1 : delays];  // prior sd of sd of incubation period
  real delay_sd_sd[delays == 0 ? 1 : delays];    // prior sd of sd of incubation period
  int max_delay[delays == 0 ? 1 : delays];       // maximum incubation period
  real <lower = 0> r_mean;           // prior mean of reproduction number
  real <lower = 0> r_sd;             // prior standard deviation of reproduction number
  real gt_mean_sd;                   // prior sd of mean generation time
  real gt_mean_mean;                 // prior mean of mean generation time
  real gt_sd_mean;                   // prior sd of sd of generation time
  real gt_sd_sd;                     // prior sd of sd of generation time
  int max_gt;                        // maximum generation time
  int model_type;                    // type of model: 0 = poisson otherwise negative binomial
  int estimate_r;                    // should the reproduction no be estimated (1 = yes)
  real L;				                     // boundary value for infections gp
	int<lower=1> M;			               // basis functions for infections gp
	real lengthscale_mean;             // mean for gp lengthscale prior
	real lengthscale_sd;               // sd for gp lengthscale prior
	int est_week_eff;
	vector[rt] time;
	vector[t] inf_time;
	int stationary;                    // is underlying Rt assumed to be stationary (+ GP)
	int break_no;                      // no of breakpoints (0 = no breakpoints)
	int breakpoints[rt];               // when do breakpoints occur 
	int fixed;                        // Indicates if Rt/backcalculation is fixed 
	int future_fixed;                 // is underlying future Rt assumed to be fixed
}

transformed data{
  real r_alpha;                              // alpha parameter of the R gamma prior
  real r_beta;                               // beta parameter of the R gamma prior
  int no_rt_time;                            // time without estimating Rt
  int rt_h;                                  // rt estimation time minus the forecasting horizon
  int noise_terms = estimate_r > 0 ? (stationary > 0 ? rt : rt - 1) : t;
                                             // no. of noise terms
  matrix[future_fixed > 0 ? (noise_terms - horizon) : noise_terms, M] PHI;  // basis function 
  
  //Update number of noise terms based on furure Rt assumption
  noise_terms = future_fixed > 0 ? (noise_terms - horizon) : noise_terms;
  
  //Update time varables
  rt_h = rt - horizon;
  
  // calculate alpha and beta for gamma distribution
  r_alpha = (r_mean / r_sd)^2;
  r_beta = r_mean / (r_sd^2);
   
   // time without estimating Rt is the differrence of t and rt
   no_rt_time = t - rt;
   
   // basis functions
   // see here for details: https://arxiv.org/pdf/2004.11408.pdf
   for (m in 1:M){ 
     PHI[,m] = phi_SE(L, m, (estimate_r > 0 ? time[1:noise_terms] : inf_time)); 
    }
}
parameters{
  simplex[est_week_eff ? 7 : 1] day_of_week_eff_raw;  // day of week reporting effect + control parameters
  real<lower = 0> delay_mean[delays];                 // mean of delays
  real<lower = 0> delay_sd[delays];                   // sd of delays
  real<lower = 0> rep_phi[model_type];                // overdispersion of the reporting process
  real<lower = 0> rho[fixed ? 0 : 1];                 // length scale of noise GP
  real<lower = 0> alpha[fixed ? 0 : 1];               // scale of of noise GP
  vector[fixed ? 0 : M] eta;                          // unconstrained noise
  vector[estimate_r] initial_R;                       // baseline reproduction number estimate
  vector[estimate_r > 0 ? no_rt_time : 0] initial_infections;
                                                      // seed infections adjustment when estimating Rt
  real<lower = 0> gt_mean[estimate_r];                // mean of generation time
  real <lower = 0> gt_sd[estimate_r];                 // sd of generation time
  real rt_break_eff[break_no];                        // Rt breakpoint effects
}

transformed parameters {
  // stored transformed parameters
  vector[fixed ? 0 : noise_terms] noise;                  // noise on the mean shifted observed cases
  vector[t] infections;                                   // infections over time
  vector[rt] reports;                                     // reports over time
  vector[est_week_eff ? 7 : 0] day_of_week_eff;           // day of the week effect
  vector[estimate_r > 0 ? rt : 0] R;                      // reproduction number over time
 {
  // temporary transformed parameters                                 
  vector[estimate_r > 0 ? max_gt : 0] rev_generation_time;// reversed generation time pdf
  vector[estimate_r > 0 ? rt : 0] infectiousness;         // infections over time
  vector[fixed > 0 ? 0 : M] diagSPD;                      // spectral density
	vector[fixed > 0 ? 0 : M] SPD_eta;                      // spectral density * noise
	int rt_break_count;                                     // counter for Rt breakpoints
	
  // GP in noise - spectral densities
  if (!fixed) {
    for(m in 1:M){ 
		diagSPD[m] =  sqrt(spd_SE(alpha[1], rho[1], sqrt(lambda(L, m)))); 
		}
  	SPD_eta = diagSPD .* eta;
	
  	noise = rep_vector(1e-6, noise_terms);
    noise = noise + PHI[,] * SPD_eta;
  }
  
  // initialise infections
  infections = rep_vector(1e-5, t);

  // Estimate Rt and use this estimate to generate infections
  if (estimate_r) {
    // calculate pdf of generation time from distribution
    for (j in 1:(max_gt)) {
       rev_generation_time[j] =
           discretised_gamma_pmf(max_gt - j + 1, gt_mean[estimate_r], 
                                 gt_sd[estimate_r], max_gt);
     }
  // initialise breakpoints as 0
  rt_break_count = 0;
  // assume a global Rt * GP
  if (stationary) {
    real log_R = log(initial_R[estimate_r]);
    R = rep_vector(log_R, rt);
    for (s in 1:rt) {
      if (!fixed) {
         if (!future_fixed || (s <= noise_terms)) {
           R[s] += noise[s];
         }else{
           R[s] = R[s - 1];
         }
       
      }
      // apply breakpoints if present
      if (break_no > 0) {
       rt_break_count += breakpoints[s];
        if (rt_break_count > 0) {
          R[s] += sum(rt_break_eff[1:rt_break_count]);
        }
      }
    }
  // assume GP on gradient of Rt (i.e Rt = R(t-1) * GP)
  }else{
    R[1] = log(initial_R[estimate_r]);
    for (s in 2:rt) {
      if (!future_fixed || (s <= (noise_terms + 1))) {
        R[s] = R[s - 1] + noise[s - 1];
      }else{
        R[s] = R[s - 1];
      }

      // apply breakpoints if present
      if (break_no > 0) {
        if (breakpoints[s] == 1) {
          rt_break_count = sum(breakpoints[1:s]);
          R[s] +=  rt_break_eff[rt_break_count];
        }
      }
    }
  }
  
  //map log R to R
  R = exp(R);

     // estimate initial infections not using Rt
     infections[1:no_rt_time] = infections[1:no_rt_time] + 
                                  shifted_cases[1:no_rt_time] .* initial_infections;
      
     // estimate remaining infections using Rt
     infectiousness = rep_vector(1e-5, rt);
     for (s in 1:rt) {
        infectiousness[s] += dot_product(infections[max(1, (s + no_rt_time - max_gt)):(s + no_rt_time -1)],
                                         tail(rev_generation_time, min(max_gt, s + no_rt_time - 1)));
        infections[s + no_rt_time] += R[s] * infectiousness[s];
      }
  }else{
    // generate infections from prior infections and non-parameteric noise
    if(!fixed) {
      infections = infections + shifted_cases .* exp(noise);
    }else{
      infections = infections + shifted_cases;
    }

  }

  // reports from onsets
  if (delays) {
     {
   vector[t] reports_hold;
   for (s in 1:delays) {
    // reverse the distributions to allow vectorised access
    vector[max_delay[s]] rev_delay = rep_vector(1e-5, max_delay[s]);
    for (j in 1:(max_delay[s])) {
      rev_delay[j] +=
        discretised_lognormal_pmf(max_delay[s] - j, delay_mean[s], delay_sd[s], max_delay[s]);
    }
     if (s == 1) {
       reports_hold = convolve(infections, rev_delay);
     }else{
       reports_hold = convolve(reports_hold, rev_delay);
     }
   }
    reports = reports_hold[(no_rt_time + 1):t];
    }
  }else{
    reports = infections[(no_rt_time + 1):t];
  }

 // Add optional weekly reporting effect
 if (est_week_eff) {
  // define day of the week effect
  day_of_week_eff = 7 * day_of_week_eff_raw;

  for (s in 1:rt) {
    // add reporting effects (adjust for simplex scale)
    reports[s] *= day_of_week_eff[day_of_week[s]];
   }
  }
 }
}

model {
  // priors for noise GP
  if (!fixed) {
  rho ~  normal(lengthscale_mean, lengthscale_sd);
  alpha ~ normal(0, 0.1);
  eta ~ std_normal();
  }

  // reporting overdispersion
  if (model_type) {
    rep_phi[model_type] ~ exponential(1);
  }

  // penalised priors for delaysincubation period, and report delay
  if (delays) {
    for (s in 1:delays) {
      target += normal_lpdf(delay_mean[s] | delay_mean_mean[s], delay_mean_sd[s]) * t;
      target += normal_lpdf(delay_sd[s] | delay_sd_mean[s], delay_sd_sd[s]) * t;
    }
  }

  // estimate rt
  if (estimate_r) {
    // prior on R
    initial_R[estimate_r] ~ gamma(r_alpha, r_beta);
    initial_infections ~ lognormal(0, 0.1);
    
    // penalised_prior on generation interval
    target += normal_lpdf(gt_mean | gt_mean_mean, gt_mean_sd) * rt;
    target += normal_lpdf(gt_sd | gt_sd_mean, gt_sd_sd) * rt;
    
    //breakpoint effects on Rt
    if (break_no > 0) {
      rt_break_eff ~ normal(0, 0.1);
    }
  }

  // daily cases given reports
  if (model_type) {
    target += neg_binomial_2_lpmf(cases | reports[1:rt_h], rep_phi[model_type]);
  }else{
    target += poisson_lpmf(cases | reports[1:rt_h]);
  }
}
  
generated quantities {
  int imputed_reports[rt]; 
  real r[estimate_r > 0 ? rt : 0];
  
  // estimate the growth rate
  if (estimate_r) {
      real k = pow(gt_sd[estimate_r] / gt_mean[estimate_r], 2);
      for (s in 1:rt) {
        r[s] = (pow(R[s], k) - 1) / (k * gt_mean[estimate_r]);
      } 
  }
  
  //simulate reported cases
  if (model_type) {
    for (s in 1:rt) {
      imputed_reports[s] = neg_binomial_2_rng(reports[s] > 1e7 ? 1e7 : reports[s], rep_phi[model_type]);
    }
   }else{
    for (s in 1:rt) {
      imputed_reports[s] = poisson_rng(reports[s] > 1e8 ? 1e8 : reports[s]);
    }
  }
}

