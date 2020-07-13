functions {
  // convolve a pdf and case vector 
  vector convolve(vector cases, vector pdf) {
    int t = num_elements(cases);
    int max_pdf = num_elements(pdf);
    vector[t] convolved_cases = rep_vector(1e-5, t);
    for (s in 1:t) {
        convolved_cases[s] += dot_product(cases[max(1, (s - max_pdf + 1)):s], tail(pdf, min(max_pdf, s)));
    }
   return(convolved_cases);
  }

  // discretised truncated lognormal pmf
  real discretised_lognormal_pmf(int y, real mu, real sigma, int max_val) {
    real adj_y = y + 1e-5;
    return((normal_cdf((log(adj_y + 1) - mu) / sigma, 0.0, 1.0) - normal_cdf((log(adj_y) - mu) / sigma, 0.0, 1.0)) / 
            normal_cdf((log(max_val) - mu) / sigma, 0.0, 1.0));
  }
  
  // discretised truncated gamma pmf
  real discretised_gamma_pmf(int y, real mu, real sigma, int max_val) {
    // calculate alpha and beta for gamma distribution
    real c_sigma = sigma + 1e-5;
    real alpha = ((mu)/ c_sigma)^2;
    real beta = (mu) / (c_sigma^2);
    //account for numerical issues
    alpha = alpha < 0 ? 1e-5 : alpha;
    beta = beta < 0 ? 1e-5 : beta; 
    return((gamma_cdf(y + 1, alpha, beta) - gamma_cdf(y, alpha, beta)) / 
    (gamma_cdf(max_val, alpha, beta) - gamma_cdf(1, alpha, beta)));
  }
  
  // exponential quadratic kernal
	real spd_SE(real alpha, real rho, real w) {
		real S;
		S = (alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho^2)*(w^2));
		return S;
	}
	
	// basis function for approximate hilbert space gp
	// see here for details: https://arxiv.org/pdf/2004.11408.pdf
	vector phi_SE(real L, int m, vector x) {
		vector[rows(x)] fi;
		fi = 1/sqrt(L) * sin(m*pi()/(2*L) * (x+L));
		return fi;
	}
	
	// eigenvalues for approximate hilbert space gp
	// see here for details: https://arxiv.org/pdf/2004.11408.pdf
	real lambda(real L, int m) {
		real lam;
		lam = ((m*pi())/(2*L))^2;
		return lam;
	}
}


data {
  int t;                             // number of time steps
  int rt;                            // time over which to estimate rt
  int horizon;                       // forecast horizon
  int day_of_week[rt];               // day of the week indicator (1 - 7)
  int<lower = 0> cases[rt - horizon];// observed cases
  vector<lower = 0>[t] shifted_cases;// median shifted smoothed cases
  int delays;                        // no. of delay distributions
  real delay_mean_sd[delays];        // prior sd of mean incubation period
  real delay_mean_mean[delays];      // prior mean of mean incubation period
  real delay_sd_mean[delays];        // prior sd of sd of incubation period
  real delay_sd_sd[delays];          // prior sd of sd of incubation period
  int max_delay[delays];             // maximum incubation period
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
	int est_week_eff;
	vector[rt] time;
	vector[t] inf_time;
	int stationary;                    // is underlying Rt assumed to be stationary (+ GP)
	int break_no;                      // no of breakpoints (0 = no breakpoints)
	int breakpoints[rt];               // when do breakpoints occur 
}

transformed data{
  real r_alpha;                              // alpha parameter of the R gamma prior
  real r_beta;                               // beta parameter of the R gamma prior
  int no_rt_time;                            // time without estimating Rt
  int rt_h;                                  // rt estimation time minus the forecasting horizon
  int noise_terms = estimate_r > 0 ? (stationary > 0 ? rt : rt - 1) : t;  
                                             // no. of noise terms
  matrix[noise_terms, M] PHI;                // basis function 
  
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
     PHI[,m] = phi_SE(L, m, (estimate_r > 0 ? time[1:(stationary > 0 ? rt : rt - 1)] : inf_time)); 
    }
}
parameters{
  simplex[est_week_eff ? 7 : 1] day_of_week_eff_raw;  // day of week reporting effect + control parameters
  real <lower = 0> delay_mean[delays];                // mean of delays
  real <lower = 0> delay_sd[delays];                  // sd of delays
  real<lower = 0> rep_phi[model_type];                // overdispersion of the reporting process
  real<lower = 0> rho[1];                             // length scale of noise GP
  real<lower = 0> alpha[1];                           // scale of of noise GP
  vector[M] eta;                                      // unconstrained noise
  vector<lower = 0>[estimate_r] initial_R;            // baseline reproduction number estimate
  vector[estimate_r > 0 ? no_rt_time : 0] initial_infections;
                                                      // baseline reproduction number estimate
  real<lower = 0> gt_mean[estimate_r];                // mean of generation time
  real <lower = 0> gt_sd[estimate_r];                 // sd of generation time
  real rt_break_eff[break_no];                        // Rt breakpoint effects
}

transformed parameters {
  // stored transformed parameters
  vector<lower = 0>[noise_terms] noise;                   // noise on the mean shifted observed cases
  vector[t] infections;                                   // infections over time
  vector[rt] reports;                                     // reports over time
  vector[est_week_eff ? 7 : 0] day_of_week_eff;           // day of the week effect
  vector[estimate_r > 0 ? rt : 0] R;                      // reproduction number over time
 {
  // temporary transformed parameters                                 
  vector[estimate_r > 0 ? max_gt : 0] rev_generation_time;// reversed generation time pdf
  vector[estimate_r > 0 ? rt : 0] infectiousness;         // infections over time
  vector[M] diagSPD;                                      // spectral density
	vector[M] SPD_eta;                                      // spectral density * noise
	int rt_break_count;                                     // counter for Rt breakpoints
	
  // GP in noise - spectral densities
	for(m in 1:M){ 
		diagSPD[m] =  sqrt(spd_SE(alpha[1], rho[1], sqrt(lambda(L, m)))); 
	}
	SPD_eta = diagSPD .* eta;
	
	noise = rep_vector(1e-5, noise_terms);
  noise = noise + exp(PHI[,] * SPD_eta);

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
  //initialise breakpoints as 0
  rt_break_count = 0;
  // assume a global Rt * GP
  if (stationary) {
    for (s in 1:rt) {
      R[s] = initial_R[estimate_r] * noise[s];
      // apply breakpoints if present
      if (break_no > 0) {
       rt_break_count += breakpoints[s];
        if (rt_break_count > 0) {
          R[s] = R[s] * prod(rt_break_eff[1:rt_break_count]);
        }
      }
    }
  // assume GP on gradient of Rt (i.e Rt = R(t-1) * GP)
  }else{
    R[1] = initial_R[estimate_r];
    for (s in 2:rt) {
      R[s] = R[s - 1] .* noise[s - 1];
      // apply breakpoints if present
      if (break_no > 0) {
        if (breakpoints[s] == 1) {
          rt_break_count = sum(breakpoints[1:s]);
          R[s] = R[s] * rt_break_eff[rt_break_count];
        }
      }
    }
  }

     // estimate initial infections not using Rt
     infections[1:no_rt_time] = infections[1:no_rt_time] + 
                                  shifted_cases[1:no_rt_time] .* exp(initial_infections);
      
     // estimate remaining infections using Rt
     infectiousness = rep_vector(1e-5, rt);
     for (s in 1:rt) {
        infectiousness[s] += dot_product(infections[max(1, (s + no_rt_time - max_gt)):(s + no_rt_time -1)],
                                         tail(rev_generation_time, min(max_gt, s + no_rt_time - 1)));
        infections[s + no_rt_time] += R[s] * infectiousness[s];
      }
  }else{
    // generate infections from prior infections and non-parameteric noise
    infections = infections + shifted_cases .* noise;
  }

  // reports from onsets
 {
   vector[t] reports_hold;
   for (s in 1:delays) {
    // reverse the distributions to allow vectorised access
    vector[max_delay[s]] rev_delay = rep_vector(1e-5, max_delay[s]);
    for (j in 1:(max_delay[s])) {
      rev_delay[j] +=
        discretised_lognormal_pmf(max_delay[s] - j + 1, delay_mean[s], delay_sd[s], max_delay[s]);
    }
     if (s == 1) {
       reports_hold = convolve(infections, rev_delay);
     }else{
       reports_hold = convolve(reports_hold, rev_delay);
     }
   }
   reports = reports_hold[(no_rt_time + 1):t];
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
  rho ~  normal(0, 2);
  alpha ~ normal(0, 0.1);
  eta ~ std_normal();

  // reporting overdispersion
  if (model_type) {
    rep_phi[model_type] ~ exponential(1);
  }

  // penalised priors for delaysincubation period, and report delay
  for (s in 1:delays) {
    target += normal_lpdf(delay_mean[s] | delay_mean_mean[s], delay_mean_sd[s]) * t;
    target += normal_lpdf(delay_sd[s] | delay_sd_mean[s], delay_sd_sd[s]) * t;
  }
  
  // estimate rt
  if (estimate_r) {
    // prior on R
    initial_R[estimate_r] ~ gamma(r_alpha, r_beta);
    initial_infections ~ normal(0, 0.1);
    
    // penalised_prior on generation interval
    target += normal_lpdf(gt_mean | gt_mean_mean, gt_mean_sd) * rt;
    target += normal_lpdf(gt_sd | gt_sd_mean, gt_sd_sd) * rt;
    
    //breakpoint effects on Rt
    if (break_no > 0) {
      rt_break_eff ~ lognormal(0, 0.1);
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
  int imputed_infections[t];
  int imputed_reports[rt]; 
  real r[estimate_r > 0 ? rt : 0];
  
  // simulated infections - assume poisson (with negative binomial reporting)
  imputed_infections = poisson_rng(infections);

  // estimate the growth rate
  if (estimate_r) {
      real k = pow(gt_sd[estimate_r] / gt_mean[estimate_r], 2);
      for (s in 1:rt) {
        r[s] = (pow(R[s], k) - 1) / (k * gt_mean[estimate_r]);
      } 
  }
  
  //simulate reported cases
  if (model_type) {
    imputed_reports = neg_binomial_2_rng(reports, rep_phi[model_type]);
   }else{
    imputed_reports = poisson_rng(reports);
  }
}

