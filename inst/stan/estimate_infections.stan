functions {
  // convolve a pdf and case vector using matrix multiplication
  vector convolve(vector cases, vector pdf, int direction) {
    int t = num_elements(cases);
    matrix[t, t] delay_mat = rep_matrix(0, t, t);
    int max_pdf = num_elements(pdf);
    row_vector[max_pdf] row_pdf = to_row_vector(pdf);
    vector[t] convolved_cases;

    for (s in 1:t) {
      if (direction) {
        int max_length = min(s, max_pdf);
        delay_mat[s, (s - max_length + 1):s] = row_pdf[(max_pdf - max_length + 1):max_pdf];
      }else{
        int max_length = min(t - s, max_pdf - 1);
        delay_mat[s, s:(s + max_length)] = row_pdf[1:(max_length + 1)];
      }
    }
    
   convolved_cases = delay_mat * to_vector(cases);

   return(convolved_cases);
  }

  // discretised truncated lognormal pmf
  real discretised_lognormal_pmf(int y, real mu, real sigma, int max_val) {
    return((normal_cdf((log(y + 1) - mu) / sigma, 0.0, 1.0) - normal_cdf((log(y) - mu) / sigma, 0.0, 1.0)) / 
            normal_cdf((log(max_val) - mu) / sigma, 0.0, 1.0));
  }
  
  // discretised truncated gamma pmf
  real discretised_gamma_pmf(int y, real mu, real sigma, int max_val) {
    // calculate alpha and beta for gamma distribution
    real alpha = ((mu)/ sigma)^2;
    real beta = (mu) / (sigma^2);
    return((gamma_cdf(y, alpha, beta) - gamma_cdf(y - 1, alpha, beta)) / gamma_cdf(max_val, alpha, beta));
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
  int day_of_week[rt];                // day of the week indicator (1 - 7)
  int<lower = 0> cases[rt - horizon];// observed cases
  vector<lower = 0>[t] shifted_cases;// median shifted smoothed cases
  real inc_mean_sd;                  // prior sd of mean incubation period
  real inc_mean_mean;                // prior mean of mean incubation period
  real inc_sd_mean;                  // prior sd of sd of incubation period
  real inc_sd_sd;                    // prior sd of sd of incubation period
  int max_inc;                       // maximum incubation period
  real rep_mean_mean;                // prior mean of mean reporting delay
  real rep_mean_sd;                  // prior sd of mean reporting delay
  real rep_sd_mean;                  // prior mean of sd of reporting delay
  real rep_sd_sd;                    // prior sd of sd of reporting delay
  int max_rep;                       // maximum report delay
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
  real rL;				                   // boundary value for Rt gp
	int<lower=1> rM;			             // basis functions for Rt gp
}

transformed data{
  real r_alpha;                      // alpha parameter of the R gamma prior
  real r_beta;                       // beta parameter of the R gamma prior
  vector[t] delta;                   // modifier to make GP + definite
  vector[t] time;                    // time vector
  int no_rt_time;                    // time without estimating Rt
  int rt_h;                          // rt estimation time minus the forecasting horizon
  matrix[t, M] PHI_inf;              // basis function for infections
  matrix[rt - 1, rM] PHI_rt;         // basis function for Rt
  
  //Update time varables
  rt_h = rt - horizon;
  
  // calculate alpha and beta for gamma distribution
  r_alpha = (r_mean / r_sd)^2;
  r_beta = r_mean / (r_sd^2);
  
  // make time vector
   for (s in 1:t) {
     time[s] = s;
   }
   
   // time without estimating Rt is the differrence of t and rt
   no_rt_time = t - rt;
   
   // basis functions
   // see here for details: https://arxiv.org/pdf/2004.11408.pdf
   for (m in 1:M){ 
     PHI_inf[,m] = phi_SE(L, m, time); 
    }
    
   for (m in 1:rM){ 
     PHI_rt[,m] = phi_SE(rL, m, time[1:(rt - 1)]); 
    }
    
}
parameters{
  simplex[7] day_of_week_eff_raw;                     // day of week reporting effect + control parameters
  real <lower = 0> inc_mean;                          // mean of incubation period
  real <lower = 0> inc_sd;                            // sd of incubation period
  real <lower = 0> rep_mean;                          // mean of reporting delay
  real <lower = 0> rep_sd;                            // sd of incubation period
  real<lower = 0> rep_phi[model_type];                // overdispersion of the reporting process
  real<lower=0> rho;                                  // length scale of noise GP
  real<lower=0> alpha;                                // scale of of noise GP
  vector[M] eta;                                      // unconstrained noise
  vector<lower = 0>[estimate_r] initial_R;            // baseline reproduction number estimate
  real<lower = 1> gt_mean[estimate_r];                // mean of generation time
  real <lower = 0> gt_sd[estimate_r];                 // sd of generation time
  real<lower=0> R_rho[estimate_r];                    // length scale of R GP
  real<lower=0> R_alpha[estimate_r];                  // scale of R GP
  vector[estimate_r > 0 ? rM : 0] R_eta;              // unconstrained R noise
}

transformed parameters {
  // stored transformed parameters
  vector<lower = 0>[t] noise;                             // noise on the mean shifted observed cases
  vector<lower = 0>[t] infections;                        // infections over time
  vector<lower = 0>[rt] reports;                          // reports over time
  vector[7] day_of_week_eff;                              // day of the week effect
  vector[estimate_r > 0 ? t : 0] branch_infections;       // infections generated by the renewal equation
  vector[estimate_r > 0 ? rt : 0] branch_reports;         // reports generated by the renewal equation
  vector[estimate_r > 0 ? rt : 0] R;                      // reproduction number over time
  
 {
  // temporary transformed parameters
  vector[max_rep] rev_delay;                              // reversed report delay pdf
  vector[max_inc] rev_incubation;                         // reversed incubation period pdf
  vector[t] onsets;                                       // onsets over time
  vector[estimate_r > 0 ? max_gt : 0] rev_generation_time;// reversed generation time pdf
  vector[estimate_r > 0 ? t : 0] infectiousness;          // infections over time
  vector[estimate_r > 0 ? t : 0] branch_onsets;           // onsets generated by the renewal equation
  vector[estimate_r > 0 ? rt - 1 : 0] R_noise;            // noise on R centred on 1
  vector[M] diagSPD;                                      // spectral density for infections     
	vector[M] SPD_eta;                                      // spectral density * noise for infections
	vector[rM] R_diagSPD;                                   // spectracl density for Rt
	vector[rM] R_SPD_eta;                                   // spectral density * noise for Rt
  
  // reverse the distributions to allow vectorised access
  for (j in 1:max_inc) {
    rev_incubation[j] =
        discretised_lognormal_pmf(max_inc - j + 1, inc_mean, inc_sd, max_inc);
  }
  
  for (j in 1:(max_rep)) {
    rev_delay[j] =
        discretised_lognormal_pmf(max_rep - j + 1, rep_mean, rep_sd, max_rep);
  }
    
  // define day of the week effect
  day_of_week_eff = 7 * day_of_week_eff_raw;

  // GP in noise - spectral densities
	for(m in 1:M){ 
		diagSPD[m] =  sqrt(spd_SE(alpha, rho, sqrt(lambda(L, m)))); 
	}
	
	SPD_eta = diagSPD .* eta;
	
  noise = exp(PHI_inf[,] * SPD_eta);

  for (s in 1:t) {
    if(noise[s] == 0) {
     noise[s] = 0.00001;
    }
  }
  
  // generate infections from prior infections and non-parameteric noise
  infections = shifted_cases .* noise;
  
  // Make sure all infections have some postive value
  for (s in 1:t) {
    if (infections[s] == 0){
      infections[s] = 0.00001; 
      }
    }
       
  // onsets from infections
  onsets = convolve(infections, rev_incubation, 1);

  // reports from onsets
 {
   vector[t] reports_hold;
   reports_hold = convolve(onsets, rev_delay, 1);
   reports = reports_hold[(no_rt_time + 1):t];
 }
 
  for (s in 1:rt) {
    // add reporting effects (adjust for simplex scale)
    reports[s] *= day_of_week_eff[day_of_week[s]];
  }
    
  ////////////////////////////////////////////////////// 
  // estimate reproduction no from a renewal equation
  //////////////////////////////////////////////////////
  if (estimate_r) {
    // calculate pdf of generation time from distribution
    for (j in 1:(max_gt - 1)) {
       rev_generation_time[j] =
           discretised_gamma_pmf(max_gt - j, gt_mean[estimate_r], gt_sd[estimate_r], max_gt);
     }
     
     // set same day to be 0
     rev_generation_time[max_gt] = 0;
     
     infectiousness = convolve(infections, rev_generation_time, 1);
     
     // Construct R over time
     	for(m in 1:rM){ 
     	  R_diagSPD[m] =  sqrt(spd_SE(R_alpha[estimate_r], R_rho[estimate_r], sqrt(lambda(rL, m)))); 
     	}
     	  
     R_SPD_eta = R_diagSPD .* R_eta;
	
     R_noise = exp(PHI_rt[,] * R_SPD_eta);
  
      R[1] = initial_R[estimate_r];
      for (s in 2:rt) {
        R[s] = R[s - 1] * R_noise[s - 1];
      }
      
     // Estimate infections using renewal equation
     branch_infections[1:no_rt_time] = infections[1:no_rt_time];
      
      for (s in 1:rt) {
         branch_infections[s + no_rt_time] = R[s] * infectiousness[s + no_rt_time];
         // Make sure all dates have a non-zero value
         if (branch_infections[s] == 0){
            branch_infections[s] = 0.00001; 
         }
       }
       
     // onsets from infections
     branch_onsets = convolve(branch_infections, rev_incubation, 1);

     // reports from onsets
     {
       vector[estimate_r > 0 ? t : 0] branch_reports_hold;
       branch_reports_hold = convolve(branch_onsets, rev_delay, 1);
       branch_reports = branch_reports_hold[(no_rt_time + 1):t];
     }
    
     for (s in 1:rt) {
      // add reporting effects (adjust for simplex scale)
      branch_reports[s] *= (day_of_week_eff[day_of_week[s]]);
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
  
  // daily cases given reports
  if (model_type) {
    target += neg_binomial_2_lpmf(cases | reports[1:rt_h], rep_phi[model_type]);
  }else{
    target += poisson_lpmf(cases | reports[1:rt_h]);
  }

  // penalised priors for incubation period, and report delay
  target += normal_lpdf(inc_mean | inc_mean_mean, inc_mean_sd) * t;
  target += normal_lpdf(inc_sd | inc_sd_mean, inc_sd_sd) * t;
  target += normal_lpdf(rep_mean | rep_mean_mean, rep_mean_sd) * t;
  target += normal_lpdf(rep_sd | rep_sd_mean, rep_sd_sd) * t;
  
  ////////////////////////////////////////////////////// 
  // estimate reproduction no from a renewal equation
  //////////////////////////////////////////////////////
  if (estimate_r) {
  // prior on R
  initial_R[estimate_r] ~ gamma(r_alpha, r_beta);
    
   // priors for R gp
   R_rho ~  normal(0, 2);
   R_alpha ~ normal(0, 0.1);
   R_eta ~ std_normal();
    
    // penalised_prior on generation interval
    target += normal_lpdf(gt_mean | gt_mean_mean, gt_mean_sd) * rt;
    target += normal_lpdf(gt_sd | gt_sd_mean, gt_sd_sd) * rt;
    
    // Likelihood of Rt given infections
    if (model_type) {
      target += neg_binomial_2_lpmf(cases | branch_reports[1:rt_h], rep_phi[model_type]);
    }else{
      target += poisson_lpmf(cases | branch_reports[1:rt_h]);
    }
  }
}
  
generated quantities {
  int imputed_infections[t];
  int imputed_infections_rt[estimate_r > 0 ? t : 0];
  int imputed_reports[rt]; 
  int imputed_branch_reports[estimate_r > 0 ? rt : 0];
  real r[estimate_r > 0 ? rt : 0];
  
  // simulated infections - assume poisson (with negative binomial reporting)
  imputed_infections = poisson_rng(infections);
  
  //simulate infections from Rt
  if (estimate_r) {
    imputed_infections_rt = poisson_rng(branch_infections);
  }
  
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
  //simulate reported cases from the Rt model
  if (estimate_r) {
    if (model_type) {
    imputed_branch_reports = neg_binomial_2_rng(branch_reports, rep_phi[model_type]);
   }else{
    imputed_branch_reports = poisson_rng(branch_reports);
  }
 }
}

