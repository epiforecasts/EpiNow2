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
    return((normal_cdf((log(y + 1) - mu) / sigma, 0.0, 1.0) - normal_cdf((log(y) - mu) / sigma, 0.0, 1.0)) / 
            normal_cdf((log(max_val) - mu) / sigma, 0.0, 1.0));
  }
  
  // discretised truncated gamma pmf
  real discretised_gamma_pmf(int y, real mu, real sigma, int max_val) {
    // calculate alpha and beta for gamma distribution
    real alpha = ((mu)/ sigma)^2;
    real beta = (mu) / (sigma^2);
    //account for numerical issues
    alpha = alpha < 0 ? 1e-5 : alpha;
    beta = beta < 0 ? 1e-5 : beta; 
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
	
	
	real q_periodic(real alpha, real rho, int v) {
		real q;
		real I;
		
		// Periodic
		if(v==0){
			I =  modified_bessel_first_kind(v, 1/rho^2); 
			q = (alpha^2) * I/exp(1/rho^2);
			return q;
		} else{
			I =  modified_bessel_first_kind(v, 1/rho^2); 
			q = (alpha^2) * 2*I/exp(1/rho^2);
			return q;
		}
	}
	
	vector phi_cosine_periodic(real w0, int m, vector x) {
		vector[rows(x)] fi;
		fi = cos(m*w0*x);
		return fi;
	}
	
	vector phi_sin_periodic(real w0, int m, vector x) {
		vector[rows(x)] fi;
		fi = sin(m*w0*x); 
		return fi;
 }
}


data {
  int t;                             // number of time steps
  int rt;                            // time over which to estimate rt
  int horizon;                       // forecast horizon
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
	int est_week_eff;
	int<lower=1> Jw;
	real period_week;	
	vector[rt] time;
	vector[t] inf_time;
}

transformed data{
  real r_alpha;                              // alpha parameter of the R gamma prior
  real r_beta;                               // beta parameter of the R gamma prior
  int no_rt_time;                            // time without estimating Rt
  int rt_h;                                  // rt estimation time minus the forecasting horizon
  matrix[estimate_r > 0 ? rt - 1 : t, M] PHI;    // basis function 
  matrix[rt, 2 * Jw + 1] PHI_week;
  
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
     PHI[,m] = phi_SE(L, m, (estimate_r > 0 ? time[1:(rt-1)] : inf_time)); 
    }
    
  for (m in 0:Jw){ 
     PHI_week[,m+1] = phi_cosine_periodic(2 * pi() / period_week, m, time); 
    }
	for (m in 1:Jw){ 
	   PHI_week[,Jw+1+m] = phi_sin_periodic(2 * pi() / period_week, m, time);
	  }
}
parameters{
  real <lower = 0> inc_mean;                          // mean of incubation period
  real <lower = 0> inc_sd;                            // sd of incubation period
  real <lower = 0> rep_mean;                          // mean of reporting delay
  real <lower = 0> rep_sd;                            // sd of incubation period
  real<lower = 0> rep_phi[model_type];                // overdispersion of the reporting process
  real<lower = 0> rho[1 + est_week_eff];              // length scale of noise GP
  real<lower = 0> alpha[1 + est_week_eff];            // scale of of noise GP
  vector[M] eta;                                      // unconstrained noise
  vector<lower = 0>[estimate_r] initial_R;            // baseline reproduction number estimate
  vector[estimate_r > 0 ? no_rt_time : 0] initial_infections;
                                                      // baseline reproduction number estimate
  real<lower = 0> gt_mean[estimate_r];                // mean of generation time
  real <lower = 0> gt_sd[estimate_r];                 // sd of generation time
  vector[est_week_eff > 0 ? 2 * Jw + 1 : 0] week_eta;
}

transformed parameters {
  // stored transformed parameters
  vector<lower = 0>[estimate_r > 0 ? rt - 1 : t] noise;   // noise on the mean shifted observed cases
  vector<lower = 0>[t] infections;                        // infections over time
  vector<lower = 0>[rt] reports;                          // reports over time
  vector[estimate_r > 0 ? rt : 0] R;                      // reproduction number over time
  vector[est_week_eff > 0 ? rt : 0] week_eff;
 {
  // temporary transformed parameters
  vector[max_rep] rev_delay;                              // reversed report delay pdf
  vector[max_inc] rev_incubation;                         // reversed incubation period pdf
  vector[t] onsets;                                       // onsets over time
  vector[estimate_r > 0 ? max_gt : 0] rev_generation_time;// reversed generation time pdf
  vector[estimate_r > 0 ? rt : 0] infectiousness;         // infections over time
  vector[M] diagSPD;                                      // spectral density
	vector[M] SPD_eta;                                      // spectral density * noise
  vector[2 * Jw + 1] diagSPD_week;
	vector[2 * Jw + 1] SPD_eta_week;
	
  // reverse the distributions to allow vectorised access
  rev_incubation = rep_vector(1e-5, max_inc);
  for (j in 1:max_inc) {
    rev_incubation[j] +=
        discretised_lognormal_pmf(max_inc - j + 1, inc_mean, inc_sd, max_inc);
  }
  
  rev_delay = rep_vector(1e-5, max_rep);
  for (j in 1:(max_rep)) {
    rev_delay[j] +=
        discretised_lognormal_pmf(max_rep - j + 1, rep_mean, rep_sd, max_rep);
  }

  // GP in noise - spectral densities
	for(m in 1:M){ 
		diagSPD[m] =  sqrt(spd_SE(alpha[1], rho[1], sqrt(lambda(L, m)))); 
	}
	SPD_eta = diagSPD .* eta;
	
	noise = rep_vector(1e-5, estimate_r > 0 ? rt - 1 : t);
  noise = noise + exp(PHI[,] * SPD_eta);

  // initialise infections
  infections = rep_vector(1e-5, t);

  // Estimate Rt and use this estimate to generate infections
  if (estimate_r) {
    // calculate pdf of generation time from distribution
    for (j in 1:(max_gt)) {
       rev_generation_time[j] =
           discretised_gamma_pmf(max_gt - j + 1, gt_mean[estimate_r], gt_sd[estimate_r], max_gt);
     }
  
      R[1] = initial_R[estimate_r];
      for (s in 2:rt) {
        R[s] = R[s - 1] .* noise[s - 1];
      }
      
     // Estimate initial infections not using Rt
     infections[1:no_rt_time] = infections[1:no_rt_time] + 
                                  shifted_cases[1:no_rt_time] .* exp(initial_infections);
      
     //Estimate remaining infections using Rt
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
  
  // onsets from infections
  onsets = convolve(infections, rev_incubation);

  // reports from onsets
 {
   vector[t] reports_hold;
   reports_hold = convolve(onsets, rev_delay);
   reports = reports_hold[(no_rt_time + 1):t];
 }
 
 // Add optional weekly reporting effect
 if (est_week_eff) {
    	
  for(m in 0:Jw){
		diagSPD_week[m+1] = sqrt(q_periodic(alpha[2], rho[2], m)); 
	}
	for(m in 1:Jw){ 
		diagSPD_week[Jw+1+m] = sqrt(q_periodic(alpha[2], rho[2], m)); 
	}
 	SPD_eta_week = diagSPD_week .* week_eta;
	
	week_eff = rep_vector(1e-5, rt);
	week_eff = week_eff + exp(PHI_week[,] * SPD_eta_week);
	
	reports = reports .* week_eff;
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

  // penalised priors for incubation period, and report delay
  target += normal_lpdf(inc_mean | inc_mean_mean, inc_mean_sd) * t;
  target += normal_lpdf(inc_sd | inc_sd_mean, inc_sd_sd) * t;
  target += normal_lpdf(rep_mean | rep_mean_mean, rep_mean_sd) * t;
  target += normal_lpdf(rep_sd | rep_sd_mean, rep_sd_sd) * t;
  

  // estimate rt
  if (estimate_r) {
    // prior on R
    initial_R[estimate_r] ~ gamma(r_alpha, r_beta);
    initial_infections ~ normal(0, 0.1);
    
    // penalised_prior on generation interval
    target += normal_lpdf(gt_mean | gt_mean_mean, gt_mean_sd) * rt;
    target += normal_lpdf(gt_sd | gt_sd_mean, gt_sd_sd) * rt;
  }
  
  if (est_week_eff) {
    week_eta ~ std_normal();
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

