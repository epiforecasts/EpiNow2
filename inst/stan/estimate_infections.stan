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

  // apply backsampling and upscaling based on a pdf
  vector backsample(vector cases, vector pdf) {
    int t = num_elements(cases);
    vector[t] backsampled_cases;
    int max_upscale = min(t, num_elements(pdf));
    int pdf_length = num_elements(pdf);
    vector[pdf_length] cdf;
    
    backsampled_cases = convolve(cases, pdf, 0);
    
    // apply upscaling
    cdf = cumulative_sum(pdf);
    
    for (i in  1:max_upscale) {
      backsampled_cases[(t - i + 1)] = (backsampled_cases[(t - i + 1)] + 1) / cdf[i];
    }
    
    //bound last day to equal day before
    backsampled_cases[t] = backsampled_cases[t -1];
    
    return(backsampled_cases);
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
}


data {
  int t;                             // number of time steps
  int rt;                            // time over which to estimate rt
  int horizon;                       // forecast horizon
  int day_of_week[t];                // day of the week indicator (1 - 7)
  int <lower = 0> cases[t - horizon];// observed cases
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
}

transformed data{
  real r_alpha;                      // alpha parameter of the R gamma prior
  real r_beta;                       // beta parameter of the R gamma prior
  vector[t] delta;                   // modifier to make GP + definite
  real time[t];                      // time vector
  int no_rt_time;                    // time without estimating Rt
  int t_h;                           // time minus the forecasting horizon
  int rt_h;                          // rt estimation time minus the forecasting horizon
  
  //Update time varables
  t_h = t - horizon;
  rt_h = rt - horizon;
  
  // calculate alpha and beta for gamma distribution
  r_alpha = (r_mean / r_sd)^2;
  r_beta = r_mean / (r_sd^2);
  
  // assign + definite term
   delta = rep_vector(1e-9, t); 

  // make time vector
   for (s in 1:t) {
     time[s] = s;
   }
   
   // time without estimating Rt is the differrence of t and rt
   no_rt_time = t - rt;
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
  vector[t] eta;                                      // unconstrained noise
  vector<lower = 0>[estimate_r] initial_R;            // baseline reproduction number estimate
  real<lower = 1> gt_mean[estimate_r];                // mean of generation time
  real <lower = 0> gt_sd[estimate_r];                 // sd of generation time
  real<lower=0> R_rho[estimate_r];                    // length scale of R GP
  real<lower=0> R_alpha[estimate_r];                  // scale of R GP
  vector[estimate_r > 0 ? rt : 0] R_eta;              // unconstrained R noise
}

transformed parameters {
  // stored transformed parameters
  vector<lower = 0>[t] noise;                             // noise on the mean shifted observed cases
  vector<lower = 0>[t] infections;                        // infections over time
  vector<lower = 0>[t] reports;                           // reports over time
  vector[7] day_of_week_eff;                              // day of the week effect
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
  vector[estimate_r > 0 ? t : 0] branch_infections;       // infections generated by the renewal equation
  matrix[t, t] K;                                         // covariance matrix 
  matrix[t, t] L_K;                                       // cholesky docomposed covariance matrix
  matrix[rt, rt] rK;                                      // covariance matrix for R estimation
  matrix[rt, rt] rL_K;                                    // cholesky docomposed covariance matrix for R estimation
  vector[estimate_r > 0 ? rt : 0] R_noise;                // noise on R centred on 1
  
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

  // GP in noise
  K = cov_exp_quad(time, alpha, rho);
  // diagonal elements with offset to make + definite
  K = K + diag_matrix(delta);

  L_K = cholesky_decompose(K);
  noise = exp(L_K * eta);
  
  for (s in 1:t) {
    if(noise[s] == 0) {
      noise[s] = 0.001;
    }
  }

  // generate infections from prior infections and non-parameteric noise
  infections = shifted_cases .* noise;
  
  // onsets from infections
  onsets = convolve(infections, rev_incubation, 1);

  // reports from onsets
  reports = convolve(onsets, rev_delay, 1);

  for (s in 1:t) {
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
      rK = cov_exp_quad(time[1:rt], R_alpha[estimate_r], R_rho[estimate_r]);
     // diagonal elements with offset to make + definite
      rK = rK + diag_matrix(delta[1:rt]);
      rL_K = cholesky_decompose(rK);
      R_noise = exp(rL_K * R_eta);
  
     // Estimate infections using renewal equation
     branch_infections[1:no_rt_time] = infections[1:no_rt_time];
     
      for (s in 1:rt) {
         R[s] = initial_R[estimate_r] * R_noise[s];
         branch_infections[s + no_rt_time] = R[s] * infectiousness[s + no_rt_time];
         
         // Make sure all dates have a non-zero value
         if (branch_infections[s] == 0){
            branch_infections[s] = 0.001; 
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
      branch_reports[s] *= (day_of_week_eff[day_of_week[s + no_rt_time]]);
    }
  }
 }
}

model {
  
  // priors for noise GP
  rho ~ lognormal(1.609438, 0.5); //log(5)
  alpha ~ std_normal();
  eta ~ std_normal();

  // reporting overdispersion
  if (model_type) {
    rep_phi[model_type] ~ exponential(1);
  }
  
  // daily cases given reports
  if (model_type) {
    target += neg_binomial_2_lpmf(cases | reports[1:t_h], rep_phi[model_type]);
  }else{
    target += poisson_lpmf(cases | reports[1:t_h]);
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
   R_rho ~ lognormal(1.609438, 0.5); //log(5)
   R_alpha ~ std_normal();
   R_eta ~ std_normal();
    
    // penalised_prior on generation interval
    target += normal_lpdf(gt_mean | gt_mean_mean, gt_mean_sd) * rt;
    target += normal_lpdf(gt_sd | gt_sd_mean, gt_sd_sd) * rt;
    
    // Likelihood of Rt given infections
    if (model_type) {
      target += neg_binomial_2_lpmf(cases[(no_rt_time + 1):t_h] | branch_reports[1:rt_h], rep_phi[model_type]);
    }else{
      target += poisson_lpmf(cases[(no_rt_time + 1):t_h] | branch_reports[1:rt_h]);
    }
  }
}
  
generated quantities {
  int imputed_infections[t];
  int imputed_reports[t]; 
  int imputed_branch_reports[estimate_r > 0 ? rt : 0];
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
  //simulate reported cases from the Rt model
  if (estimate_r) {
    if (model_type) {
    imputed_branch_reports = neg_binomial_2_rng(branch_reports, rep_phi[model_type]);
   }else{
    imputed_branch_reports = poisson_rng(branch_reports);
  }
 }
}

