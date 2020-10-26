vector update_infectiousness(vector infections, vector gt_pmf,
                             int seeding_time, int max_gt, int index){
  int inf_start = max(1, (index + seeding_time - max_gt));
  int inf_end = (index + seeding_time - 1);
  int pmf_accessed = min(max_gt, s + seeding_time - 1);
  real new_inf = dot_product(infections[inf_start:inf_end], tail(gt_pmf, pmf_accessed));
  return(new_inf)
}

vector generate_infections(vector R, int seeding_time, real[] gt_mean, 
                           real[] gt_sd, int max_gt, vector shifted_cases,
                           vector initial_infections) {
  int rt = num_elements(R);
  int t = rt + seeding_time;
  vector[t] infections = rep_vector(1e-5, t);
  vector[max_gt] gt_pmf;              // reversed generation time pdf
  vector[rt] infectiousness = rep_vector(1e-5, rt);  // infections over time
  
  for (j in 1:(max_gt)) {
    gt_pmf[j] = discretised_gamma_pmf(max_gt - j + 1, gt_mean[1], gt_sd[1], max_gt);
  }
  // estimate initial infections not using Rt
  infections[1:seeding_time] = infections[1:seeding_time] + shifted_cases[1:seeding_time] .* initial_infections;
  
  for (s in 1:rt) {
    infectiousness[s] += update_infectiousness(infections, gt_pmf, seeding_time, max_gt, s);
    infections[s + seeding_time] += R[s] * infectiousness[s];
  }
  return(infections);
}


vector deconvolve_infections(vector shifted_cases, vector noise, int fixed) {
  int t = num_elements(shifted_cases);
  vector[t] infections = rep_vector(1e-5, t);
  if(!fixed) {
    infections = infections + shifted_cases .* exp(noise);
  }else{
    infections = infections + shifted_cases;
  }
  return(infections);
}

void generation_time_lp(real[] gt_mean, real gt_mean_mean, real gt_mean_sd, 
                        real[] gt_sd, real gt_sd_mean, real gt_sd_sd, int weight) {
    target += normal_lpdf(gt_mean[1] | gt_mean_mean, gt_mean_sd) * weight;
    target += normal_lpdf(gt_sd[1] | gt_sd_mean, gt_sd_sd) * weight;
}

vector calculate_Rt(vector infections, int seeding_time,
                    real gt_mean, real gt_sd, int max_gt) {
  vector[max_gt] gt_pmf;  
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[ot] R;
  vector[ot] infectiousness = rep_vector(1e-5, ot); 
  // calculate PMF of the generation time
  for (j in 1:(max_gt)) {
    gt_pmf[j] =  discretised_gamma_pmf(max_gt - j + 1, gt_mean, gt_sd, max_gt);
  }
  // calculate Rt using Cori et al. approach
  for (s in 1:ot) {
    infectiousness[s] += update_infectiousness(infections, gt_pmf, seeding_time, max_gt, s);
    R[s] = infections[s + seeding_time] / infectiousness[s];
  }
  return(R);
}