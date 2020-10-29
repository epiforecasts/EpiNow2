real update_infectiousness(vector infections, vector gt_pmf,
                           int max_gt, int index){
  int inf_start = max(1, (index - max_gt));
  int inf_end = (index - 1);
  int pmf_accessed = min(max_gt, index - 1);
  real new_inf = dot_product(infections[inf_start:inf_end], tail(gt_pmf, pmf_accessed));
  return(new_inf);
}

vector generate_infections(vector R, int seeding_time, 
                           real[] gt_mean, real[] gt_sd, int max_gt,
                           real[] initial_infections) {
  // time indices and storage
  int rt = num_elements(R);
  int t = rt + seeding_time;
  vector[t] infections rep_vector(1e-5, t);
  vector[t] infectiousness = rep_vector(1e-5, t);
  real current_R;
  // generation time pmf
  vector[max_gt] gt_pmf;              
  for (j in 1:(max_gt)) {
    gt_pmf[j] = discretised_gamma_pmf(max_gt - j + 1, gt_mean[1], gt_sd[1], max_gt);
  }
  // iteratively update infections
  infections[1] = initial_infections[1];
  for (s in 2:t) {
    infectiousness[s] += update_infectiousness(infections, gt_pmf, max_gt, s);
    if (s <= seeding_time){
      current_R = R[1];
    }else{
      current_R = R[s - seeding_time];
    }
    infections[s] += current_R * infectiousness[s];
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

