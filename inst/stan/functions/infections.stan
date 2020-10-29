real update_infectiousness(vector infections, vector gt_pmf,
                           int seeding_time, int max_gt, int index){
  int inf_start = max(1, (index + seeding_time - max_gt));
  int inf_end = (index + seeding_time - 1);
  int pmf_accessed = min(max_gt, index + seeding_time - 1);
  real new_inf = dot_product(infections[inf_start:inf_end], tail(gt_pmf, pmf_accessed));
  return(new_inf);
}

vector initialise_infections(int t, real initial_infectiousness, 
                             vector gt_pmf, int max_gt, 
                             int seeding) {
  vector[t] infections = rep_vector(1e-5, t);
  int index = min(seeding, max_gt);
  int inf_start = seeding - index + 1;
  vector[index] seed_inf = initial_infectiousness ./ tail(gt_pmf, index);
  infections[inf_start:seeding] = infections[inf_start:seeding] + seed_inf;
  return(infections);
}

vector generate_infections(vector R, int seeding_time, 
                           real[] gt_mean, real[] gt_sd, int max_gt,
                           vector initial_infectiousness) {
  // time indices
  int rt = num_elements(R);
  int t = rt + seeding_time;
  vector[t] infections;
  vector[rt] infectiousness = rep_vector(1e-5, rt);
  // generation time pmf
  vector[max_gt] gt_pmf;              
  for (j in 1:(max_gt)) {
    gt_pmf[j] = discretised_gamma_pmf(max_gt - j + 1, gt_mean[1], gt_sd[1], max_gt);
  }
  // initial infections using seed infectiousness
  infections = initialise_infections(t, initial_infectiousness[1], gt_pmf, 
                                     max_gt, seeding_time);
  // iteratively update infections
  for (s in 1:rt) {
    if (s == 1) {
      infectiousness[s] += initial_infectiousness[1];
    }else{
      infectiousness[s] += update_infectiousness(infections, gt_pmf, seeding_time, max_gt, s);
    }
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

