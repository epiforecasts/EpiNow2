// calculate infectiousness (weighted sum of the generation time and infections)
// for a single time point
real update_infectiousness(vector infections, vector gt_pmf,
                           int seeding_time, int max_gt, int index){
  int inf_start = max(1, (index + seeding_time - max_gt));
  int inf_end = (index + seeding_time - 1);
  int pmf_accessed = min(max_gt, index + seeding_time - 1);
  real new_inf = dot_product(infections[inf_start:inf_end], tail(gt_pmf, pmf_accessed));
  return(new_inf);
}
// generate seed infections
vector generate_seed(real[] initial_infections, real[] initial_growth, int uot) {
  vector[uot] seed_infs;
  seed_infs[1] = exp(initial_infections[1]);
  if (uot > 1) {
    for (s in 2:uot) {
      seed_infs[s] = exp(initial_infections[1] + initial_growth[1] * (s - 1));
    }
  }
  return(seed_infs)
}
// generate infections using infectiousness
vector generate_infections_with_infectiousness(vector oR, int uot,
                           real[] gt_mean, real[] gt_sd, int max_gt,
                           real[] initial_infections, real[] initial_growth,
                           int pop, int ht) {
  // time indices and storage
  int ot = num_elements(oR);
  int nht = ot - ht;
  int t = ot + uot;
  vector[ot] R = oR;
  real exp_adj_Rt;
  vector[t] infections = rep_vector(1e-5, t);
  vector[ot] cum_infections = rep_vector(0, ot);
  vector[ot] infectiousness = rep_vector(1e-5, ot);
  // Initialise infections
  infections[1:uot] = generate_seed(initial_infections, initial_growth, uot);
  // calculate cumulative infections
  if (pop) {
    cum_infections[1] = sum(infections[1:uot]);
  }
  // generation time pmf
  vector[max_gt] gt_pmf = rep_vector(1e-5, max_gt);
  int gt_indexes[max_gt];
  for (i in 1:(max_gt)) {
    gt_indexes[i] = max_gt - i + 1;
  }
  gt_pmf = gt_pmf + discretised_gamma_pmf(gt_indexes, gt_mean[1], gt_sd[1], max_gt);
  // iteratively update infections
  for (s in 1:ot) {
    infectiousness[s] += update_infectiousness(infections, gt_pmf, uot, max_gt, s);
    if (pop && s > nht) {
      exp_adj_Rt = exp(-R[s] * infectiousness[s] / (pop - cum_infections[nht]));
      exp_adj_Rt = exp_adj_Rt > 1 ? 1 : exp_adj_Rt;
      infections[s + uot] = (pop - cum_infections[s]) * (1 - exp_adj_Rt);
    }else{
      infections[s + uot] += R[s] * infectiousness[s];
    }
    if (pop && s < ot) {
      cum_infections[s + 1] = cum_infections[s] + infections[s + uot];
    }
  }
  return(infections);
}
// Generate infections directly
vector generate_infections_directly(vector r, int uot, int ht
                                    real[] initial_infections, real[] initial_growth, 
                                    int prior, vector constant) {
  // time indices and storage
  int ot = num_elements(r);
  int nht = ot - ht;
  int t = ot + uot;
  vector[t] infections = rep_vector(1e-5, t);
  vector[uot] uobs_inf;
  vector[ot] obs_inf;
  // Initialise infections
  uobs_inf = generate_seed(initial_infections, initial_growth, uot);
  // Update observed infections
  if (link == 0) {
   if (prior == 1) {
    obs_inf = constant .* r;
   }else if (prior == 2) {
     obs_inf[1] = uobs_inf[uot] * r[1];
     for (i in 2:t) {
       obs_inf[i] = obs_inf[i - 1] * r[i];
     }
   }
  }else if (link == 1) {
   if (prior == 1) {
    obs_inf = constant + r;
   }else if (prior == 2) {
     obs_inf[1] = log(uobs_inf[uot]) + r[1];
     for (i in 2:t) {
       obs_inf[i] = obs_inf[i - 1] + r[i];
     }
   }
   obs_inf = exp(obs_inf);
  }

   infections[1:uot] = infections[1:uot] + uobs_inf;
   infections[(uot + 1):t] = infections[(uot + 1):t] + obs_inf;
  return(infections);
}
// Update the log density for the generation time distribution mean and sd
void generation_time_lp(real[] gt_mean, real gt_mean_mean, real gt_mean_sd,
                        real[] gt_sd, real gt_sd_mean, real gt_sd_sd, int weight) {
    target += normal_lpdf(gt_mean[1] | gt_mean_mean, gt_mean_sd) * weight;
    target += normal_lpdf(gt_sd[1] | gt_sd_mean, gt_sd_sd) * weight;
}

