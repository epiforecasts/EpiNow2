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
// generate infections by using either Rt = Rt-1 * sum(reversed generation time pmf * infections) or Rt = Rt - 1 * exp(infections * Rt)
vector generate_infections(vector oR, int uot,
                           real[] gt_mean, real[] gt_sd, int max_gt,
                           real[] initial_infections, real[] initial_growth,
                           int pop, int ht, int R_gp) {
  // time indices and storage
  int ot = num_elements(oR);
  int nht = ot - ht;
  int t = ot + uot;
  vector[ot] R = oR;
  real exp_new_inf;
  real exp_adj_Rt;
  vector[t] infections = rep_vector(1e-5, t);
  vector[ot] cum_infections = rep_vector(0, ot);
  vector[R_gp * ot] infectiousness;
  // generation time pmf
  vector[R_gp * max_gt] gt_pmf;
  int gt_indexes[R_gp * max_gt];
  if (R_gp) {
    infectiousness = rep_vector(1e-5, ot);
    gt_pmf = rep_vector(1e-5, max_gt);
    for (i in 1:(max_gt)) {
      gt_indexes[i] = max_gt - i + 1;
    }
    gt_pmf = gt_pmf + discretised_gamma_pmf(gt_indexes, gt_mean[1], gt_sd[1], max_gt);
  }
  // Initialise infections using daily growth
  infections[1] = exp(initial_infections[1]);
  if (uot > 1) {
    for (s in 2:uot) {
      infections[s] = exp(initial_infections[1] + initial_growth[1] * (s - 1));
    }
  }
  // calculate cumulative infections
  if (pop) {
    cum_infections[1] = sum(infections[1:uot]);
  }
  // iteratively update infections
  for (s in 1:ot) {
    if (R_gp) {
      infectiousness[s] += update_infectiousness(infections, gt_pmf, uot, max_gt, s);
      exp_new_inf = R[s] * infectiousness[s];
    } else{
      exp_new_inf = infections[s + uot - 1] * exp(R[s]);
    }
    if (pop && s > nht) {
      exp_adj_Rt = exp(-exp_new_inf / (pop - cum_infections[nht]));
      exp_adj_Rt = exp_adj_Rt > 1 ? 1 : exp_adj_Rt;
      infections[s + uot] = (pop - cum_infections[s]) * (1 - exp_adj_Rt);
    }else{
      infections[s + uot] += exp_new_inf;
    }
    if (pop && s < ot) {
      cum_infections[s + 1] = cum_infections[s] + infections[s + uot];
    }
  }
  return(infections);
}
// backcalculate infections using mean shifted cases and non-parametric noise
vector deconvolve_infections(vector shifted_cases, vector noise, int fixed,
                             int prior) {
  int t = num_elements(shifted_cases);
  vector[t] infections = rep_vector(1e-5, t);
  if(!fixed) {
    vector[t] exp_noise = exp(noise);
    if (prior == 1) {
      infections = infections + shifted_cases .* exp_noise;
    }else if (prior == 0) {
     infections = infections + exp_noise;
    }else if (prior == 2) {
      infections[1] = infections[1] + shifted_cases[1] * exp_noise[1];
      for (i in 2:t) {
        infections[i] = infections[i - 1] * exp_noise[i];
      }
    }
  }else{
    infections = infections + shifted_cases;
  }
  return(infections);
}
// Update the log density for the generation time distribution mean and sd
void generation_time_lp(real[] gt_mean, real gt_mean_mean, real gt_mean_sd,
                        real[] gt_sd, real gt_sd_mean, real gt_sd_sd, int weight) {
    target += normal_lpdf(gt_mean[1] | gt_mean_mean, gt_mean_sd) * weight;
    target += normal_lpdf(gt_sd[1] | gt_sd_mean, gt_sd_sd) * weight;
}

