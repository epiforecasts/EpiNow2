
vector calculate_Rt(vector infections, int seeding_time,
                    real gt_mean, real gt_sd, int max_gt) {
  vector[max_gt] gt_pmf;  
  int gt_indexes[max_gt];
  int t = num_elements(infections);
  int ot = t - seeding_time;
  vector[ot] R;
  vector[ot] infectiousness = rep_vector(1e-5, ot); 
  // calculate PMF of the generation time
  for (i in 1:(max_gt)) {
    gt_indexes[i] = max_gt - i + 1;
  }
  gt_pmf = discretised_gamma_pmf(gt_indexes, gt_mean, gt_sd, max_gt);
  // calculate Rt using Cori et al. approach
  for (s in 1:ot) {
    infectiousness[s] += update_infectiousness(infections, gt_pmf, seeding_time, max_gt, s);
    R[s] = infections[s + seeding_time] / infectiousness[s];
  }
  return(R);
}

real[] R_to_growth(vector R, real gt_mean, real gt_sd) {
  real k = pow(gt_sd / gt_mean, 2);
  int t = num_elements(R);
  real r[t];
  for (s in 1:t) {
    r[s] = (pow(R[s], k) - 1) / (k * gt_mean);
  } 
  return(r);
}

int[] report_rng(vector reports, real[] rep_phi, int model_type) {
  int t = num_elements(reports);
  int sampled_reports[t];
  real sqrt_phi;
  if (model_type) {
    sqrt_phi = 1 / sqrt(rep_phi[model_type]);
    for (s in 1:t) {
      // defer to poisson if phi is large, to avoid overflow
      if (sqrt_phi > 1e4) {
        sampled_reports[s] = poisson_rng(reports[s] > 1e8 ? 1e8 : reports[s]);
      } else {
        sampled_reports[s] = neg_binomial_2_rng(reports[s] > 1e8 ? 1e8 : reports[s], sqrt_phi);
      }
    }
  } else {
    for (s in 1:t) {
      sampled_reports[s] = poisson_rng(reports[s] > 1e8 ? 1e8 : reports[s]);
    }
  }
  return(sampled_reports);
}


