// apply day of the week effect
vector day_of_week_effect(vector reports, int[] day_of_week, vector effect) {
  int t = num_elements(reports);
  // scale day of week effect
  vector[7] scaled_effect = 7 * effect;
  vector[t] scaled_reports;
  for (s in 1:t) {
    // add reporting effects (adjust for simplex scale)
    scaled_reports[s] = reports[s] * scaled_effect[day_of_week[s]];
   }
  return(scaled_reports);
}
// Scale observations by fraction reported and update log density of 
// fraction reported
vector scale_obs(vector reports, real frac_obs) {
  int t = num_elements(reports);
  vector[t] scaled_reports;
  scaled_reports = reports * frac_obs;
  return(scaled_reports);
}
// Calculate a truncation CMF
vector truncation_cmf(real trunc_mean, real trunc_sd, int trunc_max) {
    int  trunc_indexes[trunc_max];
    vector[trunc_max] cmf;
    for (i in 1:(trunc_max)) {
      trunc_indexes[i] = i - 1;
    }
    cmf = discretised_lognormal_pmf(trunc_indexes, trunc_mean, trunc_sd, trunc_max);   
    cmf[1] = cmf[1] + 1e-8;
    cmf = cumulative_sum(cmf);
    cmf = reverse_mf(cmf, trunc_max);
    return(cmf);
}
// Truncate observed data by some truncation distribution
vector truncate(vector reports, real[] truncation_mean, real[] truncation_sd, 
                int[] truncation_max, int reconstruct) {
  int t = num_elements(reports);
  int truncation = num_elements(truncation_mean);
  vector[t] trunc_reports = reports;
  if (truncation) {
    // Calculate cmf of truncation delay
    int trunc_max = truncation_max[1] > t ? t : truncation_max[1];
    int  trunc_indexes[trunc_max];
    vector[trunc_max] cmf;
    int first_t = t - trunc_max + 1;
    cmf = truncation_cmf(truncation_mean[1], truncation_sd[1], trunc_max);
    // Apply cdf of truncation delay to truncation max last entries in reports
    if (reconstruct) {
      trunc_reports[first_t:t] = trunc_reports[first_t:t] ./ cmf;
    }else{
      trunc_reports[first_t:t] = trunc_reports[first_t:t] .* cmf;
    }
  }
  return(trunc_reports);
}
// Truncation distribution priors
void truncation_lp(real[] truncation_mean, real[] truncation_sd, 
                   real[] trunc_mean_mean, real[] trunc_mean_sd, 
                   real[] trunc_sd_mean, real[] trunc_sd_sd) {
  int truncation = num_elements(truncation_mean);
  if (truncation) {
    truncation_mean ~ normal(trunc_mean_mean, trunc_mean_sd);
    truncation_sd ~ normal(trunc_sd_mean, trunc_sd_sd);
  }                     
}
// update log density for reported cases
void report_lp(int[] cases, vector reports, 
               real[] rep_phi, int phi_prior,
               int model_type, real weight) {
  real sqrt_phi;
  if (model_type) {
    // the reciprocal overdispersion parameter (phi)
    rep_phi[model_type] ~ normal(0, phi_prior) T[0,];
    sqrt_phi = 1 / sqrt(rep_phi[model_type]);
    // defer to poisson if phi is large, to avoid overflow
    if (sqrt_phi > 1e4) {
      target += poisson_lpmf(cases | reports) * weight;
    } else {
      target += neg_binomial_2_lpmf(cases | reports, sqrt_phi) * weight;
    }
  } else {
    target += poisson_lpmf(cases | reports) * weight;
  }
}
// update log likelihood (as above but not vectorised and returning log likelihood)
vector report_log_lik(int[] cases, vector reports, 
                      real[] rep_phi, int model_type, real weight) {
    int t = num_elements(reports);
    vector[t] log_lik;
    if (model_type) {
    // the reciprocal overdispersion parameter (phi)
    real sqrt_phi = 1 / sqrt(rep_phi[model_type]);
    // defer to poisson if phi is large, to avoid overflow
    if (sqrt_phi > 1e4) {
      for (i in 1:t) {
        log_lik[i] = poisson_lpmf(cases[i] | reports[i]) * weight;
      }
    } else {
      for (i in 1:t) {
        log_lik[i] = neg_binomial_2_lpmf(cases[i] | reports[i], sqrt_phi) * weight;
      }
    }
  } else {
    for (i in 1:t) {
      log_lik[i] = poisson_lpmf(cases[i] | reports[i]) * weight;
    }
  }
  return(log_lik);
}
// sample reported cases from the observation model
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
  }else {
    for (s in 1:t) {
      sampled_reports[s] = poisson_rng(reports[s] > 1e8 ? 1e8 : reports[s]);
    }
  }
  return(sampled_reports);
}


