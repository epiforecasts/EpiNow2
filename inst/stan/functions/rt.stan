// helper function to repeat each element of a vector `times` times,
// up to a maximum of max_id`; if pad_zero is set to 1, a zero is
// inserted at the beginning of vec
vector rep_each(vector vec, int times, int max_id, int pad_zero) {
  int vec_length = num_elements(vec);
  vector[max_id] ret = rep_vector(0, max_id);
  int remainder = max_id - (vec_length - 1) * times;
  for (i in 1:(vec_length - 1)) {
    for (j in 1:times) {
      ret[j + (i - 1 + pad_zero) * times] = vec[i];
    }
  }
  // fill remainder with last value
  for (j in 1:remainder) {
    ret[j + (vec_length - 1) * times] = vec[vec_length];
  }
  return(ret);
}

// update a vector of Rts
vector update_Rt(int t, real log_R, vector noise, int[] bps,
                 real[] bp_effects, int stationary, int gp_spacing) {
  // define control parameters
  int bp_n = num_elements(bp_effects);
  int bp_c = 0;
  int gp_n = num_elements(noise);
  // define result vectors
  vector[t] bp = rep_vector(0, t);
  vector[gp_n] gp_noise = rep_vector(0, gp_n);
  vector[t] gp;
  vector[t] R;
  // initialise breakpoints
  if (bp_n) {
    for (s in 1:t) {
      if (bps[s]) {
        bp_c += bps[s];
        bp[s] = bp_effects[bp_c];
      }
    }
    bp = cumulative_sum(bp);
  }
  //initialise gaussian process
  if (gp_n) {
    if (!stationary) {
      gp_noise = cumulative_sum(noise);
    } else {
      gp_noise = noise;
    }
    gp = rep_each(noise, gp_spacing, t, 1 - stationary);
  }
  // Calculate Rt
  R = rep_vector(log_R, t) + bp + gp;
  R = exp(R);
  return(R);
}
// Rt priors
void rt_lp(vector log_R, real[] initial_infections, real[] initial_growth,
           real[] bp_effects, real[] bp_sd, int bp_n, int seeding_time,
           real r_logmean, real r_logsd, real prior_infections,
           real prior_growth) {
  // prior on R
  log_R ~ normal(r_logmean, r_logsd);
  //breakpoint effects on Rt
  if (bp_n > 0) {
    bp_sd[1] ~ normal(0, 0.1) T[0,];
    bp_effects ~ normal(0, bp_sd[1]);
  }
  // initial infections
  initial_infections ~ normal(prior_infections, 0.2);
  if (seeding_time > 1) {
    initial_growth ~ normal(prior_growth, 0.2);
  }
}
