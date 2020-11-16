// Update an individual breakpoint effect
real update_breakpoints(real input_R, real[] bp_effects,
                        int bp_index, int at_bp,
                        int stationary) {
  real R = input_R;
  if (stationary) {
    if (bp_index > 0) {
      R += sum(bp_effects[1:bp_index]);
      }
  }else{
    if (at_bp) {
      R += bp_effects[bp_index];
    }
  }               
  return(R);
}
// update an individual R
real update_R(vector R, vector noise, int noise_terms,
              int index, int stationary) {
  real cR = R[index];
  if (noise_terms > 0) {
    if (stationary){
      if (index <= noise_terms) {
        cR += noise[index];
      }else{
        if (index > 1) {
           cR = R[index - 1];
        }
      }
    }else{
      if (index <= (noise_terms + 1)) {
        cR = R[index - 1] + noise[index - 1];
      }else{
        cR = R[index - 1];
      }
    }
  }
  return(cR);
}
// update a vector of Rts
vector update_Rt(vector input_R, real log_R, vector noise, int[] bps,
                 real[] bp_effects, int stationary) {
  // define control parameters
  int noise_terms = num_elements(noise);
  int i_stationary = noise_terms > 0 ? stationary : 1;
  int t = num_elements(input_R);
  int bp_n = num_elements(bp_effects);
  int bp_in = 0;
  int at_bp = 0;
  int index;
  vector[t] R;
  // initialise Rt
  if (i_stationary) {
    R = rep_vector(log_R, t);
    index = 1;
  }else{
    R[1] = log_R;
    index = 2;
  }
  // iteratively update Rt
  for (s in index:t) {
    R[s] = update_R(R, noise, noise_terms, s, i_stationary);
    if (bp_n > 0) {
      at_bp = bps[s];
      bp_in += at_bp;
      R[s] = update_breakpoints(R[s], bp_effects, bp_in, at_bp, i_stationary);
    }
  }
  // convert to correct scale
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
