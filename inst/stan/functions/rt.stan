

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

vector update_Rt(vector input_R, real log_R, vector noise, int[] bps,
                 real[] bp_effects, int stationary) {
  // define control parameters
  int noise_terms = num_elements(noise);
  int i_stationary = noise_terms > 0 ? stationary : 1;
  int bp_stat;
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
      bp_stat = index > noise_terms ? 0 : i_stationary;
      R[s] = update_breakpoints(R[s], bp_effects, bp_in, at_bp, bp_stat);
    }
  }
  // convert to correct scale
  R = exp(R);
  return(R);
}

