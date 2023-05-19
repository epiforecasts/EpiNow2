// calculate infectiousness (weighted sum of the generation time and infections)
// for a single time point
real update_infectiousness(vector infections, vector gt_rev_pmf,
                           int seeding_time, int index){
  int gt_max = num_elements(gt_rev_pmf);
  // work out where to start the convolution of past infections with the
  // generation time distribution: (current_time - maximal generation time) if
  // that is >= 1, otherwise 1
  int inf_start = max(1, (index + seeding_time - gt_max));
  // work out where to end the convolution: (current_time - 1)
  int inf_end = (index + seeding_time - 1);
  // number of indices of the generation time to sum over (inf_end - inf_start + 1)
  int pmf_accessed = min(gt_max, index + seeding_time - 1);
  // calculate the elements of the convolution
  real new_inf = dot_product(
    infections[inf_start:inf_end], tail(gt_rev_pmf, pmf_accessed)
  );
  return(new_inf);
}

// generate infections by using Rt = Rt-1 * sum(reversed generation time pmf * infections)
vector generate_infections(real aa, vector oR, int uot, vector gt_rev_pmf,
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
    infectiousness[s] += update_infectiousness(infections, gt_rev_pmf, uot, s);
      if(aa > 0){  
        if (s > nht ){
          R[s] = exp(log(R[s-1]) - aa*infections[s-1]);
        }
      }
      infections[s + uot] += R[s] * infectiousness[s];
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
