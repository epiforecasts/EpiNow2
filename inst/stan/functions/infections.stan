// calculate infectiousness (weighted sum of the generation time and infections)
// for a single time point
real update_infectiousness(vector infections, vector gt_rev_pmf,
                           int seeding_time, int index){
  int gt_length = num_elements(gt_rev_pmf);
  // work out where to start the convolution of past infections with the
  // generation time distribution: (current_time - maximal generation time) if
  // that is >= 1, otherwise 1
  int inf_start = max(1, (index + seeding_time - gt_length + 1));
  // work out where to end the convolution: current_time
  int inf_end = (index + seeding_time);
  // number of indices of the generation time to sum over (inf_end - inf_start + 1)
  int pmf_accessed = min(gt_length, index + seeding_time);
  // calculate the elements of the convolution
  real new_inf = dot_product(
    infections[inf_start:inf_end], tail(gt_rev_pmf, pmf_accessed)
  );
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
vector renewal_model(vector oR, vector uobs_infs, vector gt_rev_pmf,
                     int pop, int ht) {
  // time indices and storage
  int ot = num_elements(oR);
  int uot = num_elements(uobs_infs);
  int nht = ot - ht;
  int t = ot + uot;
  vector[ot] R = oR;
  real exp_adj_Rt;
  vector[t] infections = rep_vector(0, t);
  vector[ot] cum_infections;
  vector[ot] infectiousness;
  // Initialise infections
  infections[1:uot] = uobs_infs;
  // calculate cumulative infections
  if (pop) {
    cum_infections[1] = sum(infections[1:uot]);
  }
  // iteratively update infections
  for (s in 1:ot) {
    infectiousness[s] = update_infectiousness(infections, gt_rev_pmf, uot, s);
    if (pop && s > nht) {
      exp_adj_Rt = exp(-R[s] * infectiousness[s] / (pop - cum_infections[nht]));
      exp_adj_Rt = exp_adj_Rt > 1 ? 1 : exp_adj_Rt;
      infections[s + uot] = (pop - cum_infections[s]) * (1 - exp_adj_Rt);
    }else{
      infections[s + uot] = R[s] * infectiousness[s];
    }
    if (pop && s < ot) {
      cum_infections[s + 1] = cum_infections[s] + infections[s + uot];
    }
  }
  return(infections);
}

// update infections using a growth model (linear,log, or non-parametric growth)
vector growth_model(vector r, int ht, vector uobs_infs,
                    int prior, vector constant) {
  // time indices and storage
  int ot = num_elements(r);
  int uot = num_elements(seed_infections);
  int nht = ot - ht;
  int t = ot + uot;
  vector[t] infections = rep_vector(1e-5, t);
  vector[ot] obs_inf;
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
