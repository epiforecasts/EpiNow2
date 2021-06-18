// convolve a pdf and case vector
vector convolve(vector cases, vector pmf, int length) {
    int t = num_elements(cases);
    int pos = 1;
    vector[length] seg_pmf;
    vector[t] ccases = rep_vector(1e-5, t);
    for (s in 1:t) {
        seg_pmf = segment(pmf, pos, length);
        ccases[s] += dot_product(cases[max(1, (s - length + 1)):s],
                                 tail(seg_pmf, min(length, s)));
        pos = pos + 1;
    }
   return(ccases);
  }
// Convolve multiple pmfs
vector convolve_pmfs(vector pmfs, int[] mdelay, int delays) {
  int cdelay = sum(mdelay);
  vector[cdelay] ppmf;
  vector[cdelay] cpmf = rep_vector(0, cdelay); 
  cpmf[1:mdelay[1]] = head(pmfs, mdelay[1]);
  if (delays > 1) {
    for (s in 2:delays) {
      //P(Z = z) = sum_over_x(P(X = x) * P(Y = z - x))
      // indexing tweaked to account for starting at 1 in stan
      // this makes z = x an allowed contribution
      ppmf = rep_vector(0, cdelay);
      for (z in 1:cdelay) {
        for (x in 1:mdelay[s]){
          if (z - x >= 0) { 
            ppmf[z] += pmfs[sum(mdelay[1:(s - 1)]) + x] * cpmf[z - x + 1];
          }
        }
      }
      cpmf = ppmf;
    }
  }
  return(cpmf);
}
// Calculate and convolve multiple delays to produce a single pmf 
vector static_pmf(vector dmean, vector dsd, int[] dmax, int dists,
                  int reverse) {
  int dtotal = sum(dmax);
  vector[dtotal] pmf;
  pmf = calculate_pmfs(dmean, dsd, dmax);
  pmf = convolve_pmfs(pmf, dmax, dists);
  if (reverse) {
    pmf = reverse_mf(pmf, dtotal);
  }
  return(pmf);
}
// Calculate and convolve multiple delays and then cast to required dimension
vector vector_pmf(vector dmean, vector dsd, int[] dmax, int dists, int ddim,
                  int[] broadcast, int t, int reverse) {
  int dtotal = sum(dmax);
  vector[dtotal] spmf;
  vector[t * dtotal] pmfs;
  int pos = 0;
  for (s in 1:ddim) {
    vector[dists] sdmean;
    vector[dists] sdsd;
    int dist_pos;
    for (d in 1:dists) {
      dist_pos = s + (d - 1) * ddim;
      sdmean[d] = dmean[dist_pos];
      sdsd[d] = dsd[dist_pos];
    }
    spmf = static_pmf(sdmean, sdsd, dmax, dists, reverse);
    for (i in 1:broadcast[s]) {
      pmfs[(pos + 1):(pos + dtotal)] = spmf;
      pos += dtotal;
    }
  }
  return(pmfs);
}
// Cast a vector of parameters to the required dimension and modifier
vector vector_param(vector param, vector mod, int no, int dim,
                    int[] broadcast, int[] mod_pres, int t) {
  vector[t * no] params;
  int no_pos = 1;
  int mod_pos = 1;
  int pos;
  int dim_pos;
  int i;
  for (s in 1:no) {
    vector[t] paramt;
    pos = 1;
    for (d in 1:dim) {
      dim_pos = d + (s - 1) * no;
      i = broadcast[s];
      paramt[pos:(pos + i - 1)] = rep_vector(param[dim_pos], i);
      pos += i;
    }
    if (mod_pres[s]) {
      paramt = paramt .* exp(segment(mod, mod_pos, t));
      mod_pos += t;
    }
    params[no_pos:(no_pos + t - 1)] = paramt;
    no_pos += t;
  }
  return(params);
}

// convolve count data by a pmf if required and otherwise trim
// count data as requested
vector convolve_counts(vector infections, vector pmfs,
                       int mdelay, int seeding_time) {
  int t = num_elements(infections);
  vector[t - seeding_time] reports;
  vector[t] unobs_reports = infections;
  if (mdelay) {
    unobs_reports = convolve(unobs_reports, pmfs, mdelay);
    reports = unobs_reports[(seeding_time + 1):t];
  }else{
    reports = infections[(seeding_time + 1):t];
  }
  return(reports);
}
// Priors for distributional parameters
void delays_lp(vector dmean, real[] dmean_mean, real[] dmean_sd,
               vector dsd, real[] dsd_mean, real[] dsd_sd, int weight){
    int delays = num_elements(dmean);
    if (delays) {
      for (s in 1:delays) {
       target += normal_lpdf(dmean[s] | dmean_mean[s], dmean_sd[s]) * weight;
       target += normal_lpdf(dsd[s] | dsd_mean[s], dsd_sd[s]) * weight;
     }
  }
}
