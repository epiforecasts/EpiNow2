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
        pos = pos + length;
    }
   return(ccases);
  }
// Convolve multiple pmfs
vector convolve_pmfs(vector pmfs, int[] mdelay, int delays) {
  int cdelay = sum(mdelay);
  vector[cdelay] ppmf;
  int pmf_index;
  vector[cdelay] cpmf = rep_vector(0, cdelay); 
  cpmf[1:mdelay[1]] = head(pmfs, mdelay[1]);
  if (delays > 1) {
    for (s in 2:delays) {
      //P(Z = z) = sum_over_x(P(X = x) * P(Y = z - x))
      // indexing tweaked to account for starting at 1 in stan
      // this makes z = x an allowed contribution
      ppmf = rep_vector(0, cdelay);
      pmf_index = sum(mdelay[1:(s - 1)]);
      for (z in 1:cdelay) {
        for (x in 1:mdelay[s]){
          if (z - x >= 0) { 
            ppmf[z] += pmfs[pmf_index + x] * cpmf[z - x + 1];
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
// Build a matrix that maps repeated distributions
// id_rep_dists(list(matrix(c(1, 2, 1, 4, 1, 4), nrow = 3)), 1, 3, 2, 1e-3)
int[,] id_rep_dists(matrix[] dists, int v, int r, int c, real thres) {
  matrix[r, r] rdists = rep_matrix(0, r, r);
  int idists[r, r];
  for (l in 1:v) {
    for (i in 1:r) {
      for (j in 1:c) {
        for (k in 1:r) {
          rdists[i, k] += fabs(dists[l, k, j] - dists[l, i, j]);
        }
      }
    }
  }
  for (i in 1:r) {
    for (j in 1:r) {
      if (i == j) {
        idists[i, j] = 0;
      }else{
        idists[i, j] = rdists[i, j] < thres ? 1 : 0;
      }
    }
  }
  return(idists);
}
// Calculate and convolve multiple delays and then cast to required dimension
// vector_pmf(c(1, 1.5, 1), c(0.4, 0.6, 0.4), 15, 1, 3, rep(1, 3), 3, 1)
vector vector_pmf(vector dmean, vector dsd, int[] dmax, int dists, int ddim,
                  int[] broadcast, int t, int reverse) {
  int dtotal = sum(dmax);
  vector[dtotal] spmf[ddim];
  vector[t * dtotal] pmfs;
  matrix[ddim, dists] dist_params[2];
  vector[dists] sdmean;
  vector[dists] sdsd;
  int rep_dists[ddim, ddim];
  int rep;
  int pos = 0;
  // allocate parameters into a matrix
  for (s in 1:ddim) {
    int dist_pos;
    for (d in 1:dists) {
      dist_pos = s + (d - 1) * ddim;
      dist_params[1, s, d] = dmean[dist_pos];
      dist_params[2, s, d] = dsd[dist_pos];
    }
  }
  // Check for repeated distributions
  rep_dists = id_rep_dists(dist_params, 2, ddim, dists, 1e-4);
  // Update PMFs either using previous or newly calculated
  for (s in 1:ddim) {
    rep = sum(rep_dists[s, 1:s]);
    if (rep) {
      int match = 0;
      int k = 1;
      while (match < 1) {
        if (rep_dists[s, k] == 1) {
          spmf[s] = spmf[k];
          match += 1;
        }
        k += 1;
      }
    }else{
      sdmean = to_vector(dist_params[1, s]);
      sdsd = to_vector(dist_params[2, s]);
      spmf[s] = static_pmf(sdmean, sdsd, dmax, dists, reverse);
    }
    // Broadcast PMFs over a vector
    for (i in 1:broadcast[s]) {
      pmfs[(pos + 1):(pos + dtotal)] = spmf[s];
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
