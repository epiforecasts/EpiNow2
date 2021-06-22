// eigenvalues for approximate hilbert space gp
// see here for details: https://arxiv.org/pdf/2004.11408.pdf
real lambda(real L, int m) {
  real lam;
  lam = ((m*pi())/(2*L))^2;
  return lam;
}
// eigenfunction for approximate hilbert space gp
// see here for details: https://arxiv.org/pdf/2004.11408.pdf
vector phi(real L, int m, vector x) {
  vector[rows(x)] fi;
  fi = 1/sqrt(L) * sin(m*pi()/(2*L) * (x+L));
  return fi;
}
// spectral density of the exponential quadratic kernal
real spd_se(real alpha, real rho, real w) {
  real S;
  S = (alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho^2)*(w^2));
  return S;
}
// spectral density of the Matern 3/2 kernel
real spd_matern(real alpha, real rho, real w) {
  real S;
  S = 4*alpha^2 * (sqrt(3)/rho)^3 * 1/((sqrt(3)/rho)^2 + w^2)^2;
  return S;
}
// setup gaussian process noise dimensions
int setup_noise(int ot_h, int t, int horizon, int estimate_r,
                int stationary, int future_fixed, int fixed_from) {
  int noise_time = estimate_r > 0 ? (stationary > 0 ? ot_h : ot_h - 1) : t;
  int noise_terms =  future_fixed > 0 ? (noise_time - horizon + fixed_from) : noise_time;
  return(noise_terms);
}
// setup approximate gaussian process
matrix setup_gp(int M, real L, int dimension) {
  vector[dimension] time;
  matrix[dimension, M] PHI;
  real half_dim = dimension / 2.0;
  for (s in 1:dimension) {
    time[s] = (s - half_dim) / half_dim;
  }
  for (m in 1:M){
    PHI[,m] = phi(L, m, time);
  }
  return(PHI);
}

vector setup_gps(int gps, int[] M, real[] L, int[] dim,
                 int gp_mat_dim) {
  vector[gp_mat_dim]  PHI;
  int pos = 1;
  for (i in 1:gps) {
    PHI[pos:(pos + seg - 1)] = to_vector(setup_gp(M[i], L[i], dim[i]));
    pos = pos + dim[i] * M[i];
  }
  return(PHI);
}

// update gaussian process using spectral densities
vector update_gp(matrix PHI, int M, real L, real alpha,
                 real rho, vector eta, int type) {
  vector[M] diagSPD;    // spectral density
  vector[M] SPD_eta;    // spectral density * noise
  int gp_dim = rows(PHI);
  vector[gp_dim] gp = rep_vector(1e-6, gp_dim);
  real unit_rho = rho / gp_dim;
  // GP - spectral densities
  if (type == 0) {
    for(m in 1:M){
      diagSPD[m] =  sqrt(spd_se(alpha, unit_rho, sqrt(lambda(L, m))));
    }
  }else if (type == 1) {
    for(m in 1:M){
      diagSPD[m] =  sqrt(spd_matern(alpha, unit_rho, sqrt(lambda(L, m))));
    }
  }
  SPD_eta = diagSPD .* eta;
  gp = gp + PHI[,] * SPD_eta;
  return(gp);
}

// Update multiple GPs and apply order settings
vector update_gps(vector PHI, int gps, int[] vdim, int[] adj_vdim, int dim,
                  int[,] steps, int[] M, real[] L, real[] alpha,
                  real[] rho_raw, vector eta, real[] ls_min, real[] ls_max,
                  int[] order, int[] type) {
  vector[dim] gp;
  int pos = 1;
  int phi_pos = 1;
  int eta_pos = 1;
  real rho;
  for (i in 1:gps) {
    //set up in loop parameters and scale lengthscale
    int l = adj_vdim[i];
    vector[l] sgp;
    vector[vdim[i]] bsgp;
    rho = ls_min[gps] + (ls_max[gps] - ls_min[gps]) * rho_raw[gps];
    // update GP using spectral density
    sgp = update_gp(
      to_matrix(segment(PHI, phi_pos, l * M[i]),
      l, M[i]), M[i], L[i], alpha[i], rho,
      segment(eta, eta_pos, M[i]), type[i]);
    // change to first differences
    if (order[i]) {
      sgp = cumulative_sum(sgp);
      gp[pos] = 0;
    }
    // project GP over timesteps held constant
    for (j in 1:l) {
      bgp[pos:(pos - 1 + steps[i, j])] = rep_vector(sgp[j], steps[i, j]);
      pos += steps[i, j]
    }

    gp[(pos + order[i]):(pos + vdim[i] - 1)] = bgp;
    pos = pos + vdim[i];
    phi_pos = phi_pos + l * M[i];
    eta_pos = eta_pos + M[i];
  }
  return(gp);
}

// priors for gaussian process
void gaussian_process_lp(real[] rho, real[] alpha, vector eta,
                         real[] ls_meanlog, real[] ls_sdlog,
                         real[] alpha_sd) {
  rho ~ lognormal(ls_meanlog, ls_sdlog);
  alpha ~ normal(0, alpha_sd);
  eta ~ std_normal();
}
