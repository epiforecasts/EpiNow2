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

vector setup_gps(int gps, int[] M, real[] L, int[] gp_dim, int[] order,
                 int gp_mat_dim) {
  vector[gp_mat_dim]  PHI;
  int pos = 1;
  for (i in 1:gps) {
    int seg = gp_dim[i] * M[i];
    PHI[pos:(pos + seg - 1)] = to_vector(setup_gp(M[i], L[i], gp_dim[i]));
    pos = pos + seg;
  }
  return(PHI);
}

// update gaussian process using spectral densities
vector update_gp(matrix PHI, int M, real L, real alpha,
                 real rho, vector eta, int type) {
  vector[M] diagSPD;    // spectral density
  vector[M] SPD_eta;    // spectral density * noise
  int noise_terms = rows(PHI);
  vector[noise_terms] noise = rep_vector(1e-6, noise_terms);
  real unit_rho = rho / noise_terms;
  // GP in noise - spectral densities
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
  noise = noise + PHI[,] * SPD_eta;
  return(noise);
}

// Update multiple GPs and apply order settings
vector update_gps(vector PHI, int gps, int[] gp_dims, int gp_dim, int[] M,
                  real[] L, real[] alpha, real[] rho_raw, vector eta,
                  real[] ls_min, real[] ls_max, int[] order, int[] gp_type) {
  vector[gp_dim] gp;
  int pos = 1;
  int phi_pos = 1;
  int eta_pos = 1;
  real rho;
  int length;
  for (i in 1:gps) {
    rho = ls_min[gps] + (ls_max[gps] - ls_min[gps]) * rho_raw[gps];
    length = gp_dims[i] - gp_order[i];
    vector[length] gp_tmp;
    gp_tmp = update_gp(
      to_matrix(segment(PHI, phi_pos, length * M[i]),
      length, M[i]), M[i], L[i], alpha[i], rho,
      segment(eta, eta_pos, M[i]), gp_type[i]
    );
    if (order[i]) {
      gp_tmp = cumulative_sum(gp_tmp);
      gp[pos] = 0;
    }
    gp[(pos + order[i]):(pos + gp_dims[i] - 1)] = gp_tmp;
    pos = pos + gp_dims[i];
    phi_pos = phi_pos + length * M[i];
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
