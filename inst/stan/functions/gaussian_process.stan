  // exponential quadratic kernal
	real spd_SE(real alpha, real rho, real w) {
		real S;
		S = (alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho^2)*(w^2));
		return S;
	}
	
	// basis function for approximate hilbert space gp
	// see here for details: https://arxiv.org/pdf/2004.11408.pdf
	vector phi_SE(real L, int m, vector x) {
		vector[rows(x)] fi;
		fi = 1/sqrt(L) * sin(m*pi()/(2*L) * (x+L));
		return fi;
	}
	
	// eigenvalues for approximate hilbert space gp
	// see here for details: https://arxiv.org/pdf/2004.11408.pdf
	real lambda(real L, int m) {
		real lam;
		lam = ((m*pi())/(2*L))^2;
		return lam;
	}


int setup_noise(int ot_h, int t, int horizon, int estimate_r, 
                int stationary, int future_fixed, int fixed_from) {
  int noise_time = estimate_r > 0 ? (stationary > 0 ? ot_h : ot_h - 1) : t;
  int noise_terms =  future_fixed > 0 ? (noise_time - horizon + fixed_from) : noise_time; 
  return(noise_terms);
}


matrix setup_gp(int M, real L, int dimension) {
  vector[dimension] time;
  matrix[dimension, M] PHI;
  for (s in 1:dimension) {
    time[s] = s;
  }
  for (m in 1:M){ 
    PHI[,m] = phi_SE(L, m, time); 
  }
  return(PHI);
}


vector update_gp(matrix PHI, int M, real L, real alpha, real rho, vector eta) {
  vector[M] diagSPD;    // spectral density
  vector[M] SPD_eta;    // spectral density * noise
  int noise_terms = rows(PHI);
  vector[noise_terms] noise = rep_vector(1e-6, noise_terms);
  // GP in noise - spectral densities
  for(m in 1:M){ 
    diagSPD[m] =  sqrt(spd_SE(alpha, rho, sqrt(lambda(L, m)))); 
  }
  SPD_eta = diagSPD .* eta;
  noise = noise + PHI[,] * SPD_eta;
  return(noise);
}

void gaussian_process_lp(real[] rho, real[] alpha, vector eta,
                         real ls_alpha, real ls_beta, real alpha_sd) {
  rho ~ inv_gamma(ls_alpha, ls_beta);
  alpha ~ normal(0, alpha_sd);
  eta ~ std_normal();
}
