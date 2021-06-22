  int gps;
  real L[gps];				                     // boundary value for infections gp
  int<lower=1> M[gps];			               // basis functions for infections gp
  real ls_meanlog[gps];                   // meanlog for gp lengthscale prior
  real ls_sdlog[gps];                     // sdlog for gp lengthscale prior
  real<lower=0> ls_min[gps];              // Lower bound for the lengthscale
  real<lower=0> ls_max[gps];              // Upper bound for the lengthscale
  real alpha_sd[gps];                     // standard deviation of the alpha gp kernal parameter
  int gp_type[gps];                       // type of gp, 0 = squared exponential, 1 = 3/2 matern
  int gp_order[gps];                    // is underlying gaussian process first or second order
  int gp_dims[gps];
  int gp_adj_dims[gps];
  int gp_steps[gps, max(gp_adj_dims)];
  int gp_mat_dim;
  int gp_dim;
  int gp_start[gps];
  int gp_end[gps];
