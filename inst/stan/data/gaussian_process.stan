  real L;				                     // boundary value for infections gp
  int<lower=1> M;			               // basis functions for infections gp
  real ls_meanlog;                   // meanlog for gp lengthscale prior
  real ls_sdlog;                     // sdlog for gp lengthscale prior
  real<lower=0> ls_min;              // Lower bound for the lengthscale
  real<lower=0> ls_max;              // Upper bound for the lengthscale
  real alpha_sd;                     // standard deviation of the alpha gp kernal parameter
  int gp_type;                       // type of gp, 0 = squared exponential, 1 = periodic, 2 = Matern
  real nu;                           // smoothness parameter for Matern kernel (used if gp_type = 2)
  real w0;                           // fundamental frequency for periodic kernel (used if gp_type = 1)
  int stationary;                    // is underlying gaussian process first or second order
  int stationary;                    // is underlying gaussian process first or second order
  int fixed;                         //  should a gaussian process be used
