  real L;				                     // boundary value for infections gp
  int<lower=1> M;			               // basis functions for infections gp
  real lengthscale_alpha;            // alpha for gp lengthscale prior
  real lengthscale_beta;             // beta for gp lengthscale prior
  real alpha_sd;                     // standard deviation of the alpha gp kernal parameter
  int gp_type;                       // type of gp, 0 = squared exponential, 1 = 3/2 matern
  int stationary;                    // is underlying gaussian process first or second order
  int fixed;                         //  should a gaussian process be used
