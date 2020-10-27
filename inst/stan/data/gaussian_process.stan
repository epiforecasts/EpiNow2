  real L;				                     // boundary value for infections gp
  int<lower=1> M;			               // basis functions for infections gp
  real lengthscale_alpha;            // alpha for gp lengthscale prior
  real lengthscale_beta;             // beta for gp lengthscale prior
  real alpha_sd;                     // standard deviation of the alpha gp kernal parameter
  int stationary;                    // is underlying gaussian process first or second order
  int fixed;                         //  should a gaussian process be used
