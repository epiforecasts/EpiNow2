  real L;				                     // boundary value for infections gp
  int<lower=1> M;			               // basis functions for infections gp
  int gp_type;                       // type of gp, 0 = squared exponential, 1 = periodic, 2 = Matern
  real nu;                           // smoothness parameter for Matern kernel (used if gp_type = 2)
  real w0;                           // fundamental frequency for periodic kernel (used if gp_type = 1)
  int stationary;                    // is underlying gaussian process first or second order
  int fixed;                         //  should a gaussian process be used
