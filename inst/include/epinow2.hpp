#ifndef EPINOW2_HPP
#define EPINOW2_HPP

// C++ implementations of Stan functions that the EpiNow2 models declare
// without a body. Include this before the Stan model code: rstan builds
// do so via tools/stan_include.R (run from configure), and cmdstanr builds
// pass it as `user_header` in epinow2_cmdstan_model().
#include "epinow2/convolve_with_rev_pmf.hpp"

#endif
