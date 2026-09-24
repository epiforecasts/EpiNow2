[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

EpiNow2 is an R package for estimating the time-varying reproduction number, growth rate, and doubling time from right truncated data.

The package uses Stan for the core modelling and R for pre-processing and post-processing of inputs and outputs.

This website covers the documentation of the Stan code/functions (See the next tab).

## Documentation structure

The documentation is organized by function groups, with related functions grouped together:

Each function page includes:

- A brief description of the function's purpose
- Detailed explanation of the function's behavior
- Parameter descriptions
- Return value information
- Call and caller graphs
- Collaboration graphs
- Referencing and referenced functions

## Functions implemented in C++

Some functions are declared in the Stan code without a body and implemented in C++ in `inst/include`, with a hand-written reverse-mode gradient.
At present this is `convolve_with_rev_pmf()` (see the convolution functions group and `inst/include/epinow2/convolve_with_rev_pmf.hpp` for the maths).
Any model that includes these functions must be compiled with `inst/include/epinow2.hpp` included before the model code, and with undefined functions allowed in `stanc`.
The installed package does this for its `rstan` models, `epinow2_cmdstan_model()` passes the header to `cmdstanr` as `user_header`, and `epinow2_stan_header()` returns its path for other uses.

## EpiNow2 main website

Click [here](https://epiforecasts.github.io/EpiNow2/) to return to the main EpiNow2 website.
