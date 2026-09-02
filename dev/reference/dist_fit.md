# Fit an Integer Adjusted Exponential, Gamma or Lognormal distributions

Fits an integer adjusted exponential, gamma or lognormal distribution
using stan.

## Usage

``` r
dist_fit(
  values = NULL,
  samples = 1000,
  cores = 1,
  chains = 2,
  dist = "exp",
  verbose = FALSE,
  backend = "rstan"
)
```

## Arguments

- values:

  Numeric vector of values

- samples:

  Numeric, number of samples to take. Must be \>= 1000. Defaults to
  1000.

- cores:

  Numeric, defaults to 1. Number of CPU cores to use (no effect if
  greater than the number of chains).

- chains:

  Numeric, defaults to 2. Number of MCMC chains to use. More is better
  with the minimum being two.

- dist:

  Character string, which distribution to fit. Defaults to exponential
  (`"exp"`) but gamma (`"gamma"`) and lognormal (`"lognormal"`) are also
  supported.

- verbose:

  Logical, defaults to FALSE. Should verbose progress messages be
  printed.

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

## Value

A stan fit of an interval censored distribution

## Examples

``` r
# \donttest{
# integer adjusted exponential model
dist_fit(rexp(1:100, 2),
  samples = 1000, dist = "exp",
  cores = ifelse(interactive(), 4, 1), verbose = TRUE
)
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 4.6e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.46 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 1: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 1: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 1: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 1: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 1: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 1: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 1: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 1: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 1: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 1: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 1: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 1: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 1: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 1: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.121 seconds (Warm-up)
#> Chain 1:                0.072 seconds (Sampling)
#> Chain 1:                0.193 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: Rejecting initial value:
#> Chain 2:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 2:   Stan can't start sampling from this initial value.
#> Chain 2: 
#> Chain 2: Gradient evaluation took 3.3e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.33 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 2: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 2: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 2: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 2: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 2: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 2: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 2: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 2: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 2: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 2: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 2: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 2: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 2: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 2: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.12 seconds (Warm-up)
#> Chain 2:                0.057 seconds (Sampling)
#> Chain 2:                0.177 seconds (Total)
#> Chain 2: 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>            mean se_mean   sd   2.5%   25%   50%   75% 97.5% n_eff Rhat
#> lambda[1]  3.22    0.03 0.58   2.30  2.82  3.14  3.54  4.68   367    1
#> lp__      -8.92    0.03 0.60 -10.77 -9.04 -8.68 -8.54 -8.49   318    1
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Sep  2 16:14:04 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).


# integer adjusted gamma model
dist_fit(rgamma(1:100, 5, 5),
  samples = 1000, dist = "gamma",
  cores = ifelse(interactive(), 4, 1), verbose = TRUE
)
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.000339 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 3.39 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 1: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 1: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 1: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 1: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 1: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 1: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 1: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 1: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 1: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 1: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 1: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 1: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 1: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 1: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 2.367 seconds (Warm-up)
#> Chain 1:                1.337 seconds (Sampling)
#> Chain 1:                3.704 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.000235 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 2.35 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 2: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 2: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 2: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 2: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 2: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 2: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 2: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 2: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 2: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 2: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 2: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 2: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 2: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 2: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 2.449 seconds (Warm-up)
#> Chain 2:                1.047 seconds (Sampling)
#> Chain 2:                3.496 seconds (Total)
#> Chain 2: 
#> WARN [2026-09-02 16:14:11] dist_fit (chain: 1, 2): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>                mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> alpha_raw[1]   0.93    0.04 0.57   0.08   0.48   0.86   1.29   2.16   210    1
#> beta_raw[1]    0.98    0.04 0.56   0.08   0.54   0.91   1.32   2.22   236    1
#> alpha[1]       6.69    0.04 0.57   5.83   6.24   6.61   7.04   7.92   210    1
#> beta[1]        6.65    0.04 0.56   5.76   6.22   6.59   7.00   7.90   236    1
#> lp__         -12.85    0.11 1.38 -16.55 -13.30 -12.46 -11.88 -11.47   153    1
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Sep  2 16:14:11 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).

# integer adjusted lognormal model
dist_fit(rlnorm(1:100, log(5), 0.2),
  samples = 1000, dist = "lognormal",
  cores = ifelse(interactive(), 4, 1), verbose = TRUE
)
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 6.7e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.67 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 1: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 1: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 1: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 1: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 1: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 1: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 1: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 1: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 1: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 1: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 1: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 1: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 1: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 1: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.29 seconds (Warm-up)
#> Chain 1:                0.153 seconds (Sampling)
#> Chain 1:                0.443 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 4.8e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.48 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 2: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 2: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 2: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 2: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 2: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 2: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 2: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 2: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 2: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 2: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 2: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 2: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 2: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 2: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.285 seconds (Warm-up)
#> Chain 2:                0.142 seconds (Sampling)
#> Chain 2:                0.427 seconds (Total)
#> Chain 2: 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>            mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> mu[1]      1.61    0.00 0.02   1.58   1.60   1.61   1.63   1.65   556    1
#> sigma[1]   0.14    0.00 0.02   0.11   0.13   0.14   0.15   0.18   587    1
#> lp__     -67.83    0.06 0.99 -70.19 -68.19 -67.54 -67.13 -66.85   266    1
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Sep  2 16:14:12 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
