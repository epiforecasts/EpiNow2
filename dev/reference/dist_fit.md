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
#> Chain 1: Gradient evaluation took 4.9e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.49 seconds.
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
#> Chain 1:  Elapsed Time: 0.129 seconds (Warm-up)
#> Chain 1:                0.073 seconds (Sampling)
#> Chain 1:                0.202 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 2.8e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.28 seconds.
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
#> Chain 2:                0.071 seconds (Sampling)
#> Chain 2:                0.191 seconds (Total)
#> Chain 2: 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>             mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> lambda[1]   2.41    0.02 0.37   1.78   2.16   2.38   2.66   3.17   344 1.01
#> lp__      -17.82    0.05 0.68 -19.64 -18.02 -17.53 -17.35 -17.31   189 1.02
#> 
#> Samples were drawn using NUTS(diag_e) at Fri Nov 27 12:43:41 2026.
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
#> Chain 1: Gradient evaluation took 0.000241 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.41 seconds.
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
#> Chain 1:  Elapsed Time: 1.801 seconds (Warm-up)
#> Chain 1:                0.927 seconds (Sampling)
#> Chain 1:                2.728 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.00022 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 2.2 seconds.
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
#> Chain 2:  Elapsed Time: 1.71 seconds (Warm-up)
#> Chain 2:                0.863 seconds (Sampling)
#> Chain 2:                2.573 seconds (Total)
#> Chain 2: 
#> WARN [2026-11-27 12:43:46] dist_fit (chain: 1, 2): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-11-27 12:43:46] dist_fit (chain: 1, 2): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>                mean se_mean   sd   2.5%    25%    50%    75% 97.5% n_eff Rhat
#> alpha_raw[1]   0.82    0.05 0.57   0.05   0.35   0.72   1.17  2.16   128 1.02
#> beta_raw[1]    0.84    0.04 0.52   0.06   0.41   0.78   1.18  1.95   217 1.00
#> alpha[1]       7.46    0.05 0.57   6.69   6.99   7.36   7.81  8.80   128 1.02
#> beta[1]        7.32    0.04 0.52   6.53   6.89   7.26   7.66  8.43   217 1.00
#> lp__         -11.46    0.12 1.37 -14.85 -12.12 -11.17 -10.41 -9.95   142 1.01
#> 
#> Samples were drawn using NUTS(diag_e) at Fri Nov 27 12:43:46 2026.
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
#> Chain 1: Gradient evaluation took 5.5e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.55 seconds.
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
#> Chain 1:  Elapsed Time: 0.253 seconds (Warm-up)
#> Chain 1:                0.128 seconds (Sampling)
#> Chain 1:                0.381 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: Rejecting initial value:
#> Chain 2:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 2:   Stan can't start sampling from this initial value.
#> Chain 2: 
#> Chain 2: Gradient evaluation took 4.4e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.44 seconds.
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
#> Chain 2:  Elapsed Time: 0.235 seconds (Warm-up)
#> Chain 2:                0.114 seconds (Sampling)
#> Chain 2:                0.349 seconds (Total)
#> Chain 2: 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>            mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> mu[1]      1.63    0.00 0.02   1.59   1.62   1.63   1.65   1.67   664 1.00
#> sigma[1]   0.18    0.00 0.02   0.15   0.17   0.18   0.19   0.22   518 1.01
#> lp__     -79.95    0.05 1.06 -82.92 -80.32 -79.60 -79.22 -78.97   407 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Fri Nov 27 12:43:47 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
