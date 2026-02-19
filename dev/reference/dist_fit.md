# Fit an Integer Adjusted Exponential, Gamma or Lognormal distributions

**\[stable\]** Fits an integer adjusted exponential, gamma or lognormal
distribution using stan.

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
#> Chain 1: Gradient evaluation took 4.2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.42 seconds.
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
#> Chain 1:  Elapsed Time: 0.141 seconds (Warm-up)
#> Chain 1:                0.065 seconds (Sampling)
#> Chain 1:                0.206 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 4.1e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.41 seconds.
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
#> Chain 2:  Elapsed Time: 0.141 seconds (Warm-up)
#> Chain 2:                0.08 seconds (Sampling)
#> Chain 2:                0.221 seconds (Total)
#> Chain 2: 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>             mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> lambda[1]   2.93    0.03 0.52   2.05   2.54   2.86   3.24   4.06   317 1.01
#> lp__      -11.47    0.04 0.70 -13.34 -11.65 -11.22 -11.03 -10.97   310 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Feb 19 18:55:54 2026.
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
#> Chain 1: Gradient evaluation took 0.000279 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.79 seconds.
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
#> Chain 1:  Elapsed Time: 2.388 seconds (Warm-up)
#> Chain 1:                0.984 seconds (Sampling)
#> Chain 1:                3.372 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.000352 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 3.52 seconds.
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
#> Chain 2:  Elapsed Time: 2.578 seconds (Warm-up)
#> Chain 2:                1.101 seconds (Sampling)
#> Chain 2:                3.679 seconds (Total)
#> Chain 2: 
#> WARN [2026-02-19 18:56:01] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-02-19 18:56:01] dist_fit (chain: 2): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-02-19 18:56:01] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2026-02-19 18:56:01] dist_fit (chain: 2): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>                mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> alpha_raw[1]   1.00    0.05 0.58   0.09   0.55   0.94   1.37   2.19   137 1.00
#> beta_raw[1]    0.99    0.04 0.56   0.11   0.58   0.90   1.36   2.25   184 1.00
#> alpha[1]       6.14    0.05 0.58   5.24   5.69   6.08   6.52   7.33   137 1.00
#> beta[1]        5.83    0.04 0.56   4.95   5.42   5.74   6.20   7.09   184 1.00
#> lp__         -17.21    0.12 1.41 -20.75 -17.86 -16.74 -16.15 -15.73   138 1.01
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Feb 19 18:56:01 2026.
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
#> Chain 1: Gradient evaluation took 5.7e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.57 seconds.
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
#> Chain 1:  Elapsed Time: 0.319 seconds (Warm-up)
#> Chain 1:                0.158 seconds (Sampling)
#> Chain 1:                0.477 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 5.2e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.52 seconds.
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
#> Chain 2:  Elapsed Time: 0.33 seconds (Warm-up)
#> Chain 2:                0.151 seconds (Sampling)
#> Chain 2:                0.481 seconds (Total)
#> Chain 2: 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>            mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> mu[1]      1.62    0.00 0.02   1.58   1.60   1.62   1.63   1.66   550    1
#> sigma[1]   0.17    0.00 0.02   0.13   0.16   0.17   0.18   0.21   498    1
#> lp__     -75.20    0.05 1.01 -77.74 -75.65 -74.85 -74.46 -74.19   459    1
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Feb 19 18:56:02 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
