# Synthetic Recovery visual results

This file combines the figures generated in `rt.R` into a single file, used to 
confirm that the fits to synthetic data with different EpiNow2
specifications work as expected. 

In all figures, the black dots indicate the ground truth R(t) estimate, used to 
generate the synthetic data used for model fitting. 

### R(t) estimate from the default Gaussian Process settings using the No-U-Turn (NUTS) sampler
![](./figs/rt_gp_nuts.png)

### R(t) estimate from back-calculation
![](./figs/rt_backcalc_nuts.png)

### R(t) estimate from a weekly random walk, with no Gaussian process
![](./figs/rt_weekly_rw_nuts.png)

### R(t) estimate from a monthly random walk + a stationary Gaussian process
![](./figs/rt_gp_rw_nuts.png)