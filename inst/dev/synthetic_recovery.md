# Synthetic Recovery visual results

This file combines the figures generated in `rt.R` into a single file, used to 
confirm that the fits to synthetic data with different EpiNow2
specifications work as expected. 

In all figures, the black dots indicate the ground truth R(t) estimate, used to 
generate the synthetic data used for model fitting. 

### Estimate from the default Gaussian Process settings using the No-U-Turn (NUTS) sampler
![](./figs/rt_gp_nuts.png)
![](./figs/inf_gp_nuts.png)

### Estimate from back-calculation
![](./figs/rt_backcalc_nuts.png)
![](./figs/inf_backcalc_nuts.png)

### Estimate from a weekly random walk, with no Gaussian process
![](./figs/rt_weekly_rw_nuts.png)
![](./figs/inf_weekly_rw_nuts.png)

### Estimate from a monthly random walk + a stationary Gaussian process
![](./figs/rt_gp_rw_nuts.png)
![](./figs/inf_gp_rw_nuts.png)


### Comparison of different model specifications
CRPS over time evaluated against known R(t)
![](./figs/rt_crps.png)
CRPS over time evaluated against infections
![](./figs/inf_crps.png)

