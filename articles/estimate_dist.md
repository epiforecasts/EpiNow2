# Model definition: estimate_dist()

## Overview

[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
fits a parametric delay distribution from interval-censored linelist
data via MCMC. The model accounts for double interval censoring of the
primary and secondary events, right truncation due to a finite
observation period, and optional left truncation. The likelihood is
provided by the
[primarycensored](https://primarycensored.epinowcast.org/)
package^(\[1\]); see its documentation for derivations and a fuller
account of why these adjustments are needed^(\[2,3\]). The conceptual
motivation in *EpiNow2* is covered in
[`vignette("delays")`](https://epiforecasts.io/EpiNow2/articles/delays.md),
and a worked example in
[`vignette("estimate_dist_workflow")`](https://epiforecasts.io/EpiNow2/articles/estimate_dist_workflow.md).
For analyses needing time-varying delays, partial pooling across strata,
or regression on covariates,
[`epidist`](https://epidist.epinowcast.org/) builds on the same
likelihood and is the recommended next step.

## Data and notation

Each observation $`i = 1, \ldots, N`$ is a record of a primary event
(e.g. infection) and a secondary event (e.g. symptom onset). Both events
are observed only as intervals. Let $`P_i = [p^{-}_i, p^{+}_i)`$ be the
primary event window and $`S_i = [s^{-}_i, s^{+}_i)`$ be the secondary
event window. We refer to $`w^{P}_i = p^{+}_i - p^{-}_i`$ as the primary
window width and to $`w^{S}_i = s^{+}_i - s^{-}_i`$ as the secondary
window width. At daily resolution both are typically equal to one.

The observed delay interval is
$`[d^{-}_i, d^{+}_i) = [s^{-}_i - p^{-}_i,\, s^{+}_i - p^{-}_i)`$, the
difference between the secondary window and the start of the primary
window. Each observation also has a relative observation time $`D_i`$
and an optional left truncation point $`L_i`$, both measured from
$`p^{-}_i`$. $`D_i`$ is the time at which the data were extracted
relative to the primary event start and acts as the right truncation
point. Per-observation weights $`n_i`$ allow identical
$`(d^{-}_i, d^{+}_i, w^{P}_i, D_i)`$ combinations to be aggregated; this
affects only computational efficiency, not the likelihood.

The continuous, unobserved delay $`T_i`$ between the true primary event
time $`T^{P}_i`$ and the true secondary event time $`T^{S}_i`$ is
assumed to follow a parametric distribution with cumulative distribution
function $`F(\cdot \,|\, \theta)`$, where $`\theta`$ is the parameter
vector. We denote the primary event distribution within its window by
$`f_P`$. The model treats $`\theta`$ as unknown and $`f_P`$ as known.

## Likelihood

### Continuous formulation

If $`T^{P}_i`$ has density $`f_P`$ on $`[p^{-}_i, p^{+}_i)`$, the
distribution of the delay $`T_i`$ from the primary event to a fixed
reference point (e.g. $`p^{-}_i`$) is the convolution of $`f_P`$ with
the underlying delay distribution. The corresponding cumulative
distribution function adjusted for primary event censoring is

``` math
\begin{equation}
  F^{*}(t \,|\, \theta, w^{P}_i)
  = \int_{p^{-}_i}^{p^{+}_i} F(t - u \,|\, \theta) f_P(u) \, du.
\end{equation}
```

Without primary censoring the distribution of the discrete observed
delay would simply be obtained by differencing $`F`$ at the secondary
window boundaries. With primary censoring, $`F`$ is replaced by
$`F^{*}`$ throughout. For uniform $`f_P`$ this can be evaluated
analytically for the lognormal, gamma, and Weibull delay distributions;
for other combinations the integral is evaluated numerically. The
vendored Stan code dispatches to either branch through
`check_for_analytical()` in `primarycensored.stan`.

### Truncation

The model handles right truncation at $`D_i`$ and optional left
truncation at $`L_i`$ by renormalising $`F^{*}`$:

``` math
\begin{equation}
  F^{*}_{[L_i, D_i]}(t \,|\, \theta, w^{P}_i)
  = \frac{F^{*}(t \,|\, \theta, w^{P}_i) - F^{*}(L_i \,|\, \theta, w^{P}_i)}
         {F^{*}(D_i \,|\, \theta, w^{P}_i) - F^{*}(L_i \,|\, \theta, w^{P}_i)}
\end{equation}
```

for $`L_i \leq t \leq D_i`$ and zero outside the interval. $`D_i`$ may
be set to $`\infty`$ to indicate no right truncation; in that case the
denominator reduces to $`1 - F^{*}(L_i \,|\, \theta, w^{P}_i)`$. $`L_i`$
defaults to $`0`$ and is not currently exposed at the R interface.

### Discrete observation likelihood

The probability mass for observation $`i`$ is the difference of the
truncated, primary-censored CDF at the secondary window boundaries:

``` math
\begin{equation}
  \Pr(d^{-}_i \leq T_i < d^{+}_i \,|\, \theta, w^{P}_i, L_i, D_i)
  = F^{*}_{[L_i, D_i]}(d^{+}_i \,|\, \theta, w^{P}_i)
  - F^{*}_{[L_i, D_i]}(d^{-}_i \,|\, \theta, w^{P}_i).
\end{equation}
```

The aggregated log-likelihood used in `estimate_dist.stan` is

``` math
\begin{equation}
  \log \mathcal{L}(\theta) = \sum_{i=1}^{N} n_i \log
    \Pr(d^{-}_i \leq T_i < d^{+}_i \,|\, \theta, w^{P}_i, L_i, D_i),
\end{equation}
```

implemented as the call to `primarycensored_lpmf()` inside the model
block.

### Untruncated approximation

When $`D_i`$ is much larger than the largest observed delay, the right
truncation factor is numerically indistinguishable from one and the
renormalisation in the equation above can be skipped.
[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
applies this approximation by setting $`D_i`$ to $`\infty`$ whenever
$`D_i > c\, \max_j d^{+}_j`$, where $`c`$ is the `obs_time_threshold`
argument and defaults to $`2`$ following the same heuristic used by
[`epidist`](https://epidist.epinowcast.org/). Setting
`obs_time_threshold = Inf` disables the approximation.

## Primary event distribution

Two primary event distributions are supported:

- **Uniform** (default): $`f_P`$ is uniform on $`[p^{-}_i, p^{+}_i)`$.
  This is appropriate for daily reporting where the within-day timing of
  the primary event is unknown and assumed equally likely at any point
  in the window.
- **Exponential growth**: $`f_P`$ is proportional to $`\exp(r u)`$ on
  $`[p^{-}_i, p^{+}_i)`$, with growth rate $`r`$ supplied by the user
  via `primary_params`. This adjusts for the bias introduced when the
  primary event rate is changing rapidly within the window, for example
  in the early phase of an outbreak^(\[2\]). The growth rate is treated
  as fixed data and is not estimated.

The primary event parameters are passed to Stan in the `primary_id` and
`primary_params` data fields and are not part of $`\theta`$.

## Delay families and parameterisations

The model supports five delay families, selected by the `dist` argument.
Parameter names match the natural parameterisation used elsewhere in
*EpiNow2*.

| `dist`        | Parameters         | Density support      |
|---------------|--------------------|----------------------|
| `"lognormal"` | `meanlog`, `sdlog` | $`T > 0`$            |
| `"gamma"`     | `shape`, `rate`    | $`T > 0`$            |
| `"normal"`    | `mean`, `sd`       | $`T \in \mathbb{R}`$ |
| `"exp"`       | `rate`             | $`T > 0`$            |
| `"weibull"`   | `shape`, `scale`   | $`T > 0`$            |

For `"lognormal"`, `"gamma"`, and `"weibull"` with a uniform primary,
the primary-censored CDF $`F^{*}`$ has a closed form and is computed
analytically. The other combinations use numerical integration over the
primary window.

### Priors

Priors are passed via the `priors` argument as a named list of
`dist_spec` objects, with names matching the parameter names of the
chosen family. Each prior is converted into a
[`Normal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
density on the parameter and applied independently. Lower bounds are
imposed for parameters that must be positive (e.g. `sdlog`, `rate`,
`shape`, `scale`); the truncation is handled by `params_lp()` in
`inst/stan/functions/params.stan`.

The default priors are family-specific:

``` math
\begin{align}
  \text{lognormal:}\quad \text{meanlog} &\sim \mathrm{Normal}(1, 1), &
  \text{sdlog} &\sim \mathrm{Normal}(0.5, 0.5), \\
  \text{gamma:}\quad \text{shape} &\sim \mathrm{Normal}(2, 2), &
  \text{rate} &\sim \mathrm{Normal}(0.5, 0.5), \\
  \text{normal:}\quad \text{mean} &\sim \mathrm{Normal}(5, 5), &
  \text{sd} &\sim \mathrm{Normal}(1, 1), \\
  \text{exponential:}\quad \text{rate} &\sim \mathrm{Normal}(0.5, 0.5), \\
  \text{Weibull:}\quad \text{shape} &\sim \mathrm{Normal}(2, 2), &
  \text{scale} &\sim \mathrm{Normal}(5, 5).
\end{align}
```

These defaults are deliberately wide and are intended as starting points
rather than fits to any particular delay. Users are encouraged to set
priors that reflect domain knowledge about the delay being estimated;
see
[`vignette("estimate_dist_workflow")`](https://epiforecasts.io/EpiNow2/articles/estimate_dist_workflow.md)
for guidance on translating prior beliefs into the lognormal
parameterisation.

## References

1\.

Abbott, S., Brand, S., Azam, J. M., Pearson, C., Funk, S., & Charniga,
K. (2026). *Primarycensored: Primary event censored distributions*.
<https://doi.org/10.5281/zenodo.13632839>

2\.

Park, S. W., Akhmetzhanov, A. R., Charniga, K., Cori, A., Davies, N. G.,
Dushoff, J., Funk, S., Gostic, K., Grenfell, B., Linton, N. M.,
Lipsitch, M., Lison, A., Overton, C. E., Ward, T., & Abbott, S. (2024).
Estimating epidemiological delay distributions for infectious diseases.
*medRxiv*. <https://doi.org/10.1101/2024.01.12.24301247>

3\.

Charniga, K., Park, S. W., Akhmetzhanov, A. R., Cori, A., Dushoff, J.,
Funk, S., Gostic, K. M., Linton, N. M., Lison, A., Overton, C. E.,
Pulliam, J. R. C., Ward, T., Cauchemez, S., & Abbott, S. (2024). Best
practices for estimating and reporting epidemiological delay
distributions of infectious diseases. *PLoS Comput. Biol.*, *20*(10),
e1012520. <https://doi.org/10.1371/journal.pcbi.1012520>
