# Distribution functions re-exported from distspec

**\[deprecated\]**

## Usage

``` r
Gamma(...)

LogNormal(...)

Normal(...)

Fixed(...)

Exp(...)

Weibull(...)

Dirichlet(...)

NonParametric(...)

discretise(...)

discretize(...)

get_pmf(...)

convert_to_logmean(...)

convert_to_logsd(...)

fix_parameters(...)

get_distribution(...)

get_parameters(...)

is_constrained(...)

bound_dist(...)

collapse(...)

new_dist_spec(...)
```

## Arguments

- ...:

  Passed to the distspec function of the same name.

## Details

The probability distribution interface has moved to the distspec
package. These functions are re-exported so that the `EpiNow2::` form
keeps resolving, and will be removed in a future release; use distspec
directly (the bare names are attached when EpiNow2 is loaded).
