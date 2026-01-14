# Internal function for generating a `dist_spec` given parameters and a distribution.

**\[experimental\]** This will convert all parameters to natural
parameters before generating a `dist_spec`. If they have uncertainty
this will be done using sampling.

## Usage

``` r
new_dist_spec(params, distribution, max = Inf, cdf_cutoff = 0)
```

## Arguments

- params:

  Parameters of the distribution (including `max`)

- distribution:

  Character; the distribution to use.

- max:

  Numeric, maximum value of the distribution. The distribution will be
  truncated at this value. Default: `Inf`, i.e. no maximum.

- cdf_cutoff:

  Numeric; the desired CDF cutoff. Any part of the cumulative
  distribution function beyond 1 minus the value of this argument is
  removed. Default: `0`, i.e. use the full distribution.

## Value

A `dist_spec` of the given specification.

## Examples

``` r
new_dist_spec(
  params = list(mean = 2, sd = 1),
  distribution = "normal"
)
#> - normal distribution:
#>   mean:
#>     2
#>   sd:
#>     1
```
