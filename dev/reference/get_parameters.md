# Get parameters from distributions or fitted models

**\[experimental\]** Generic function to extract parameters. For
`dist_spec` objects, extracts the distribution parameters (e.g., shape
and rate for Gamma). For fitted model objects, extracts estimated
parameters and delays as `dist_spec` objects that can be used as priors.

## Usage

``` r
get_parameters(x, ...)

# S3 method for class 'dist_spec'
get_parameters(x, id = NULL, ...)

# S3 method for class 'epinowfit'
get_parameters(x, ...)
```

## Arguments

- x:

  A `dist_spec` object or fitted model object

- ...:

  Additional arguments passed to methods

- id:

  Numeric index of the distribution to extract (for multi- component
  `dist_spec` objects). If `NULL` (default), extracts from the first
  component.

## Value

For `dist_spec`: a list of distribution parameters. For fitted models: a
named list of `dist_spec` objects.

## Examples

``` r
# For dist_spec objects
dist <- Gamma(shape = 3, rate = 2)
get_parameters(dist)
#> $shape
#> [1] 3
#> 
#> $rate
#> [1] 2
#> 

if (FALSE) { # \dontrun{
# For fitted models - extract estimated distributions
dists <- get_parameters(fit)
names(dists)
} # }
```
