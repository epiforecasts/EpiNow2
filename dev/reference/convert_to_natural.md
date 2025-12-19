# Internal function for converting parameters to natural parameters.

**\[experimental\]** This is used for preprocessing before generating a
`dist_spec` object from a given set of parameters and distribution

## Usage

``` r
convert_to_natural(params, distribution)
```

## Arguments

- params:

  A numerical named parameter vector

- distribution:

  Character; the distribution to use.

## Value

A list with two elements, `params_mean` and `params_sd`, containing mean
and sd of natural parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
convert_to_natural(
  params = list(mean = 2, sd = 1),
  distribution = "gamma"
)
} # }
```
