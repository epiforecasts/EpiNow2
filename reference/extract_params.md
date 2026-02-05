# Extract parameter names

**\[experimental\]** Internal function for extracting given parameter
names of a distribution from the environment. Called by `new_dist_spec`

## Usage

``` r
extract_params(params, distribution)
```

## Arguments

- params:

  Given parameters (obtained using `as.list(environment())`)

- distribution:

  Character; the distribution to use.

## Value

A character vector of parameters and their values.
