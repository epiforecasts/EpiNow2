# Internal function to create a parameter list

Internal function to create a parameter list

## Usage

``` r
make_param(name, dist = NULL, lower_bound = -Inf)
```

## Arguments

- name:

  Character, name of the parameter

- dist:

  `<dist_spec>`, the distribution of the parameter; default: NULL

- lower_bound:

  Numeric, lower bound for the parameter; default: -Inf

## Value

A list with the parameter details, classed as "EpiNow2.param"
