# Create delay variables for stan

Create delay variables for stan

## Usage

``` r
create_stan_delays(..., time_points = 1L)
```

## Arguments

- ...:

  Named delay distributions. The names are assigned to IDs

- time_points:

  Integer, the number of time points in the data; determines weight
  associated with weighted delay priors; default: 1

## Value

A list of variables as expected by the stan model
