# Check and warn if truncation distribution is longer than observed time

Checks if the truncation distribution PMF is longer than the observed
time period (excluding seeding time and forecast horizon). The
truncation is applied to the observed time period in the Stan model, so
having a truncation distribution longer than this period means the tail
of the distribution will be used.

## Usage

``` r
check_truncation_length(stan_args, time_points)
```

## Arguments

- stan_args:

  List of stan arguments including the data element with delay
  information from
  [`create_stan_delays()`](https://epiforecasts.io/EpiNow2/reference/create_stan_delays.md)

- time_points:

  Integer length of the observed time period (t - seeding_time -
  horizon)

## Value

Called for its side effects
