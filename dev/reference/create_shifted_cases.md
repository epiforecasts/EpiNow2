# Create Delay Shifted Cases

**\[stable\]**

This functions creates a data frame of reported cases that has been
smoothed using a centred partial rolling average (with a period set by
`smoothing_window`) and shifted back in time by some delay. It is used
by
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
to generate the mean shifted prior on which the back calculation method
(see
[`backcalc_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/backcalc_opts.md))
is based.

## Usage

``` r
create_shifted_cases(data, shift, smoothing_window, horizon)
```

## Arguments

- data:

  A `<data.frame>` of disease reports (confirm) by date (date).
  `confirm` must be numeric and `date` must be in date format.
  Optionally, `data` can also have a logical `accumulate` column which
  indicates whether data should be added to the next data point. This is
  useful when modelling e.g. weekly incidence data. See also the
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/dev/reference/fill_missing.md)
  function which helps add the `accumulate` column with the desired
  properties when dealing with non-daily data. If any accumulation is
  done this happens after truncation as specified by the `truncation`
  argument. If all entries of `confirm` are missing (`NA`) the returned
  estimates will represent the prior distributions.

- shift:

  Numeric, mean delay shift to apply.

- smoothing_window:

  Numeric, the rolling average smoothing window to apply. Must be odd in
  order to be defined as a centred average.

## Value

A `<data.frame>` for shifted reported cases

## Details

The function first shifts all the data back in time by `shift` days
(thus discarding the first `shift` days of data) and then applies a
centred rolling mean of length `smoothing_window` to the shifted data
except for the final period. The final period (the forecast horizon plus
half the smoothing window) is instead replaced by a log-linear model fit
(with 1 added to the data for fitting to avoid zeroes and later
subtracted again), projected to the end of the forecast horizon. The
initial part of the data (corresponding to the length of the smoothing
window) is then removed, and any non-integer resulting values rounded
up.

## Examples

``` r
if (FALSE) { # \dontrun{
shift <- 7
horizon <- 7
smoothing_window <- 14
## add NAs for horizon
cases <- add_horizon(example_confirmed[1:30], horizon)
## add zeroes initially
cases <- data.table::rbindlist(list(
  data.table::data.table(
    date = seq(
      min(cases$date) - 10,
      min(cases$date) - 1,
      by = "days"
    ),
    confirm = 0, breakpoint = 0
  ),
  cases
))
create_shifted_cases(cases, shift, smoothing_window, horizon)
} # }
```
