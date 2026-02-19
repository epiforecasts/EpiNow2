# Construct the Required Future Rt assumption

**\[stable\]** Converts the `future` argument from
[`rt_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/rt_opts.md)
into arguments that can be passed to stan.

## Usage

``` r
create_future_rt(future = c("latest", "project", "estimate"), delay = 0)
```

## Arguments

- future:

  A character string or integer. This argument indicates how to set
  future Rt values. Supported options are to project using the Rt model
  ("project"), to use the latest estimate based on partial data
  ("latest"), to use the latest estimate based on data that is over 50%
  complete ("estimate"). If an integer is supplied then the Rt estimate
  from this many days into the future (or past if negative) past will be
  used forwards in time.

- delay:

  Numeric mean delay

## Value

A list containing a logical called fixed and an integer called from
