# Convolve and scale a time series

This applies a lognormal convolution with given, potentially
time-varying parameters representing the parameters of the lognormal
distribution used for the convolution and an optional scaling factor.
This is akin to the model used in
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
and
[`simulate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/simulate_secondary.md).

## Usage

``` r
convolve_and_scale(
  data,
  type = c("incidence", "prevalence"),
  family = c("none", "poisson", "negbin"),
  delay_max = 30,
  ...
)
```

## Arguments

- data:

  A `<data.frame>` containing the `date` of report and `primary` cases
  as a numeric vector.

- type:

  A character string indicating the type of observation the secondary
  reports are. Options include:

  - "incidence": Assumes that secondary reports equal a convolution of
    previously observed primary reported cases. An example application
    is deaths from an infectious disease predicted by reported cases of
    that disease (or estimated infections).

  - "prevalence": Assumes that secondary reports are cumulative and are
    defined by currently observed primary reports minus a convolution of
    secondary reports. An example application is hospital bed usage
    predicted by hospital admissions.

- family:

  Character string defining the observation model. Options are Negative
  binomial ("negbin"), the default, Poisson ("poisson"), and "none"
  meaning the expectation is returned.

- delay_max:

  Integer, defaulting to 30 days. The maximum delay used in the
  convolution model.

- ...:

  Additional parameters to pass to the observation model (i.e `rnbinom`
  or `rpois`).

## Value

A `<data.frame>` containing simulated data in the format required by
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md).

## Details

Up to version 1.4.0 this function was called
[`simulate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/simulate_secondary.md).

## See also

[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)

## Examples

``` r
# load data.table for manipulation
library(data.table)

#### Incidence data example ####

# make some example secondary incidence data
cases <- example_confirmed
cases <- as.data.table(cases)[, primary := confirm]

# Assume that only 40 percent of cases are reported
cases[, scaling := 0.4]
#>            date confirm primary scaling
#>          <Date>   <num>   <num>   <num>
#>   1: 2020-02-22      14      14     0.4
#>   2: 2020-02-23      62      62     0.4
#>   3: 2020-02-24      53      53     0.4
#>   4: 2020-02-25      97      97     0.4
#>   5: 2020-02-26      93      93     0.4
#>  ---                                   
#> 126: 2020-06-26     296     296     0.4
#> 127: 2020-06-27     255     255     0.4
#> 128: 2020-06-28     175     175     0.4
#> 129: 2020-06-29     174     174     0.4
#> 130: 2020-06-30     126     126     0.4

# Parameters of the assumed log normal delay distribution
cases[, meanlog := 1.8][, sdlog := 0.5]
#>            date confirm primary scaling meanlog sdlog
#>          <Date>   <num>   <num>   <num>   <num> <num>
#>   1: 2020-02-22      14      14     0.4     1.8   0.5
#>   2: 2020-02-23      62      62     0.4     1.8   0.5
#>   3: 2020-02-24      53      53     0.4     1.8   0.5
#>   4: 2020-02-25      97      97     0.4     1.8   0.5
#>   5: 2020-02-26      93      93     0.4     1.8   0.5
#>  ---                                                 
#> 126: 2020-06-26     296     296     0.4     1.8   0.5
#> 127: 2020-06-27     255     255     0.4     1.8   0.5
#> 128: 2020-06-28     175     175     0.4     1.8   0.5
#> 129: 2020-06-29     174     174     0.4     1.8   0.5
#> 130: 2020-06-30     126     126     0.4     1.8   0.5

# Simulate secondary cases
cases <- convolve_and_scale(cases, type = "incidence")
cases
#>            date confirm primary scaling meanlog sdlog index scaled       conv
#>          <Date>   <num>   <num>   <num>   <num> <num> <int>  <num>      <num>
#>   1: 2020-02-22      14      14     0.4     1.8   0.5     1    5.6   5.600000
#>   2: 2020-02-23      62      62     0.4     1.8   0.5     2   24.8   5.827560
#>   3: 2020-02-24      53      53     0.4     1.8   0.5     3   21.2   8.801043
#>   4: 2020-02-25      97      97     0.4     1.8   0.5     4   38.8  12.938343
#>   5: 2020-02-26      93      93     0.4     1.8   0.5     5   37.2  16.590082
#>  ---                                                                         
#> 126: 2020-06-26     296     296     0.4     1.8   0.5   126  118.4  91.432820
#> 127: 2020-06-27     255     255     0.4     1.8   0.5   127  102.0  95.786381
#> 128: 2020-06-28     175     175     0.4     1.8   0.5   128   70.0 103.811968
#> 129: 2020-06-29     174     174     0.4     1.8   0.5   129   69.6 109.093575
#> 130: 2020-06-30     126     126     0.4     1.8   0.5   130   50.4 109.029626
#>      secondary
#>          <int>
#>   1:         5
#>   2:         5
#>   3:         8
#>   4:        12
#>   5:        16
#>  ---          
#> 126:        91
#> 127:        95
#> 128:       103
#> 129:       109
#> 130:       109
#### Prevalence data example ####

# make some example prevalence data
cases <- example_confirmed
cases <- as.data.table(cases)[, primary := confirm]

# Assume that only 30 percent of cases are reported
cases[, scaling := 0.3]
#>            date confirm primary scaling
#>          <Date>   <num>   <num>   <num>
#>   1: 2020-02-22      14      14     0.3
#>   2: 2020-02-23      62      62     0.3
#>   3: 2020-02-24      53      53     0.3
#>   4: 2020-02-25      97      97     0.3
#>   5: 2020-02-26      93      93     0.3
#>  ---                                   
#> 126: 2020-06-26     296     296     0.3
#> 127: 2020-06-27     255     255     0.3
#> 128: 2020-06-28     175     175     0.3
#> 129: 2020-06-29     174     174     0.3
#> 130: 2020-06-30     126     126     0.3

# Parameters of the assumed log normal delay distribution
cases[, meanlog := 1.6][, sdlog := 0.8]
#>            date confirm primary scaling meanlog sdlog
#>          <Date>   <num>   <num>   <num>   <num> <num>
#>   1: 2020-02-22      14      14     0.3     1.6   0.8
#>   2: 2020-02-23      62      62     0.3     1.6   0.8
#>   3: 2020-02-24      53      53     0.3     1.6   0.8
#>   4: 2020-02-25      97      97     0.3     1.6   0.8
#>   5: 2020-02-26      93      93     0.3     1.6   0.8
#>  ---                                                 
#> 126: 2020-06-26     296     296     0.3     1.6   0.8
#> 127: 2020-06-27     255     255     0.3     1.6   0.8
#> 128: 2020-06-28     175     175     0.3     1.6   0.8
#> 129: 2020-06-29     174     174     0.3     1.6   0.8
#> 130: 2020-06-30     126     126     0.3     1.6   0.8

# Simulate secondary cases
cases <- convolve_and_scale(cases, type = "prevalence")
cases
#>            date confirm primary scaling meanlog sdlog index scaled      conv
#>          <Date>   <num>   <num>   <num>   <num> <num> <int>  <num>     <num>
#>   1: 2020-02-22      14      14     0.3     1.6   0.8     1    4.2  4.200000
#>   2: 2020-02-23      62      62     0.3     1.6   0.8     2   18.6  6.749663
#>   3: 2020-02-24      53      53     0.3     1.6   0.8     3   15.9 10.939618
#>   4: 2020-02-25      97      97     0.3     1.6   0.8     4   29.1 13.765588
#>   5: 2020-02-26      93      93     0.3     1.6   0.8     5   27.9 17.347381
#>  ---                                                                        
#> 126: 2020-06-26     296     296     0.3     1.6   0.8   126   88.8 77.941892
#> 127: 2020-06-27     255     255     0.3     1.6   0.8   127   76.5 82.996181
#> 128: 2020-06-28     175     175     0.3     1.6   0.8   128   52.5 83.459485
#> 129: 2020-06-29     174     174     0.3     1.6   0.8   129   52.2 80.163424
#> 130: 2020-06-30     126     126     0.3     1.6   0.8   130   37.8 75.330670
#>      secondary
#>          <int>
#>   1:         4
#>   2:        18
#>   3:        23
#>   4:        38
#>   5:        49
#>  ---          
#> 126:       273
#> 127:       266
#> 128:       235
#> 129:       207
#> 130:       170
```
