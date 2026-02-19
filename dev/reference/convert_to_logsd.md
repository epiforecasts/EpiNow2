# Convert mean and sd to log standard deviation for a log normal distribution

**\[stable\]** Convert from mean and standard deviation to the log
standard deviation of the lognormal distribution. Useful for defining
distributions supported by
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
[`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md),
and
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md).

## Usage

``` r
convert_to_logsd(mean, sd)
```

## Arguments

- mean:

  Numeric, mean of a distribution

- sd:

  Numeric, standard deviation of a distribution

## Value

The log standard deviation of a lognormal distribution

## Examples

``` r
convert_to_logsd(2, 1)
#> [1] 0.4723807
```
