# Secondary Reports Options

**\[stable\]** Returns a list of options defining the secondary model
used in
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md).
This model is a combination of a convolution of previously observed
primary reports combined with current primary reports (either additive
or subtractive). It can optionally be cumulative. See the documentation
of `type` for sensible options to cover most use cases and the returned
values of `secondary_opts()` for all currently supported options.

## Usage

``` r
secondary_opts(type = c("incidence", "prevalence"), ...)
```

## Arguments

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

- ...:

  Overwrite options defined by type. See the returned values for all
  options that can be passed.

## Value

A `<secondary_opts>` object of binary options summarising secondary
model used in
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md).
Options returned are `cumulative` (should the secondary report be
cumulative), `historic` (should a convolution of primary reported cases
be used to predict secondary reported cases), `primary_hist_additive`
(should the historic convolution of primary reported cases be additive
or subtractive), `current` (should currently observed primary reported
cases contribute to current secondary reported cases),
`primary_current_additive` (should current primary reported cases be
additive or subtractive).

## See also

[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)

## Examples

``` r
# incidence model
secondary_opts("incidence")
#> $cumulative
#> [1] 0
#> 
#> $historic
#> [1] 1
#> 
#> $primary_hist_additive
#> [1] 1
#> 
#> $current
#> [1] 0
#> 
#> $primary_current_additive
#> [1] 0
#> 
#> attr(,"class")
#> [1] "secondary_opts" "list"          

# prevalence model
secondary_opts("prevalence")
#> $cumulative
#> [1] 1
#> 
#> $historic
#> [1] 1
#> 
#> $primary_hist_additive
#> [1] 0
#> 
#> $current
#> [1] 1
#> 
#> $primary_current_additive
#> [1] 1
#> 
#> attr(,"class")
#> [1] "secondary_opts" "list"          
```
