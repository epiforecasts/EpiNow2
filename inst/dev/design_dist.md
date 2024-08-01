# Design considerations of the distribution interface

We are aiming for an interface with the following properties
  * it should be clear to users which probability distribution is being used
  * it can represent discrete and continuous parameters
  * distribution parameters are labelled according to their meaning (e.g. mean vs. meanlog)

We need to be able to represent
  * (especially left-)truncated and untruncated distributions
  * nested distributions (parameters of one distribution can be distributed according to another distribution)

Ideally we'd also be able to represent
  * parametric and nonparametric distributions
  * distributions that are convolutions of two specified distributions

This interface could be of potential use to other packages (e.g. `epinowcast`, `epidist`, `epiparameter`). It should therefore stand on its own. Any `EpiNow2` specific functionality (especially relating to conversion to stan code) should not interfere with core functionality of the distribution interface.

## Proposed interface

We are an proposing an interface similar to what is used in `{rstanarm}` and `{distributions3}`, where probability distributions are called according to their capitalised names. This avoids masking `base::gamma()` (although we still mask `stats::Gamma`). There is potential for integration especially with `{distributionS3}`. Parameters are nested according to the same syntax.

Examples:
```{r}
Gamma(mean = 3, sd = 1)
Gamma(mean = 3, sd = 1, max = 14)
Gamma(mean = Normal(mean = 3, sd = 1), sd = 4, max = 14)
Gamma(shape = Normal(mean = 3, sd = 1), rate = Normal(mean = 2, sd = 0.5), max = 14)
Gamma(shape = 2, rate = 1, max = 14)
LogNormal(meanlog = 1.5, sdlog = 1)
LogNormal(mean = Normal(1.5, 0.1), sd = Normal(1, 0.1))
NonParametric(c(0.2, 0.4, 0.4))
NonParametric(c(0.2, 0.4, 0.2, 0.05))
```

## Implementation details

The objects representing probability distributions are a list with two elements, `parameters` (a list of parameters, which can be probability distributions themselves), and `distribution` (the type of distribution). It has class membership `dist_spec`.

```{r, eval = FALSE}
str(LogNormal(3, 1))
#> List of 2
#>  $ parameters  :List of 2
#>  ..$ meanlog: num 3
#>  ..$ sdlog  : num 1
#> $ distribution: chr "lognormal"
#> - attr(*, "class")= chr [1:2] "dist_spec" "list"
```

The objects can have additional attributes, especially `tolerance` (1 - the proportion of the cumulative distribution to be represented) and `max` (an absolute maximum).

### Representing convolutions

Convolutions are represented as a list of `dist_spec`s, themselves a `dist_spec` so that the attributes of `max` and `cutoff` can be represented at either the individual or convolved `dist_spec` level.

```{r, eval = FALSE}
str(LogNormal(3, 1) + Gamma(2, 3))
#> List of 2
#>  $ :List of 2
#>   ..$ parameters  :List of 2
#>   .. ..$ meanlog: num 3
#>   .. ..$ sdlog  : num 1
#>   ..$ distribution: chr "lognormal"
#>   ..- attr(*, "class")= chr [1:2] "dist_spec" "list"
#>  $ :List of 2
#>   ..$ parameters  :List of 2
#>   .. ..$ shape: num 2
#>   .. ..$ rate : num 3
#>   ..$ distribution: chr "gamma"
#>   ..- attr(*, "class")= chr [1:2] "dist_spec" "list"
#>  - attr(*, "class")= chr [1:2] "multi_dist_spec" "dist_spec" "list"
```
