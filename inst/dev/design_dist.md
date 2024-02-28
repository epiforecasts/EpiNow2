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

We are an proposing an dinterface similar to what is used in `{rstanarm}` and `{distributions3}`, where probability distributions are called according to their capitalised names. This avoids masking `base::gamma()` (although we still mask `stats::Gamma`). There is potential for integration especially with `{distributionS3}`. Parameters are nested according to the same syntax.

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
