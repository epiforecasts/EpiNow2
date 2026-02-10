# EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/epiforecasts/EpiNow2/branch/main/graph/badge.svg?token=FZWwEMdpq6)](https://app.codecov.io/gh/epiforecasts/EpiNow2)
[![](https://cranlogs.r-pkg.org/badges/grand-total/EpiNow2)](https://cran.r-project.org/package=EpiNow2)

[![MIT
license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/epiforecasts/EpiNow2/blob/main/LICENSE.md/)
[![GitHub
contributors](https://img.shields.io/github/contributors/epiforecasts/EpiNow2)](https://github.com/epiforecasts/EpiNow2/graphs/contributors)
[![universe](https://epiforecasts.r-universe.dev/badges/EpiNow2)](http://epiforecasts.r-universe.dev/#package:EpiNow2)
[![GitHub
commits](https://img.shields.io/github/commits-since/epiforecasts/EpiNow2/v1.7.1.svg?color=orange)](https://GitHub.com/epiforecasts/EpiNow2/commit/main/)
[![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211)

## Summary

[EpiNow2](https://epiforecasts.io/EpiNow2/) estimates the time-varying
reproduction number, growth rate, and doubling time using a range of
open-source tools ([Abbott et
al.](https://doi.org/10.12688/wellcomeopenres.16006.1)), and current
best practices ([Gostic et
al.](https://doi.org/10.1371/journal.pcbi.1008409)). It aims to help
users avoid some of the limitations of naive implementations in a
framework that is informed by community feedback and is actively
supported.

Forecasting is also supported for the time-varying reproduction number,
infections, and reported cases using the same generative process
approach as used for estimation.

More details

[EpiNow2](https://epiforecasts.io/EpiNow2/) estimates the time-varying
reproduction number on cases by date of infection (using a similar
approach to that implemented in
[`{EpiEstim}`](https://github.com/mrc-ide/EpiEstim)). True infections,
treated as latent and unobserved, are estimated and then mapped to
observed data (for example cases by date of report) via one or more
delay distributions (in the examples in the package documentation these
are an incubation period and a reporting delay) and a reporting model
that can include weekly periodicity.

Uncertainty is propagated from all inputs into the final parameter
estimates, helping to mitigate spurious findings. This is handled
internally. The time-varying reproduction estimates and the uncertain
generation time also give time-varying estimates of the rate of growth.

Models provided

[EpiNow2](https://epiforecasts.io/EpiNow2/) provides three models:

- [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md):
  Reconstruct cases by date of infection from reported cases.

- [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md):
  Estimate the relationship between primary and secondary observations,
  for example, deaths (secondary) based on hospital admissions
  (primary), or bed occupancy (secondary) based on hospital admissions
  (primary).

- [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md):
  Estimate a truncation distribution from multiple snapshots of the same
  data source over time. For more flexibility, check out the
  [`{epinowcast}`](https://package.epinowcast.org/) package.

The default model in
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
uses a non-stationary Gaussian process to estimate the time-varying
reproduction number and infer infections. Other options, which generally
reduce runtimes at the cost of the granularity of estimates or real-time
performance, include:

- A stationary Gaussian process (faster to estimate but currently gives
  reduced performance for real time estimates).
- User specified breakpoints.
- A fixed reproduction number.
- A piecewise constant, combining a fixed reproduction number with
  breakpoints.
- A random walk, combining a fixed reproduction number with regularly
  spaced breakpoints (i.e weekly).
- A deconvolution/back-calculation method for inferring infections,
  followed with calculating the time-varying reproduction number.
- Adjustment for the remaining susceptible population beyond the
  forecast horizon.

By default, all these models are fit with [MCMC
sampling](https://mc-stan.org/docs/reference-manual/mcmc.html) using the
[`rstan`](https://mc-stan.org/users/interfaces/rstan) R package as the
backend. Users can, however, switch to use approximate algorithms like
[variational
inference](https://en.wikipedia.org/wiki/Variational_Bayesian_methods),
the
[pathfinder](https://mc-stan.org/docs/reference-manual/pathfinder.html)
algorithm, or [Laplace
approximation](https://mc-stan.org/docs/reference-manual/laplace.html)
especially for quick prototyping. The latter two methods are provided
through the [`cmdstanr`](https://mc-stan.org/cmdstanr/) R package, so
users will have to install that separately.

The documentation for `estimate_infections` provides examples of the
implementation of the different options available.

[EpiNow2](https://epiforecasts.io/EpiNow2/) is designed to be used via a
single function call to two functions:

- [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md):
  Estimate Rt and cases by date of infection and forecast these
  infections into the future.

- [`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md):
  Efficiently run
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
  across multiple regions in an efficient manner.

These two functions call
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
which works to reconstruct cases by date of infection from reported
cases.

For more details on using each function corresponding function
documentation.

## Installation

Install the released version of the package:

``` r
install.packages("EpiNow2")
```

Install the development version of the package with:

``` r
install.packages("EpiNow2", repos = c("https://epiforecasts.r-universe.dev", getOption("repos")))
```

Alternatively, install the development version of the package with
[pak](https://pak.r-lib.org/) as follows (few users should need to do
this):

``` r
# check whether {pak} is installed
if (!require("pak")) {
  install.packages("pak")
}
pak::pkg_install("epiforecasts/EpiNow2")
```

If using `pak` fails, try:

``` r
# check whether {remotes} is installed
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("epiforecasts/EpiNow2")
```

To build [EpiNow2](https://epiforecasts.io/EpiNow2/) from source, users
will need to configure their C toolchain. This is because
[EpiNow2](https://epiforecasts.io/EpiNow2/) implements the underlying
models in Stan (a statistical modelling programming language), which is
built on C++.

Each operating system has a different set up procedure. Windows users
need to install an appropriate version of
[RTools](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows).
Mac users can [follow these
steps](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Mac),
and Linux users can use [this
guide](https://github.com/stan-dev/rstan/wiki/Configuring-C-Toolchain-for-Linux).

## Resources

Getting Started

The Getting Started vignette (see
[`vignette("EpiNow2")`](https://epiforecasts.io/EpiNow2/dev/articles/EpiNow2.md))
is your quickest entry point to the package. It provides a quick run
through of the two main functions in the package and how to set up them
up. It also discusses how to summarise and visualise the results after
running the models.

More broadly, users can also learn the details of estimating delay
distributions, nowcasting, and forecasting in a structured way through
the free and open short-course, [“Nowcasting and forecasting infectious
disease dynamics”](https://nfidd.github.io/nfidd/), developed by some
authors of this package.

Package website

The package has two websites: one for [the stable release version on
CRAN](https://epiforecasts.io/EpiNow2/), and another for [the version in
development](https://epiforecasts.io/EpiNow2/dev/). These two provide
various resources for learning about the package, including the function
reference, details about each model (model definition), workflows for
each model (usage), and case studies or literature of applications of
the package. However, the development website may contain experimental
features and information not yet available in the stable release.

End-to-end workflows

The workflow vignette (see
[`vignette("estimate_infections_workflow")`](https://epiforecasts.io/EpiNow2/dev/articles/estimate_infections_workflow.md))
provides guidance on the end-to-end process of estimating reproduction
numbers and performing short-term forecasts for a disease spreading in a

Model definitions

In different vignettes we provide the mathematical definition of each
model. For example, the model definition vignette for
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
can be found in
[`vignette("estimate_infections")`](https://epiforecasts.io/EpiNow2/dev/articles/estimate_infections.md).

Example implementations

A simple example of using the package to estimate a national Rt for
Covid-19 can be found
[here](https://gist.github.com/seabbs/163d0f195892cde685c70473e1f5e867).

## Contributing

We welcome all contributions. If you have identified an issue with the
package, you can file an issue
[here](https://github.com/epiforecasts/EpiNow2/issues). We also welcome
additions and extensions to the underlying model either in the form of
options or improvements. If you wish to contribute in any form, please
follow the [package contributing
guide](https://github.com/epiforecasts/EpiNow2/blob/main/.github/CONTRIBUTING.md).

## Contributors

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [allcontributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

[seabbs](https://github.com/epiforecasts/EpiNow2/commits?author=seabbs),
[sbfnk](https://github.com/epiforecasts/EpiNow2/commits?author=sbfnk),
[jamesmbaazam](https://github.com/epiforecasts/EpiNow2/commits?author=jamesmbaazam),
[joeHickson](https://github.com/epiforecasts/EpiNow2/commits?author=joeHickson),
[hsbadr](https://github.com/epiforecasts/EpiNow2/commits?author=hsbadr),
[pitmonticone](https://github.com/epiforecasts/EpiNow2/commits?author=pitmonticone),
[actions-user](https://github.com/epiforecasts/EpiNow2/commits?author=actions-user),
[ellisp](https://github.com/epiforecasts/EpiNow2/commits?author=ellisp),
[kaitejohnson](https://github.com/epiforecasts/EpiNow2/commits?author=kaitejohnson),
[jdmunday](https://github.com/epiforecasts/EpiNow2/commits?author=jdmunday),
[pearsonca](https://github.com/epiforecasts/EpiNow2/commits?author=pearsonca),
[Bisaloo](https://github.com/epiforecasts/EpiNow2/commits?author=Bisaloo),
[JAllen42](https://github.com/epiforecasts/EpiNow2/commits?author=JAllen42),
[adamkucharski](https://github.com/epiforecasts/EpiNow2/commits?author=adamkucharski),
[adrian-lison](https://github.com/epiforecasts/EpiNow2/commits?author=adrian-lison),
[avehtari](https://github.com/epiforecasts/EpiNow2/commits?author=avehtari),
[andrjohns](https://github.com/epiforecasts/EpiNow2/commits?author=andrjohns),
[claude](https://github.com/epiforecasts/EpiNow2/commits?author=claude),
[jcken95](https://github.com/epiforecasts/EpiNow2/commits?author=jcken95),
[LloydChapman](https://github.com/epiforecasts/EpiNow2/commits?author=LloydChapman),
[medewitt](https://github.com/epiforecasts/EpiNow2/commits?author=medewitt),
[nikosbosse](https://github.com/epiforecasts/EpiNow2/commits?author=nikosbosse),
[sophiemeakin](https://github.com/epiforecasts/EpiNow2/commits?author=sophiemeakin),
[zsusswein](https://github.com/epiforecasts/EpiNow2/commits?author=zsusswein)

### Issue Authors

[raulfernandezn](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Araulfernandezn),
[pcarbo](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Apcarbo),
[johnaponte](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ajohnaponte),
[sophie-schiller](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Asophie-schiller),
[munozedg](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Amunozedg),
[kathsherratt](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Akathsherratt),
[yungwai](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ayungwai),
[kgostic](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Akgostic),
[fkrauer](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Afkrauer),
[philturk](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aphilturk),
[krageth](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Akrageth),
[tony352](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Atony352),
[username-rp](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ausername-rp),
[HAKGH](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AHAKGH),
[AndrewRiceMGW](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AAndrewRiceMGW),
[brynhayder](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Abrynhayder),
[RichardMN](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3ARichardMN),
[andrybicio](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aandrybicio),
[rhamoonga](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Arhamoonga),
[furqan915](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Afurqan915),
[MFZaini1984](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AMFZaini1984),
[fabsig](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Afabsig),
[affans](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aaffans),
[GauriSaran](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AGauriSaran),
[davidvilanova](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Adavidvilanova),
[jrcpulliam](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ajrcpulliam),
[dajmcdon](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Adajmcdon),
[joshwlambert](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ajoshwlambert),
[avallecam](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aavallecam),
[athowes](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aathowes),
[lorenzwalthert](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Alorenzwalthert),
[nlinton](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Anlinton),
[martinamcm](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Amartinamcm),
[jonathonmellor](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ajonathonmellor),
[TimTaylor](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3ATimTaylor),
[ciaramccarthy1](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aciaramccarthy1),
[SamuelBrand1](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3ASamuelBrand1),
[damonbayer](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Adamonbayer),
[valentinedwv](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Avalentinedwv),
[coderabbitai](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Acoderabbitai),
[HenrikBengtsson](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AHenrikBengtsson),
[bob-carpenter](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Abob-carpenter)

### Issue Contributors

[jhellewell14](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Ajhellewell14),
[thlytras](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Athlytras),
[LizaHadley](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3ALizaHadley),
[ntorresd](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Antorresd),
[micahwiesner67](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Amicahwiesner67),
[paigemiller](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Apaigemiller),
[WardBrian](https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3AWardBrian)
