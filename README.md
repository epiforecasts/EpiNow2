
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
commits](https://img.shields.io/github/commits-since/epiforecasts/EpiNow2/v1.6.0.svg?color=orange)](https://GitHub.com/epiforecasts/EpiNow2/commit/main/)
[![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211)

## Summary

`{EpiNow2}` estimates the time-varying reproduction number, growth rate,
and doubling time using a range of open-source tools ([Abbott et
al.](https://doi.org/10.12688/wellcomeopenres.16006.1)), and current
best practices ([Gostic et
al.](https://doi.org/10.1371/journal.pcbi.1008409)). It aims to help
users avoid some of the limitations of naive implementations in a
framework that is informed by community feedback and is actively
supported.

Forecasting is also supported for the time-varying reproduction number,
infections, and reported cases using the same generative process
approach as used for estimation.

<details>
<summary>
More details
</summary>

`{EpiNow2}` estimates the time-varying reproduction number on cases by
date of infection (using a similar approach to that implemented in
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

</details>
<details>
<summary>
Models provided
</summary>

`{EpiNow2}` provides three models:

- `estimate_infections()`: Reconstruct cases by date of infection from
  reported cases.

- `estimate_secondary()`: Estimate the relationship between primary and
  secondary observations, for example, deaths (secondary) based on
  hospital admissions (primary), or bed occupancy (secondary) based on
  hospital admissions (primary).

- `estimate_truncation()`: Estimate a truncation distribution from
  multiple snapshots of the same data source over time. For more
  flexibility, check out the
  [`{epinowcast}`](https://package.epinowcast.org/) package.

The default model in `estimate_infections()` uses a non-stationary
Gaussian process to estimate the time-varying reproduction number and
infer infections. Other options, which generally reduce runtimes at the
cost of the granularity of estimates or real-time performance, include:

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

`{EpiNow2}` is designed to be used via a single function call to two
functions:

- `epinow()`: Estimate Rt and cases by date of infection and forecast
  these infections into the future.

- `regional_epinow()`: Efficiently run `epinow()` across multiple
  regions in an efficient manner.

These two functions call `estimate_infections()`, which works to
reconstruct cases by date of infection from reported cases.

For more details on using each function corresponding function
documentation.

</details>

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

To build `{EpiNow2}` from source, users will need to configure their C
toolchain. This is because `{EpiNow2}` implements the underlying models
in Stan (a statistical modelling programming language), which is built
on C++.

Each operating system has a different set up procedure. Windows users
need to install an appropriate version of
[RTools](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows).
Mac users can [follow these
steps](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Mac),
and Linux users can use [this
guide](https://github.com/stan-dev/rstan/wiki/Configuring-C-Toolchain-for-Linux).

## Resources

<details>
<summary>
Getting Started
</summary>

The Getting Started vignette (see `vignette("EpiNow2")`) is your
quickest entry point to the package. It provides a quick run through of
the two main functions in the package and how to set up them up. It also
discusses how to summarise and visualise the results after running the
models.

More broadly, users can also learn the details of estimating delay
distributions, nowcasting, and forecasting in a structured way through
the free and open short-course, [“Nowcasting and forecasting infectious
disease dynamics”](https://nfidd.github.io/nfidd/), developed by some
authors of this package.

</details>
<details>
<summary>
Package website
</summary>

The package has two websites: one for [the stable release version on
CRAN](https://epiforecasts.io/EpiNow2/), and another for [the version in
development](https://epiforecasts.io/EpiNow2/dev/). These two provide
various resources for learning about the package, including the function
reference, details about each model (model definition), workflows for
each model (usage), and case studies or literature of applications of
the package. However, the development website may contain experimental
features and information not yet available in the stable release.

</details>
<details>
<summary>
End-to-end workflows
</summary>

The workflow vignette (see `vignette("estimate_infections_workflow")`)
provides guidance on the end-to-end process of estimating reproduction
numbers and performing short-term forecasts for a disease spreading in a

</details>
<details>
<summary>
Model definitions
</summary>

In different vignettes we provide the mathematical definition of each
model. For example, the model definition vignette for
`estimate_infections()` can be found in
`vignette("estimate_infections")`.

</details>
<details>
<summary>
Example implementations
</summary>

A simple example of using the package to estimate a national Rt for
Covid-19 can be found
[here](https://gist.github.com/seabbs/163d0f195892cde685c70473e1f5e867).

</details>

## Contributing

We welcome all contributions. If you have identified an issue with the
package, you can file an issue
[here](https://github.com/epiforecasts/EpiNow2/issues). We also welcome
additions and extensions to the underlying model either in the form of
options or improvements. If you wish to contribute in any form, please
follow the [package contributing
guide](https://github.com/epiforecasts/EpiNow2/blob/main/.github/CONTRIBUTING.md).

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [allcontributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

<a href="https://github.com/epiforecasts/EpiNow2/commits?author=seabbs">seabbs</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=sbfnk">sbfnk</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=jamesmbaazam">jamesmbaazam</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=joeHickson">joeHickson</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=hsbadr">hsbadr</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=pitmonticone">pitmonticone</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=actions-user">actions-user</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=ellisp">ellisp</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=jdmunday">jdmunday</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=kaitejohnson">kaitejohnson</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=pearsonca">pearsonca</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=JAllen42">JAllen42</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=adamkucharski">adamkucharski</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=andrjohns">andrjohns</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=Bisaloo">Bisaloo</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=LloydChapman">LloydChapman</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=medewitt">medewitt</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=nikosbosse">nikosbosse</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=sophiemeakin">sophiemeakin</a>,
<a href="https://github.com/epiforecasts/EpiNow2/commits?author=zsusswein">zsusswein</a>

### Issue Authors

<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Araulfernandezn">raulfernandezn</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Apcarbo">pcarbo</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ajohnaponte">johnaponte</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Asophie-schiller">sophie-schiller</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Amunozedg">munozedg</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Akathsherratt">kathsherratt</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ayungwai">yungwai</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Akgostic">kgostic</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Afkrauer">fkrauer</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aphilturk">philturk</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Akrageth">krageth</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Atony352">tony352</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ausername-rp">username-rp</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AHAKGH">HAKGH</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AAndrewRiceMGW">AndrewRiceMGW</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Abrynhayder">brynhayder</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3ARichardMN">RichardMN</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aandrybicio">andrybicio</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Arhamoonga">rhamoonga</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Afurqan915">furqan915</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AMFZaini1984">MFZaini1984</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Afabsig">fabsig</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aaffans">affans</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3AGauriSaran">GauriSaran</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Adavidvilanova">davidvilanova</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ajrcpulliam">jrcpulliam</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Adajmcdon">dajmcdon</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Ajoshwlambert">joshwlambert</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aavallecam">avallecam</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aathowes">athowes</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Alorenzwalthert">lorenzwalthert</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Anlinton">nlinton</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Amartinamcm">martinamcm</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Aadrian-lison">adrian-lison</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+author%3Amicahwiesner67">micahwiesner67</a>

### Issue Contributors

<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Ajhellewell14">jhellewell14</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Athlytras">thlytras</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3ALizaHadley">LizaHadley</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3Antorresd">ntorresd</a>,
<a href="https://github.com/epiforecasts/EpiNow2/issues?q=is%3Aissue+commenter%3ASamuelBrand1">SamuelBrand1</a>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
