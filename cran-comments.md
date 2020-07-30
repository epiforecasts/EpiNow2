## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 8 notes

* This is a resubmission.
* GNU make is a SystemRequirement
* Suggests or Enhances not in mainstream repositories: EpiSoon (available using Additional_repositories from https://epiforecasts.io/drat/)
* Moved all feasible examples to donttest from dontrun
* Added references to the description of the package
* Checked packages are not installed in either the code, examples or vignettes.
* Wrapped examples that required suggested packages with requireNamespace
* Checked that no code automatically writes to the filesytem. Example of using the internal saving now saves to the temporary directory.

