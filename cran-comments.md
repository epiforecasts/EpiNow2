## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 5 notes

* This is a resubmission.
* GNU make is a SystemRequirement
* Suggests or Enhances not in mainstream repositories: EpiSoon (available using Additional_repositories from https://epiforecasts.io/drat/)
* Dropped usage of \dontrun
* All data included in the package are needed for downstream users and require documentation, which seems impossible to achieve if the files are in inst/extdata. Similarly, the example_confirmed file provides a documented example dataset of what input is supported. We’re very happy to change things but are unsure of what further action to take to resolve this issue without additional guidance.