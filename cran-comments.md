## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

- Package archived on CRAN.

## Comments

All issues flagged by CRAN have been resolved. In particular the roxygen2 documentation issue that was flagged as the reason for archiving has been resolved by rebuilding the documentation. The change in policy to no longer allow a non-CRAN package as a suggest has also been resolved by removing this functionality.

Additional CRAN comments flagged post archiving (i.e temporary directory usage, no examples for internal functions, and no return values) have been updated as requested where action was required to comply with CRAN policies.

C++17 has been made a system requirement as requested by CRAN. Whilst rstan (on which we depend) currently only supports C++14 the package compiles as expected and passes all tests on C++17.
