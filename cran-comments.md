## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

- Package archived on CRAN.

## Comments

After making C++17 a system requirement in the last release we have updated our use of the rstantools package to manually flag the use of C++14 as this began to cause problems with compilation on Fedora (and we assume soon other platforms). We have tested installation using rhub an win dev builder.
