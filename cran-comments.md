# cran-comments

## Resubmission

This is a resubmission. In this version:

- Reduced example data size to speed up examples flagged for slow runtime:
  fewer MCMC samples, removed pre-generated plots, switched from xz to gzip
  compression for faster loading

## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE about overall checktime (16 min > 10 min) on Windows is due to the
package's extensive test suite and Stan model compilation. This is expected
for a package that interfaces with Stan.

## Test environments

- Local: macOS (aarch64-apple-darwin), R 4.4.2
- GitHub Actions: ubuntu-latest, R release
- GitHub Actions: ubuntu-latest, R devel
- win-builder: R devel (Windows)
- win-builder: R devel (Debian)

## Downstream dependencies

No reverse dependencies on CRAN.
