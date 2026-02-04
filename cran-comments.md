# cran-comments

## Resubmission

This is a resubmission. In this version:

- Reduced example data size to speed up examples flagged for slow runtime:
  fewer MCMC samples, removed pre-generated plots, switched from xz to gzip
  compression for faster loading

## Test environments

- Local: macOS (aarch64-apple-darwin), R 4.4.2
- GitHub Actions: ubuntu-latest, R release
- GitHub Actions: ubuntu-latest, R devel
- win-builder: R devel (Windows)
- win-builder: R devel (Debian)

## Downstream dependencies

No reverse dependencies on CRAN.
