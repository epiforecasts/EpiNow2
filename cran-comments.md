# cran-comments

## Release type

Minor release of EpiNow2 (1.9.0). Summary in NEWS.md.

## R CMD check results

0 errors | 0 warnings | 2 notes (expected — see below)

Both notes are standard for Stan-based packages:

- `Suggests or Enhances not in mainstream repositories: cmdstanr`. We use
  cmdstanr as an optional backend; it is intentionally suggested and available
  via the r-multiverse mirror specified in DESCRIPTION's
  `Additional_repositories`.
- `Compilation used the following non-portable flag(s):
  '-mno-omit-leaf-frame-pointer'`. This flag is injected by rstan/StanHeaders
  into the generated Stan model C++ and is outside our control.

Our local `devtools::check()` additionally surfaced a `.git` hidden-file note
and an `unable to verify current time` note. Both are artefacts of the local
check environment (`.git` is excluded by R CMD build and is not present in the
submitted tarball; the time note reflects local clock skew). It also produced
one test failure in `tests/testthat/test-setup_future.R`, which has
`skip_on_cran()` at the top and so is not run on CRAN's check workers.

## Test environments

- Local: Ubuntu 25.10, R 4.5.1.
- GitHub Actions:
  - ubuntu-latest, R release / oldrel-1 / devel
  - macOS-latest, R release
  - windows-latest, R release
- (Pending) win-builder: R devel.

## Downstream dependencies

There are currently no reverse dependencies on CRAN.
