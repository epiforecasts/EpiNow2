
library(pkgnet)

## Declare paths explicitly as currently required by pkgnet
pkg_path <- system.file(package = "EpiNow")
report_path <- file.path("inst/pkg-structure", "EpiNow_report.html")

## Generate pkg report
report <- CreatePackageReport("EpiNow",
                              report_path = report_path)


