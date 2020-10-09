library(pkgnet)

## Declare paths explicitly as currently required by pkgnet
pkg_path <- system.file(package = "EpiNow2")
report_path <- file.path("inst/pkg-structure", "EpiNow2_report.html")

## Generate pkg report
report <- CreatePackageReport("EpiNow2",
                              report_path = report_path)


