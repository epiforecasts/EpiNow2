# Helpers for tests that read regional results back from disk.

example_regional_output <- function() {
  readRDS(system.file(
    package = "EpiNow2", "extdata", "example_regional_epinow.rds"
  ))
}

# Writes `<dir>/<region>/<date>/<file>.rds` using the saving functions
# that `epinow()` uses.
write_regional_results <- function(regional, dir, date = "latest") {
  for (region in names(regional)) {
    folder <- file.path(dir, region, date)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    fit <- regional[[region]]
    save_estimate_infections(fit, folder, return_fit = FALSE)
    estimates_by_report_date(fit, target_folder = folder)
    summary(fit, return_numeric = TRUE, target_folder = folder)
    saveRDS(fit$timing, file.path(folder, "runtime.rds"))
  }
  invisible(dir)
}
