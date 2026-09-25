# Helpers for tests that read regional results back from disk.

#' Load the example regional_epinow output shipped with the package
#'
#' @return The output of `regional_epinow()` saved in `inst/extdata`
example_regional_output <- function() {
  readRDS(system.file(
    package = "EpiNow2", "extdata", "example_regional_epinow.rds"
  ))
}

#' Write regional results to disk as epinow() does with a target folder
#'
#' Uses the same saving functions as `epinow()` so the files match the
#' layout `<dir>/<region>/<date>/<file>.rds` that the reading functions
#' expect.
#'
#' @param regional A named list of `epinow` objects.
#' @param dir Directory to write the results into.
#' @param date Name of the dated folder to write into.
#' @return `dir`, invisibly
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
