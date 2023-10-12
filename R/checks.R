#' Validate data input
#'
#' @description
#' `check_reports_valid()` checks that the input data frame is a data frame,
#' and that it has the right column names and types. In particular, it checks
#' that the date column is a date, and that the other columns are integers. It
#' checks that the date column has no NA values.
#'
#' @param reports A data frame with either:
#' * two columns: date and confirm, if to be
#'  used by estimate_infection() or estimate_truncation, or
#' * three columns: date, primary, and secondary, if to be used by
#'  estimate_secondary().
#' @param for_estimate_secondary Logical; whether the data is being passed by
#' estimate_secondary() or the other estimate_*() functions.
#' @return Called for its side effects.
#' @author James M. Azam
#' @keywords internal
check_reports_valid <- function(reports, for_estimate_secondary) {
  # Check that the case time series (reports) is a data frame
  checkmate::assert_data_frame(reports)
  # Perform checks depending on whether the data is for estimate_secondary()
  # or not
  if (for_estimate_secondary) {
    # Check that reports has the right column names
    checkmate::assert_names(
      names(reports),
      must.include = c("date", "primary", "secondary")
    )
    # Check that the reports data.frame has the right column types
    checkmate::assert_date(reports$date, any.missing = FALSE)
    checkmate::assert_integerish(reports$primary)
    checkmate::assert_integerish(reports$secondary)
  } else {
    # Check that reports has the right column names
    checkmate::assert_names(
      names(reports),
      must.include = c("date", "confirm")
    )
    # Check that the reports data.frame has the right column types
    checkmate::assert_date(reports$date, any.missing = FALSE)
    checkmate::assert_integerish(reports$confirm)
  }
}
