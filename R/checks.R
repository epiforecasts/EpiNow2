#' Validate data input
#'
#' @description
#' `check_reports_valid()` checks that the supplied data is a `<data.frame>`,
#' and that it has the right column names and types. In particular, it checks
#' that the date column is in date format and does not contain NA's, and that
#' the other columns are numeric.
#'
#' @param reports A data frame with either:
#' * a minimum of two columns: `date` and `confirm`, if to be
#'  used by [estimate_infections()] or [estimate_truncation()], or
#' * a minimum of three columns: `date`, `primary`, and `secondary`, if to be
#' used by [estimate_secondary()].
#' @param model The EpiNow2 model to be used. Either
#' "estimate_infections", "estimate_truncation", or "estimate_secondary".
#' This is used to determine which checks to perform on the data input.
#' @importFrom checkmate assert_data_frame assert_date assert_names
#' assert_numeric
#' @importFrom rlang arg_match
#' @return Called for its side effects.
#' @author James M. Azam
#' @keywords internal
check_reports_valid <- function(reports, model) {
  # Check that the case time series (reports) is a data frame
  assert_data_frame(reports)
  # Perform checks depending on the model to the data is meant to be used with
  model <- arg_match(
  model,
  values = c(
    "estimate_infections",
    "estimate_truncation",
    "estimate_secondary"
    )
  )

  if (model == "estimate_secondary") {
    # Check that reports has the right column names
    assert_names(
      names(reports),
      must.include = c("date", "primary", "secondary")
    )
    # Check that the reports data.frame has the right column types
    assert_date(reports$date, any.missing = FALSE)
    assert_numeric(reports$primary, lower = 0)
    assert_numeric(reports$secondary, lower = 0)
  } else {
    # Check that reports has the right column names
    assert_names(
      names(reports),
      must.include = c("date", "confirm")
    )
    # Check that the reports data.frame has the right column types
    assert_date(reports$date, any.missing = FALSE)
    assert_numeric(reports$confirm, lower = 0)
  }
}

#' Validate probability distribution for passing to stan
#'
#' @description
#' `check_stan_dist()` checks that the supplied data is a `<dist_spec>`,
#' and that it can be further processed by stan.
#'
#' @param dist A `dist_spec` object.`
#' @importFrom checkmate assert_class
#' @importFrom rlang arg_match
#' @return Called for its side effects.
#' @author Sebastian Funk
#' @keywords internal
check_stan_dist <- function(dist) {
  # Check that `dist` is a `dist_spec`
  assert_class(dist, "dist_spec")
  # Check that `dist` is lognormal or gamma or nonparametric
  if (!all(dist$dist %in% c("lognormal", "gamma"))) {
    stop(
      "Distributions passed to the model need to be lognormal, gamma or ",
      "nonparametric (i.e., have no uncertainty)."
    )
  }
  if (any(is.infinite(max(dist)))) {
    stop("All distribution passed to the model need to have a finite maximum")
  }
}
