#' Validate data input
#'
#' @description `r lifecycle::badge("stable")`
#' `check_reports_valid()` checks that the supplied data is a `<data.frame>`,
#' and that it has the right column names and types. In particular, it checks
#' that the date column is in date format and does not contain NAs, and that
#' the other columns are numeric.
#'
#' @param data A data frame with either:
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
#' @keywords internal
check_reports_valid <- function(data,
                                model = c(
                                  "estimate_infections",
                                  "estimate_truncation",
                                  "estimate_secondary"
                                )) {
  # Check that the case time series (reports) is a data frame
  assert_data_frame(data)
  # Perform checks depending on the model to the data is meant to be used with
  model <- arg_match(model)

  if (model == "estimate_secondary") {
    # Check that data has the right column names
    assert_names(
      names(data),
      must.include = c("date", "primary", "secondary")
    )
    # Check that the data data.frame has the right column types
    assert_date(data$date, any.missing = FALSE)
    assert_numeric(data$primary, lower = 0)
    assert_numeric(data$secondary, lower = 0)
  } else {
    # Check that data has the right column names
    assert_names(
      names(data),
      must.include = c("date", "confirm")
    )
    # Check that the data data.frame has the right column types
    assert_date(data$date, any.missing = FALSE)
    assert_numeric(data$confirm, lower = 0)
  }
}

#' Validate probability distribution for passing to stan
#'
#' @description `r lifecycle::badge("stable")`
#' `check_stan_delay()` checks that the supplied data is a `<dist_spec>`,
#' that it is a supported distribution, and that is has a finite maximum.
#'
#' @param dist A `dist_spec` object.`
#' @importFrom checkmate assert_class
#' @importFrom rlang arg_match
#' @importFrom cli cli_abort col_blue
#' @return Called for its side effects.
#' @keywords internal
check_stan_delay <- function(dist) {
  # Check that `dist` is a `dist_spec`
  assert_class(dist, "dist_spec")
  # Check that `dist` is lognormal or gamma or nonparametric
  distributions <- vapply(
    seq_len(ndist(dist)), get_distribution, x = dist, FUN.VALUE = character(1)
  )
  if (
    !all(distributions %in% c("lognormal", "gamma", "fixed", "nonparametric"))
  ) {
    cli_abort(
      c(
       "!" = "Distributions passed to the model need to be
        {col_blue(\"lognormal\")}, {col_blue(\"gamma\")},
        {col_blue(\"fixed\")}, or {col_blue(\"nonparametric\")}."
      )
    )
  }
  # Check that `dist` has parameters that are either numeric or normal
  # distributions with numeric parameters and infinite maximum
  numeric_or_normal <- unlist(lapply(seq_len(ndist(dist)), function(id) {
    if (get_distribution(dist, id) != "nonparametric") {
      params <- get_parameters(dist, id)
      vapply(params, function(x) {
        is.numeric(x) ||
          (is(x, "dist_spec") && get_distribution(x) == "normal" &&
             is.infinite(max(x)))
      }, logical(1))
    }
  }))
  if (!all(numeric_or_normal)) {
    cli_abort(
      c(
        "!" = "Delay distributions passed to the model need to have parameters
        that are either {col_blue(\"numeric\")} or
        {col_blue(\"normally distributed\")} with {col_blue(\"numeric\")}
        parameters and {col_blue(\"infinite maximum\")}."
      )
    )
  }
  if (is.null(attr(dist, "cdf_cutoff"))) {
    attr(dist, "cdf_cutoff") <- 0
  }
  assert_numeric(attr(dist, "cdf_cutoff"), lower = 0, upper = 1)
  # Check that `dist` has a finite maximum
  if (any(is.infinite(max(dist))) && !(attr(dist, "cdf_cutoff") > 0)) {
    cli_abort(
      c(
        "i" = "All distribution passed to the model need to have a
      {col_blue(\"finite maximum\")}, which can be achieved either by
      setting {.var max} or non-zero {.var cdf_cutoff}."
      )
    )
  }
}

#' Validate probability distribution for using as generation time
#'
#' @description `r lifecycle::badge("stable")`
#' does all the checks in`check_stan_delay()` and additionally makes sure
#' that if `dist` is nonparametric,  its first element is zero.
#'
#' @importFrom lifecycle deprecate_warn
#' @inheritParams check_stan_delay dist
#' @return Called for its side effects.
#' @keywords internal
check_generation_time <- function(dist) {
  # Do the standard delay checks
  check_stan_delay(dist)
  ## check for nonparametric with nonzero first element
  nonzero_first_element <- vapply(seq_len(ndist(dist)), function(i) {
    get_distribution(dist, i) == "nonparametric" && get_pmf(dist, i)[1] > 0
  }, logical(1))
  if (all(nonzero_first_element)) {
    deprecate_warn(
      "1.6.0",
      I(
        "Specifying nonparametric generation times with nonzero first element"
      ),
      details = c(
        "Since zero generation times are not supported by the model, the
         generation time will be left-truncated at one. ",
        "In future versions this will cause an error. Please ensure that the
         first element of the nonparametric generation interval is zero."
      )
    )
  }
}

#' Check that PMF tail is not sparse
#'
#' @description Checks if the tail of a PMF vector has more than `span`
#' consecutive values smaller than `tol` and throws a warning if so.
#' @param pmf A probability mass function vector
#' @param span The number of consecutive indices in the tail to check
#' @param tol The value which to consider the tail as sparse
#' @importFrom cli cli_warn col_blue
#'
#' @return Called for its side effects.
#' @keywords internal
check_sparse_pmf_tail <- function(pmf, span = 5, tol = 1e-6) {
  if (all(tail(pmf, span) < tol)) {
    cli_warn(
      c(
        "!" = "The PMF tail has {col_blue(span)} consecutive value{?s} smaller
        than {col_blue(tol)}.",
        "i" = "This will increase run times with very small increases in
        accuracy. Consider using the `cdf_cutoff` argument when constructing
        the distribution object, or using the `bound_dist()` function."
      ),
      .frequency = "regularly",
      .frequency_id = "sparse_pmf_tail"
    )
  }
}

#' Check if data has either explicit NA values or implicit missing dates.
#'
#' @param data The data to be checked
#'
#' @return `TRUE` if data is complete, else if data has implicit or explicit
#' missingness, `FALSE`.
#' @keywords internal
test_data_complete <- function(data) {
  data <- setDT(data) # Convert data to data.table

  # Check for explicit missingness in required columns
  # (date, confirm, primary, secondary)
  columns_to_check <- c(
    "date",
    intersect(c("confirm", "primary", "secondary"), names(data))
  )
  if (any(sapply(data[, ..columns_to_check], anyNA))) {
    return(FALSE)
  }

  # Check for implicit missingness by comparing the expected full date sequence
  complete_dates <- seq(
    min(data$date, na.rm = TRUE),
    max(data$date, na.rm = TRUE),
    by = "1 day"
  )
  if (length(complete_dates) > length(unique(data$date))) {
    return(FALSE)
  }

  return(TRUE) # Return TRUE if no missing values or gaps in date sequence
}
