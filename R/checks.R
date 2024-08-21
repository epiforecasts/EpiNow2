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
    stop(
      "Distributions passed to the model need to be lognormal, gamma, fixed ",
      "or nonparametric."
    )
  }
  # Check that `dist` has parameters that are either numeric or normal
  # distributions with numeric parameters and infinite maximum
  numeric_or_normal <- unlist(lapply(seq_len(ndist(dist)), function(id) {
    params <- get_parameters(dist, id)
    vapply(params, function(x) {
      is.numeric(x) ||
        (is(x, "dist_spec") && get_distribution(x) == "normal" &&
           is.infinite(max(x)))
    }, logical(1))
  }))
  if (!all(numeric_or_normal)) {
    stop(
      "Delay distributions passed to the model need to have parameters that ",
      "are either numeric or normally distributed with numeric parameters ",
      "and infinite maximum."
    )
  }
  if (is.null(attr(dist, "tolerance"))) {
    attr(dist, "tolerance") <- 0
  }
  assert_numeric(attr(dist, "tolerance"), lower = 0, upper = 1)
  # Check that `dist` has a finite maximum
  if (any(is.infinite(max(dist))) && !(attr(dist, "tolerance") > 0)) {
    stop(
      "All distribution passed to the model need to have a finite maximum,",
      "which can be achieved either by setting `max` or non-zero `tolerance`."
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
#'
#' @return Called for its side effects.
#' @keywords internal
check_sparse_pmf_tail <- function(pmf, span = 5, tol = 1e-6) {
  if (all(pmf[(length(pmf) - span + 1):length(pmf)] < tol)) {
    warning(
      sprintf(
        "The PMF tail has %s consecutive values smaller than %s.",
        span, tol
      ),
      " This will drastically increase run time with very small increases ",
      "in accuracy. Consider increasing the tail values of the PMF.",
      call. = FALSE
    )
  }
}
