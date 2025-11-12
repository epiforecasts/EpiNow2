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
#'   assert_numeric
#' @importFrom purrr walk
#' @importFrom rlang arg_match
#' @return Called for its side effects.
#' @keywords internal
check_reports_valid <- function(data,
                                model = c(
                                  "estimate_infections",
                                  "estimate_secondary"
                                )) {
  # Check that the case time series (reports) is a data frame
  assert_data_frame(data)
  # Perform checks depending on the model to the data is meant to be used with
  model <- arg_match(model)

  assert_date(data$date, any.missing = FALSE)
  if (model == "estimate_secondary") {
    # Check that data has the right column names
    assert_names(
      names(data),
      must.include = c("date", "primary", "secondary")
    )
    assert_numeric(data$primary, lower = 0)
    assert_numeric(data$secondary, lower = 0)
  } else {
    # Check that data has the right column names
    assert_names(
      names(data),
      must.include = c("date", "confirm")
    )
    assert_numeric(data$confirm, lower = 0)
  }
  assert_logical(data$accumulate, null.ok = TRUE)
  return(invisible(data))
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
    seq_len(ndist(dist)), get_distribution,
    x = dist, FUN.VALUE = character(1)
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
  if (any(is.infinite(max(dist))) && attr(dist, "cdf_cutoff") == 0) {
    cli_abort(
      c(
        "i" = "All distributions passed to the model need to have a
      {col_blue(\"finite maximum\")}, which can be achieved either by
      setting {.var max} or, if using a distribution with fixed parameters,
      non-zero {.var cdf_cutoff}."
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
#' @inheritParams check_stan_delay
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
    deprecate_stop(
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


#' Check and warn if individual non-parametric delay PMFs are longer than the
#' input data
#'
#' @param stan_args List of stan arguments including the data element with
#'   delay information from [create_stan_delays()]
#' @param ... Named delay distributions (e.g., gt, delay, trunc) used to
#'   identify which delays are too long
#' @importFrom cli cli_warn
#'
#' @return Called for its side effects
#' @keywords internal
check_np_delay_lengths <- function(stan_args, ...) {
  delays <- list(...)

  # Check if there are any non-parametric delays
  if (is.null(stan_args$data$delay_n_np) || stan_args$data$delay_n_np == 0) {
    return(invisible())
  }

  # Get the data length
  data_length <- stan_args$data$t

  # Get the groups array
  np_pmf_groups <- stan_args$data$delay_np_pmf_groups

  # Calculate individual PMF lengths from the groups
  n_np <- length(np_pmf_groups) - 1
  pmf_lengths <- numeric(n_np)
  for (i in seq_len(n_np)) {
    pmf_lengths[i] <- np_pmf_groups[i + 1] - np_pmf_groups[i]
  }

  # Build name mapping: replicate delay names by number of distributions
  type_n <- vapply(delays, ndist, integer(1))
  delay_names <- rep(names(delays), type_n)

  # Flatten delays to match the order in stan data
  if (length(delays) > 1) {
    flat_delays <- do.call(c, delays)
  } else {
    flat_delays <- delays
  }

  # Find which delays are non-parametric
  parametric <- vapply(flat_delays, function(x) {
    get_distribution(x) != "nonparametric"
  }, logical(1))

  # Get parameter names for non-parametric delays
  np_delay_names <- delay_names[!parametric]

  # Check which PMFs exceed data length
  pmf_longer_than_data <- pmf_lengths > data_length

  if (any(pmf_longer_than_data)) {
    long_pmf_lengths <- pmf_lengths[pmf_longer_than_data]
    long_delay_names <- np_delay_names[pmf_longer_than_data]

    cli::cli_warn(
      c(
        "!" = "Non-parametric delay distributions are longer than the input
        data.",
        "{.var {long_delay_names}} {?has/have} length{?s}
        {.val {long_pmf_lengths}} but data has
        {.val {data_length}} rows.",
        "i" = "These will be trimmed to match the data length. To avoid this,
        ensure PMFs have the same length as the data."
      ),
      .frequency = "once",
      .frequency_id = "pmf_individual_longer_than_data"
    )
  }

  return(invisible())
}

