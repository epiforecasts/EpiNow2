#' Adjust from Case Counts by Infection Date to Date of Report
#'
#' @description  `r lifecycle::badge("stable")`
#' Maps from cases by date of infection to date of report via date of
#' onset.
#' @param infections `<data.table>` containing a `date` variable and a numeric
#' `cases` variable.
#'
#' @param delay_defs A list of single row data.tables that each  defines a
#' delay distribution (model, parameters and maximum delay for each model).
#' See [lognorm_dist_def()] for an example of the structure.
#'
#' @param reporting_effect A numeric vector of length 7 that allows the scaling
#' of reported cases by the day on which they report (1 = Monday, 7 = Sunday).
#' By default no scaling occurs.
#'
#' @param reporting_model A function that takes a single numeric vector as an
#' argument and returns a single numeric vector. Can be used to apply stochastic
#' reporting effects. See the examples for details.
#'
#' @return A `data.table` containing a `date` variable (date of report) and a
#' `cases` variable. If `return_onset = TRUE` there will be a third variable
#' `reference` which indicates what the date variable refers to.
#' @export
#' @inheritParams sample_approx_dist
#' @importFrom data.table setorder data.table data.table
#' @importFrom lubridate wday
#' @author Sam Abbott
#' @examples
#' \donttest{
#' # define example cases
#' cases <- data.table::copy(example_confirmed)[, cases := as.integer(confirm)]
#'
#' # define a single report delay distribution
#' delay_def <- lognorm_dist_def(
#'   mean = 5, mean_sd = 1, sd = 3, sd_sd = 1,
#'   max_value = 30, samples = 1, to_log = TRUE
#' )
#'
#' # define a single incubation period
#' incubation_def <- lognorm_dist_def(
#'   mean = incubation_periods[1, ]$mean,
#'   mean_sd = incubation_periods[1, ]$mean_sd,
#'   sd = incubation_periods[1, ]$sd,
#'   sd_sd = incubation_periods[1, ]$sd_sd,
#'   max_value = 30, samples = 1
#' )
#'
#' # simple mapping
#' report <- adjust_infection_to_report(
#'  cases, delay_defs = list(incubation_def, delay_def)
#' )
#' print(report)
#'
#' # mapping with a weekly reporting effect
#' report_weekly <- adjust_infection_to_report(
#'   cases,
#'   delay_defs = list(incubation_def, delay_def),
#'   reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95)
#' )
#' print(report_weekly)
#'
#' # map using a deterministic median shift for both delays
#' report_median <- adjust_infection_to_report(cases,
#'   delay_defs = list(incubation_def, delay_def),
#'   type = "median"
#' )
#' print(report_median)
#'
#' # map with a weekly reporting effect and stochastic reporting model
#' report_stochastic <- adjust_infection_to_report(
#'   cases,
#'   delay_defs = list(incubation_def, delay_def),
#'   reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95),
#'   reporting_model = function(n) {
#'     out <- suppressWarnings(rnbinom(length(n), as.integer(n), 0.5))
#'     out <- ifelse(is.na(out), 0, out)
#'   }
#' )
#' print(report_stochastic)
#' }
adjust_infection_to_report <- function(infections, delay_defs,
                                       reporting_model, reporting_effect,
                                       type = "sample",
                                       truncate_future = TRUE) {
  # Reset DT Defaults on Exit
  set_dt_single_thread()

  ## deprecated
  sample_single_dist <- function(input, delay_def) {
    ## Define sample delay fn
    sample_delay_fn <- function(n, ...) {
      EpiNow2::dist_skel(
        n = n,
        model = delay_def$model[[1]],
        params = delay_def$params[[1]],
        max_value = delay_def$max_value[[1]],
        ...
      )
    }


    ## Infection to onset
    out <- EpiNow2::sample_approx_dist(
      cases = input,
      dist_fn = sample_delay_fn,
      max_value = delay_def$max_value,
      direction = "forwards",
      type = type,
      truncate_future = FALSE
    )

    return(out)
  }

  sample_dist_spec <- function(input, delay_def) {
    ## Define sample delay fn
    sample_delay_fn <- function(n, dist, cum, ...) {
      fixed_dist <- discretise(fix_dist(delay_def, strategy = "sample"))
      if (dist) {
        fixed_dist[[1]]$pmf[n + 1]
      } else {
        sample(seq_along(fixed_dist[[1]]$pmf) - 1, size = n, replace = TRUE)
      }
    }

    ## Infection to onset
    out <- EpiNow2::sample_approx_dist(
      cases = input,
      dist_fn = sample_delay_fn,
      max_value = max(delay_def),
      direction = "forwards",
      type = type,
      truncate_future = FALSE
    )

    return(out)
  }

  if (is(delay_defs, "dist_spec")) {
    report <- sample_dist_spec(infections, extract_single_dist(delay_defs, 1))
    if (length(delay_defs) > 1) {
      for (def in seq(2, length(delay_defs))) {
        report <- sample_dist_spec(report, extract_single_dist(delay_defs, def))
      }
    }
  } else {
    deprecate_warn(
      "2.0.0",
      "adjust_infection_to_report(delay_defs = 'should be a dist_spec')",
      details = "Specifying this as a list of data tables is deprecated."
    )
    report <- sample_single_dist(infections, delay_defs[[1]])
    if (length(delay_defs) > 1) {
      for (def in 2:length(delay_defs)) {
        report <- sample_single_dist(report, delay_defs[[def]])
      }
    }
  }
  ## Add a weekly reporting effect if present
  if (!missing(reporting_effect)) {
    reporting_effect <- data.table::data.table(
      day = 1:7,
      effect = reporting_effect
    )

    report <- report[, day := lubridate::wday(date, week_start = 1)]
    report <- report[reporting_effect, on = "day"]
    report <- report[, cases := as.integer(cases * effect)][
      ,
      `:=`(effect = NULL, day = NULL)
    ]

    report <- data.table::setorder(report, date)
  }

  if (!missing(reporting_model)) {
    report <- report[, cases := reporting_model(cases)]
  }

  ## Truncate reported cases by maximum infection date
  if (type == "sample" && truncate_future) {
    report <- report[date <= max(infections$date)]
  }
  return(report)
}

#' Approximate Sampling a Distribution using Counts
#'
#' @description `r lifecycle::badge("soft-deprecated")`
#' Convolves cases by a PMF function. This function will soon be removed or
#' replaced with a more robust stan implementation.
#'
#' @param cases A `<data.frame>` of cases (in date order) with the following
#' variables: `date` and `cases`.
#'
#' @param max_value Numeric, maximum value to allow. Defaults to 120 days
#'
#' @param direction Character string, defato "backwards". Direction in which to
#' map cases. Supports either "backwards" or "forwards".
#'
#' @param dist_fn Function that takes two arguments with the first being
#' numeric and the second being logical (and defined as `dist`). Should return
#' the probability density or a sample from the defined distribution. See
#' the examples for more.
#'
#' @param earliest_allowed_mapped A character string representing a date
#' ("2020-01-01"). Indicates the earliest allowed mapped value.
#'
#' @param type Character string indicating the method to use to transform
#' counts. Supports either "sample"  which approximates sampling or "median"
#' would shift by the median of the distribution.
#'
#' @param truncate_future Logical, should cases be truncated if they occur
#' after the first date reported in the data. Defaults to `TRUE`.
#'
#' @return A `<data.table>` of cases by date of onset
#' @export
#' @importFrom purrr map_dfc
#' @importFrom data.table data.table setorder
#' @importFrom lubridate days
#' @examples
#' \donttest{
#' cases <- example_confirmed
#' cases <- cases[, cases := as.integer(confirm)]
#' print(cases)
#'
#' # total cases
#' sum(cases$cases)
#'
#' delay_fn <- function(n, dist, cum) {
#'   if (dist) {
#'     pgamma(n + 0.9999, 2, 1) - pgamma(n - 1e-5, 2, 1)
#'   } else {
#'     as.integer(rgamma(n, 2, 1))
#'   }
#' }
#'
#' onsets <- sample_approx_dist(
#'   cases = cases,
#'   dist_fn = delay_fn
#' )
#'
#' # estimated onset distribution
#' print(onsets)
#'
#' # check that sum is equal to reported cases
#' total_onsets <- median(
#'   purrr::map_dbl(
#'     1:10,
#'     ~ sum(sample_approx_dist(
#'       cases = cases,
#'       dist_fn = delay_fn
#'     )$cases)
#'   )
#' )
#' total_onsets
#'
#'
#' # map from onset cases to reported
#' reports <- sample_approx_dist(
#'   cases = cases,
#'   dist_fn = delay_fn,
#'   direction = "forwards"
#' )
#'
#'
#' # map from onset cases to reported using a mean shift
#' reports <- sample_approx_dist(
#'   cases = cases,
#'   dist_fn = delay_fn,
#'   direction = "forwards",
#'   type = "median"
#' )
#' }
sample_approx_dist <- function(cases = NULL,
                               dist_fn = NULL,
                               max_value = 120,
                               earliest_allowed_mapped = NULL,
                               direction = "backwards",
                               type = "sample",
                               truncate_future = TRUE) {
  if (type == "sample") {
    if (direction == "backwards") {
      direction_fn <- rev
    } else if (direction == "forwards") {
      direction_fn <- function(x) {
        x
      }
    }
    # reverse cases so starts with current first
    reversed_cases <- direction_fn(cases$cases)
    reversed_cases[is.na(reversed_cases)] <- 0
    # draw from the density fn of the dist
    draw <- dist_fn(0:max_value, dist = TRUE, cum = FALSE)

    # approximate cases
    mapped_cases <- suppressMessages(purrr::map_dfc(
      seq_along(reversed_cases),
      ~ c(
        rep(0, . - 1),
        stats::rbinom(
          length(draw),
          rep(reversed_cases[.], length(draw)),
          draw
        ),
        rep(0, length(reversed_cases) - .)
      )
    ))


    # set dates order based on direction mapping
    if (direction == "backwards") {
      dates <- seq(min(cases$date) - lubridate::days(length(draw) - 1),
        max(cases$date),
        by = "days"
      )
    } else if (direction == "forwards") {
      dates <- seq(min(cases$date),
        max(cases$date) + lubridate::days(length(draw) - 1),
        by = "days"
      )
    }

    # summarises movements and sample for placement of non-integer cases
    case_sum <- direction_fn(rowSums(mapped_cases))
    floor_case_sum <- floor(case_sum)
    sample_cases <- floor_case_sum +
      as.numeric((runif(seq_along(case_sum)) < (case_sum - floor_case_sum)))

    # summarise imputed onsets and build output data.table
    mapped_cases <- data.table::data.table(
      date = dates,
      cases = sample_cases
    )

    # filter out all zero cases until first recorded case
    mapped_cases <- data.table::setorder(mapped_cases, date)
    mapped_cases <- mapped_cases[
      ,
      cum_cases := cumsum(cases)
    ][cum_cases != 0][, cum_cases := NULL]
  } else if (type == "median") {
    shift <- as.integer(
      median(as.integer(dist_fn(1000, dist = FALSE)), na.rm = TRUE)
    )

    if (direction == "backwards") {
      mapped_cases <- data.table::copy(cases)[
        ,
        date := date - lubridate::days(shift)
      ]
    } else if (direction == "forwards") {
      mapped_cases <- data.table::copy(cases)[
        ,
        date := date + lubridate::days(shift)
      ]
    }
  }

  if (!is.null(earliest_allowed_mapped)) {
    mapped_cases <- mapped_cases[date >= as.Date(earliest_allowed_mapped)]
  }

  # filter out future cases
  if (direction == "forwards" && truncate_future) {
    max_date <- max(cases$date)
    mapped_cases <- mapped_cases[date <= max_date]
  }
  return(mapped_cases)
}
