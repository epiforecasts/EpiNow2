#' Adjust from Case Counts by Infection Date to Date of Report
#'
#' @description  `r lifecycle::badge("deprecated")`
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
#' @importFrom lifecycle deprecate_warn
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
  #nolint start: missing_argument_linter
  deprecate_warn(
    "1.5.0",
    "adjust_infection_to_report()"
  )
  #nolint end
  # Reset DT Defaults on Exit
  set_dt_single_thread()

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

  report <- sample_single_dist(infections, delay_defs[[1]])

  if (length(delay_defs) > 1) {
    for (def in 2:length(delay_defs)) {
      report <- sample_single_dist(report, delay_defs[[def]])
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
