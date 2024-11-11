##' Fills missing data in a data set to prepare it for use within the package
##'
##' @description `r lifecycle::badge("experimental")`
##'
##' @param data Data frame with a `date` column. The other columns depend on the
##'   model that the data are to be used, e.g. [estimate_infections()] or
##'   [estimate_secondary()]. See the documentation there for the expected
##'   format.
##' @param missing Character. Options are "missing" (the default), "accumulate"
##'   and "zero". This determines how missing dates the data are interpreted.
##'   If set to "missing", any missing dates in the observation data will be
##'   interpreted as missing and skipped in the likelihood. If set to
##'   "accumulate", modelled observations will be accumulated and added to the
##'   next non-missing data point. This can be used to model incidence data that
##'   is reported at less than daily intervals. If set to "accumulate", the
##'   first data point is not included in the likelihood (unless `initial` is
##'   set to a non-zero value) but used only to reset modelled observations to
##'   zero. If "zero" then all observations on missing dates will be assumed to
##'   be of value 0.
##' @param na Character. How to process dates that have NA values. The options
##'   available are the same ones as for the `missing` argument.
##' @param initial_accumulate Integer. The number of initial dates to accumulate
##'   if `missing` or a column name is set to `"accumulate"`. This number of
##'   dates is added to the beginning of the data set to be accumulated onto the
##'   first data point. This is useful, for example, for modelling weekly
##'   incidence data, in which case this should be set to 7. If accumulating and
##'   the first data point is one to accumulate and this is not set, then that
##'   data point will be removed with a warning.
##' @param obs_column Character. If given, only the column specified here will
##'   be used for checking missingness. This is useful if using a data set that
##'   has NA values in multiple columns, but only one of them corresponds to
##'   observations that are to be processed here. By default, all columns will
##'   be processed and, e.g. `accumulate` will be set if any column is NA in any
##'   given row.
##' @return a data.table with an `accumulate` column that indicates whether
##'   values are accumulated (see the documnetation of the `data` arugment in
##'   [estiamte_infections()])
##' @importFrom rlang arg_match
##' @importFrom data.table as.data.table data.table
##' @export
##' @examples
##' cases <- data.table::copy(reported_cases)
##' ## calculate weekly sum
##' cases[, confirm := frollsum(confirm, 7)]
##' ## limit to dates once a week
##' cases <- cases[seq(7, nrow(reported_cases_weekly), 7)]
##' ## set the second observation to missing
##' cases[2, confirm := NA]
##' ## fill missing data
##' fill_missing(cases, missing = "accumulate", initial_accumulate = 7)
fill_missing <- function(data,
                         missing = c("missing", "accumulate", "zero"),
                         na = c("missing", "accumulate", "zero"),
                         initial_accumulate,
                         obs_column) {
  assert_data_frame(data)
  assert_names(names(data), must.include = "date", disjunct.from = "accumulate")
  if (!missing(obs_column)) {
    assert_names(names(data), must.include = obs_column)
  } else {
    obs_column <- setdiff(colnames(data), "date")
  }
  assert_date(data$date, any.missing = FALSE)

  data <- as.data.table(data)

  missing <- arg_match(missing)
  na <- arg_match(na)

  ## first, processing missing dates
  initial_add <- ifelse(missing(initial_accumulate), 1, initial_accumulate)

  date_seq <- seq.Date(
    min(data$date) - initial_add + 1,
    max(data$date), by = "day"
  )

  ## mark dates that are present in the data
  data[, ..present := TRUE]
  complete_dates <- data.table(date = date_seq)
  data <- merge(data, complete_dates, by = "date", all.y = TRUE)

  data[, accumulate := FALSE]
  for (col in obs_column) {
    if (missing == "accumulate") {
      data[is.na(..present), accumulate := TRUE]
    } else if (missing == "zero") {
      data[is.na(..present), paste(col) := 0]
    }
  }
  if (missing(initial_accumulate) &&
      !data$accumulate[1] &&
      any(data$accumulate)) {
    #nolint start: duplicate_argument_linter
    cli_warn(
      c(
        "!" =
          "Initial data point not marked as accumulated but some others are.",
        "i" =
          "This means that the first data point will not be included in the
           likelihood.",
        "i" =
          "To change this behaviour, set `initial_accumulate`
           (including setting it to 1 if the first data point is to be taken
            as is, i.e. as the first daily data point)."
      )
    )
    data <- data[-1]
    #nolint end
  }

  ## second, processing missing dates
  for (col in obs_column) {
    if (na == "accumulate") {
      data[is.na(get(col)) & !is.na(..present), accumulate := TRUE]
    } else if (missing == "zero") {
      data[is.na(get(col)) & !is.na(..present), paste(col) := 0]
    }
  }

  data[, ..present := NULL]

  return(data[])
}

##' Temporary function to support the transition to full support of missing
##' data.
##'
##' @description `r lifecycle::badge("deprecated")`
##'
##' @inheritParams create_stan_data
##' @inheritParams fill_missing
##' @return data set with missing dates filled in as na values
##' @keywords internal
default_fill_missing_obs <- function(data, obs, obs_column) {
  if (!("accumulate" %in% colnames(data))) {
    data_rows <- nrow(data)
    data <- fill_missing(data = data, obs_column = obs_column)
    if (nrow(data) > data_rows && !obs$accumulate) {
      #nolint start: duplicate_argument_linter
      cli_warn(
        "!" = "Data contains missing dates.",
        "i" = "Missing dates are interpreted as truly missing data.
        In the future, this behaviour will be deprecated and the package
        functions will expect complete data.",
        "i" = "Complete data can be created using the `fill_missing` function."
      )
      #nolint end
    }
  }
  return(data)
}
