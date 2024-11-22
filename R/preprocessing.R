##' Fill missing data in a data set to prepare it for use within the package
##'
##' @description `r lifecycle::badge("experimental")`
##'
##' @param data Data frame with a `date` column. The other columns depend on the
##'   model that the data are to be used, e.g. [estimate_infections()] or
##'   [estimate_secondary()]. See the documentation there for the expected
##'   format.
##' @param missing_dates Character. Options are "ignore" (the default),
##'   "accumulate" and "zero". This determines how missing dates in the data are
##'   interpreted.  If set to "ignore", any missing dates in the observation
##'   data will be interpreted as missing and skipped in the likelihood. If set
##'   to "accumulate", modelled observations on dates that are missing in the
##'   data will be accumulated and added to the next non-missing data point.
##'   This can be used to model incidence data that is reported less frequently
##'   than daily. In that case, the first data point is not included in the
##'   likelihood (unless `initial_accumulate` is set to a value greater than
##'   one) but used only to reset modelled observations to zero. If "zero" then
##'   all observations on missing dates will be assumed to be of value 0.
##' @param missing_obs Character. How to process dates that exist in the data
##'   but have observations with NA values. The options available are the same
##'   ones as for the `missing_dates` argument.
##' @param initial_accumulate Integer. The number of initial dates to accumulate
##'   if `missing_dates` or `missing_obs` is set to `"accumulate"`. This
##'   argument needs ot have a minimum of 1. If it is set to 1 then no
##'   accumulation is happening on the first data point. If it is greater than 1
##'   then dates are added to the beginning of the data set to get be able to
##'   have a sufficient number of modelled observations accumulated onto the
##'   first data point. This is useful, for example, for modelling weekly
##'   incidence data, in which case this should be set to 7. If accumulating and
##'   the first data point is not NA and this is argument is not set, then that
##'   data point will be removed with a warning.
##' @param obs_column Character (default: "confirm"). If given, only the column
##'   specified here will be used for checking missingness. This is useful if
##'   using a data set that has multiple columns of hwich one of them
##'   corresponds to observations that are to be processed here.
##' @param by Character vector. Name(s) of any additional column(s) where
##'   missing data should be processed separately for each value in the column.
##'   This is useful when using data representing e.g. multiple geographies. If
##'   NULL (default) no such grouping is done.
##' @return a data.table with an `accumulate` column that indicates whether
##'   values are accumulated (see the documentation of the `data` argument in
##'   [estimate_infections()])
##' @importFrom rlang arg_match
##' @importFrom data.table as.data.table CJ
##' @importFrom checkmate assert_data_frame assert_names assert_date
##'   assert_character assert_integerish
##' @export
##' @examples
##' cases <- data.table::copy(example_confirmed)
##' ## calculate weekly sum
##' cases[, confirm := data.table::frollsum(confirm, 7)]
##' ## limit to dates once a week
##' cases <- cases[seq(7, nrow(cases), 7)]
##' ## set the second observation to missing
##' cases[2, confirm := NA]
##' ## fill missing data
##' fill_missing(cases, missing_dates = "accumulate", initial_accumulate = 7)
fill_missing <- function(data,
                         missing_dates = c("ignore", "accumulate", "zero"),
                         missing_obs = c("ignore", "accumulate", "zero"),
                         initial_accumulate,
                         obs_column = "confirm",
                         by = NULL) {
  assert_data_frame(data)
  assert_character(missing_dates)
  assert_character(missing_obs)
  assert_character(obs_column)
  assert_character(by, null.ok = TRUE)
  if (!missing(initial_accumulate)) {
    assert_integerish(initial_accumulate, lower = 1)
  }
  assert_names(
    colnames(data),
    must.include = c("date", by, obs_column),
    disjunct.from = "accumulate"
  )
  assert_date(data$date, any.missing = FALSE)

  data <- as.data.table(data)

  missing_dates <- arg_match(missing_dates)
  missing_obs <- arg_match(missing_obs)

  ## first, processing missing dates
  initial_add <- ifelse(missing(initial_accumulate), 1, initial_accumulate)

  cols <- list(
    date = seq(min(data$date) - initial_add + 1, max(data$date), by = "day")
  )
  if (!is.null(by))  {
    for (by_col in by) {
      cols[[by_col]] <- unique(data[[by_col]])
    }
  }

  complete <- do.call(CJ, cols)

  ## mark dates that are present in the data
  data[, ..present := TRUE]
  data <- merge(data, complete, by = c("date", by), all.y = TRUE)

  data[, accumulate := FALSE]
  for (col in obs_column) {
    if (missing_dates == "accumulate") {
      data[is.na(..present), accumulate := TRUE]
    } else if (missing_dates == "zero") {
      data[is.na(..present), paste(col) := 0]
    }
  }
  if (missing(initial_accumulate) &&
      isFALSE(data$accumulate[1]) &&
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
    #nolint end
    data <- data[date > min(date)]
  }

  ## second, processing missing observations
  for (col in obs_column) {
    if (missing_obs == "accumulate") {
      data[is.na(get(col)) & !is.na(..present), accumulate := TRUE]
    } else if (missing_obs == "zero") {
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
