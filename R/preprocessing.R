##' Fill missing data in a data set to prepare it for use within the package
##'
##' @description `r lifecycle::badge("experimental")`
##' This function ensures that all days between the first and last date in the
##'   data are present. It adds an `accumulate` column that indicates whether
##'   modelled observations should be accumulated onto a later data point.
##'   point. This is useful for modelling data that is reported less frequently
##'   than daily, e.g. weekly incidence data, as well as other reporting
##'   artifacts such as delayed weekedn reporting. The function can also be used
##'   to fill in missing observations with zeros.
##'
##' @param data Data frame with a `date` column. The other columns depend on the
##'   model that the data are to be used, e.g. [estimate_infections()] or
##'   [estimate_secondary()]. See the documentation there for the expected
##'   format. The data must not already have an `accumulate` function, otherwise
##'   the function will fail with an error.
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
##'   first data point. For modelling weekly incidence data this should be set
##'   to 7. If accumulating and the first data point is not NA and this is
##'   argument is not set, then if all dates in the data have the same gap this
##'   will be taken as initial accumulation and a warning given to inform the
##'   user. If not all gaps are the same the first data point will be removed
##'   with a warning.
##' @param obs_column Character (default: "confirm"). If given, only the column
##'   specified here will be used for checking missingness. This is useful if
##'   using a data set that has multiple columns of hwich one of them
##'   corresponds to observations that are to be processed here.
##' @param by Character vector. Name(s) of any additional column(s) where
##'   data processing should be done separately for each value in the column.
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
  assert_names(
    colnames(data),
    must.include = c("date", by, obs_column),
    disjunct.from = "accumulate"
  )
  assert_date(data$date, any.missing = FALSE)

  missing_dates <- arg_match(missing_dates)
  missing_obs <- arg_match(missing_obs)

  data <- as.data.table(data)

  if (missing(initial_accumulate)) {
    ## detect frequency of accumulation if possible
    missing_date_patterns <- data[, list(pattern = unique(diff(date))), by = by]
    unique_patterns <- unique(missing_date_patterns$pattern)
    if (length(unique_patterns) == 1 && unique_patterns > 1) {
      cli_inform(
        c(
          "!" =
            "Detected fixed accumulation frequency of {unique_patterns}.
            This will be used for initial accumulation. Use
            {.var initial_accumulate} to change this behaviour. To silence this
            warning, set {.var initial_accumulate} to {unique_patterns}."
        )
      )
      initial_accumulate <- unique_patterns
    }
  } else {
    assert_integerish(initial_accumulate, lower = 1)
  }

  ## first, processing missing dates
  initial_add <- ifelse(missing(initial_accumulate), 1, initial_accumulate)

  cols <- list(
    date = seq(min(data$date) - initial_add + 1, max(data$date), by = "day")
  )
  for (by_col in by) {
    cols[[by_col]] <- unique(data[[by_col]])
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
      cli_warn(c(
        "!" = "Data contains missing dates.",
        "i" = "Missing dates are interpreted as truly missing data.
        In the future, this behaviour will be deprecated and the package
        functions will expect complete data.",
        "i" = "Complete data can be created using the `fill_missing` function."
      ))
      #nolint end
    }
  }
  return(data)
}

##' Add missing values for future dates
##'
##' @param data Data frame with a `date` column. The other columns depend on the
##'   model that the data are to be used, e.g. [estimate_infections()] or
##'   [estimate_secondary()]. See the documentation there for the expected
##'   format.
##' @param accumulate The number of days to accumulate when generating posterior
##'   prediction, e.g. 7 for weekly accumulated forecasts. If this is not set an
##'   attempt will be made to detect the accumulation frequency in the data.
##' @inheritParams fill_missing
##' @inheritParams estimate_infections
##' @importFrom data.table copy merge.data.table setDT
##' @return A data.table with missing values for future dates
##' @keywords internal
add_horizon <- function(data, horizon, accumulate = 1L,
                        obs_column = "confirm", by = NULL) {
  assert_data_frame(data)
  assert_character(obs_column)
  assert_character(by, null.ok = TRUE)
  assert_names(
    colnames(data),
    must.include = c("date", by, obs_column)
  )
  assert_integerish(horizon, lower = 0)
  assert_integerish(accumulate, lower = 1)
  assert_date(data$date, any.missing = FALSE)

  reported_cases <- data.table::setDT(data)
  if (horizon > 0) {
    reported_cases_future <- data.table::copy(reported_cases)[,
      .(date = seq(max(date) + 1, max(date) + horizon, by = "days")),
      by = by
    ]
    ## detect accumulation
    if (missing(accumulate) && "accumulate" %in% colnames(data)) {
      accumulation_times <- which(!data$accumulate)
      gaps <- unique(diff(accumulation_times))
      if (length(gaps) == 1 && gaps > 1) { ## all gaps are the same
        accumulate <- gaps
        cli_inform(c(
          "i" = "Forecasts accumulated every {gaps} days, same as accumulation
            used in the likelihood. To change this behaviour or silence this
            message set {.var accumulate} explicitly in {.fn forecast_opts}."
        ))
      }
    }
    if (accumulate > 1 || "accumulate" %in% colnames(data)) {
      ## if we accumulate add the column
      initial_future_accumulate <- sum(cumsum(rev(!data$accumulate)) == 0)
      reported_cases_future[, accumulate := TRUE]
      ## set accumulation to FALSE where appropriate
      if (horizon >= accumulate - initial_future_accumulate) {
        reported_cases_future[,
          counter := as.integer(
            date - min(date) + initial_future_accumulate + 1
          )
        ]
        reported_cases_future[
          counter %% future_accumulate == 0,
          accumulate := FALSE,
          env = list(future_accumulate = accumulate)
        ]
      }
    }
    ## fill any missing columns
    reported_cases <- rbind(
      reported_cases, reported_cases_future,
      fill = TRUE
    )
  }
  return(reported_cases[])
}

##' Add breakpoints to certain dates in a data set.
##'
##' @param dates A vector of dates to use as breakpoints.
##' @inheritParams estimate_infections
##' @return A data.table with `breakpoint` set to 1 on each of the specified
##'   dates.
##' @export
##' @importFrom data.table setDT
##' @examples
##' reported_cases <- add_breakpoints(example_confirmed, as.Date("2020-03-26"))
add_breakpoints <- function(data, dates = as.Date(character(0))) {
  assert_data_frame(data)
  assert_names(colnames(data), must.include = "date")
  assert_date(dates)
  assert_date(data$date, any.missing = FALSE)
  reported_cases <- data.table::setDT(data)
  if (is.null(reported_cases$breakpoint)) {
    reported_cases$breakpoint <- 0
  }
  missing_dates <- setdiff(dates, data$date)
  if (length(missing_dates) > 0) {
    cli_abort("Breakpoint date{?s} not found in data: {.var {missing_dates}}")
  }
  reported_cases[date %in% dates, breakpoint := 1]
  reported_cases[is.na(breakpoint), breakpoint := 0]
  return(reported_cases)
}

##' Filter leading zeros from a data set.
##'
##' @inheritParams estimate_infections
##' @inheritParams fill_missing
##' @return A data.table with leading zeros removed.
##' @export
##' @importFrom data.table setDT
##' @examples
##' cases <- data.frame(
##'   date = as.Date("2020-01-01") + 0:10,
##'   confirm = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
##' )
##' filter_leading_zeros(cases)
filter_leading_zeros <- function(data, obs_column = "confirm", by = NULL) {
  assert_data_frame(data)
  assert_character(obs_column)
  assert_character(by, null.ok = TRUE)
  assert_names(
    colnames(data),
    must.include = c("date", by, obs_column)
  )
  reported_cases <- data.table::setDT(data)
  reported_cases <- reported_cases[order(date)][
    date >= min(date[get(obs_column)[!is.na(get(obs_column))] > 0])
  ]
  return(reported_cases[])
}

##' Convert zero case counts to `NA` (missing) if the 7-day average is above a
##' threshold.
##'
##' This function aims to detect spurious zeroes by comparing the 7-day average
##' of the case counts to a threshold. If the 7-day average is above the
##' threshold, the zero case count is replaced with `NA`.
##'
##' @param threshold Numeric, defaults to `Inf`. Indicates if detected zero
##'   cases are meaningful by using a threshold number of cases based on the
##'   7-day average. If the average is above this threshold at the time of a
##'   zero observation count then the zero is replaced with a missing (`NA`)
##'   count and thus ignored in the likelihood.
##'
##' @inheritParams estimate_infections
##' @inheritParams fill_missing
##' @importFrom data.table setDT frollsum
##' @return A data.table with the zero threshold applied.
apply_zero_threshold <- function(data, threshold = Inf,
                                 obs_column = "confirm") {
  assert_data_frame(data)
  assert_numeric(threshold)
  reported_cases <- data.table::setDT(data)

  # Calculate `average_7_day` which for rows with `confirm == 0`
  # (the only instance where this is being used) equates to the 7-day
  # right-aligned moving average at the previous data point.
  reported_cases <-
    reported_cases[
      ,
      `:=`(average_7_day = (
          data.table::frollsum(get(obs_column), n = 8, na.rm = TRUE)
        ) / 7
      )
    ]
  # Check case counts preceding zero case counts and set to 7 day average if
  # average over last 7 days is greater than a threshold
  if (!is.infinite(threshold)) {
    reported_cases <- reported_cases[
      get(obs_column) == 0 & average_7_day > threshold,
      paste(obs_column) := NA_integer_
    ]
  }
  reported_cases[is.na(get(obs_column)), paste(obs_column) := NA_integer_]
  reported_cases[, "average_7_day" := NULL]
  return(reported_cases[])
}
