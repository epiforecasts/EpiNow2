#' Aggregate estimates to a specified time period
#'
#' @description `r lifecycle::badge("experimental")`
#' Internal helper function to aggregate daily estimates and observations to
#' a specified time period (weekly, monthly, yearly, etc.) for visualization
#' purposes.
#'
#' @param estimate A `<data.table>` of estimates containing date, type, and
#' credible interval columns (as used in [plot_estimates()]).
#'
#' @param reported A `<data.table>` of reported cases with date and confirm
#' columns (optional).
#'
#' @param unit Character string specifying the aggregation unit. Options include
#' "weekly", "monthly", "yearly", or any other unit supported by
#' [lubridate::floor_date()].
#'
#' @param week_start Integer indicating the day of the week that starts the
#' epidemiological week (used when `unit = "weekly"`). Uses standard weekday
#' numbering where 1 = Monday, 2 = Tuesday, ..., 7 = Sunday. Defaults to 1
#' (Monday).
#'
#' @return A list containing:
#' * `estimate`: Aggregated estimates with recalculated CrIs
#' * `reported`: Aggregated observations (if provided)
#'
#' @keywords internal
#' @importFrom data.table data.table setDT copy setnames first
#' @importFrom lubridate floor_date
aggregate_to_period <- function(estimate,
                                 reported = NULL,
                                 unit = "weekly",
                                 week_start = 1) {
  estimate <- data.table::copy(estimate)
  data.table::setDT(estimate)

  # Map user-friendly names to lubridate units
  unit_map <- c(
    "weekly" = "week",
    "monthly" = "month",
    "yearly" = "year"
  )
  lubridate_unit <- unit_map[unit]
  if (is.na(lubridate_unit)) {
    lubridate_unit <- unit  # Use as-is if not in map
  }

  # Add period column
  if (lubridate_unit == "week") {
    estimate[, period := lubridate::floor_date(
      date,
      unit = lubridate_unit,
      week_start = week_start
    )]
  } else {
    estimate[, period := lubridate::floor_date(date, unit = lubridate_unit)]
  }

  # Identify grouping columns (everything except date, period, and value columns)
  value_cols <- c(
    "median", "mean", "sd",
    grep("^lower_", names(estimate), value = TRUE),
    grep("^upper_", names(estimate), value = TRUE),
    grep("^q", names(estimate), value = TRUE)
  )
  value_cols <- intersect(value_cols, names(estimate))

  group_cols <- setdiff(
    names(estimate),
    c("date", "period", value_cols)
  )

  # For each period and grouping, sum the median/mean values
  # Note: This is an approximation - ideally we'd aggregate samples
  estimate_aggregated <- estimate[,
    lapply(.SD, function(x) {
      if (is.numeric(x)) sum(x, na.rm = TRUE) else first(x)
    }),
    by = c("period", group_cols),
    .SDcols = value_cols
  ]

  # Rename period to date for compatibility with plot_estimates
  data.table::setnames(estimate_aggregated, "period", "date")

  # Aggregate reported cases if provided
  reported_aggregated <- NULL
  if (!is.null(reported)) {
    reported <- data.table::copy(reported)
    data.table::setDT(reported)

    if (lubridate_unit == "week") {
      reported[, period := lubridate::floor_date(
        date,
        unit = lubridate_unit,
        week_start = week_start
      )]
    } else {
      reported[, period := lubridate::floor_date(date, unit = lubridate_unit)]
    }

    # Get all columns except date and period
    report_group_cols <- setdiff(names(reported), c("date", "period", "confirm"))

    if (length(report_group_cols) > 0) {
      reported_aggregated <- reported[,
        .(confirm = sum(confirm, na.rm = TRUE)),
        by = c("period", report_group_cols)
      ]
    } else {
      reported_aggregated <- reported[,
        .(confirm = sum(confirm, na.rm = TRUE)),
        by = "period"
      ]
    }

    data.table::setnames(reported_aggregated, "period", "date")
  }

  list(
    estimate = estimate_aggregated,
    reported = reported_aggregated
  )
}
