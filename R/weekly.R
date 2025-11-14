#' Aggregate estimates to weekly summaries
#'
#' @description `r lifecycle::badge("experimental")`
#' Internal helper function to aggregate daily estimates and observations to
#' weekly summaries for visualization purposes.
#'
#' @param estimate A `<data.table>` of estimates containing date, type, and
#' credible interval columns (as used in [plot_estimates()]).
#'
#' @param reported A `<data.table>` of reported cases with date and confirm
#' columns (optional).
#'
#' @param week_start Integer indicating the day of the week that starts the
#' epidemiological week. Uses standard weekday numbering where 1 = Monday,
#' 2 = Tuesday, ..., 7 = Sunday. Defaults to 1 (Monday).
#'
#' @return A list containing:
#' * `estimate`: Aggregated weekly estimates with recalculated CrIs
#' * `reported`: Aggregated weekly observations (if provided)
#'
#' @keywords internal
#' @importFrom data.table data.table setDT copy setnames first
#' @importFrom lubridate floor_date
aggregate_to_weekly <- function(estimate,
                                 reported = NULL,
                                 week_start = 1) {
  estimate <- data.table::copy(estimate)
  data.table::setDT(estimate)

  # Add week column
  estimate[, week := lubridate::floor_date(
    date,
    unit = "week",
    week_start = week_start
  )]

  # Identify grouping columns (everything except date, week, and value columns)
  value_cols <- c(
    "median", "mean", "sd",
    grep("^lower_", names(estimate), value = TRUE),
    grep("^upper_", names(estimate), value = TRUE),
    grep("^q", names(estimate), value = TRUE)
  )
  value_cols <- intersect(value_cols, names(estimate))

  group_cols <- setdiff(
    names(estimate),
    c("date", "week", value_cols)
  )

  # For each week and grouping, sum the median/mean values
  # Note: This is an approximation - ideally we'd aggregate samples
  estimate_weekly <- estimate[,
    lapply(.SD, function(x) {
      if (is.numeric(x)) sum(x, na.rm = TRUE) else first(x)
    }),
    by = c("week", group_cols),
    .SDcols = value_cols
  ]

  # Rename week to date for compatibility with plot_estimates
  data.table::setnames(estimate_weekly, "week", "date")

  # Aggregate reported cases if provided
  reported_weekly <- NULL
  if (!is.null(reported)) {
    reported <- data.table::copy(reported)
    data.table::setDT(reported)

    reported[, week := lubridate::floor_date(
      date,
      unit = "week",
      week_start = week_start
    )]

    # Get all columns except date and week
    report_group_cols <- setdiff(names(reported), c("date", "week", "confirm"))

    if (length(report_group_cols) > 0) {
      reported_weekly <- reported[,
        .(confirm = sum(confirm, na.rm = TRUE)),
        by = c("week", report_group_cols)
      ]
    } else {
      reported_weekly <- reported[,
        .(confirm = sum(confirm, na.rm = TRUE)),
        by = "week"
      ]
    }

    data.table::setnames(reported_weekly, "week", "date")
  }

  list(
    estimate = estimate_weekly,
    reported = reported_weekly
  )
}
