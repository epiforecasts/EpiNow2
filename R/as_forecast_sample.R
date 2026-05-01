#' Convert EpiNow2 model output to a `forecast_sample` object
#'
#' @description `r lifecycle::badge("experimental")`
#' Convert outputs of EpiNow2 fitting and forecasting functions to
#' `forecast_sample` objects via [scoringutils::as_forecast_sample()] for
#' evaluating predictive performance. Methods are provided for objects
#' returned by [epinow()], [estimate_infections()], [forecast_secondary()],
#' and [estimate_truncation()].
#'
#' These methods extract sample-level posterior predictions via
#' [get_predictions()] with `format = "sample"`, merge them with the supplied
#' observations on `date`, and pass the result to
#' [scoringutils::as_forecast_sample()].
#'
#' [scoringutils] is an optional dependency; calling these methods without it
#' installed gives an informative error.
#'
#' @param data Output of [epinow()], [estimate_infections()],
#'   [forecast_secondary()], or [estimate_truncation()].
#' @param observations A `<data.frame>` of observed values to score against.
#'   Must contain a `date` column. For [epinow()] and [estimate_infections()]
#'   objects must also contain a `confirm` column; for [forecast_secondary()]
#'   objects a `secondary` column; for [estimate_truncation()] objects a
#'   `confirm` column representing the latest, least-truncated observations.
#' @param horizon Optional numeric vector of horizons to keep, applied via
#'   `%in%` to the `horizon` column of [get_predictions()] output before
#'   scoring. If `NULL` (default), a class-specific filter is applied:
#'   horizons `>= 0` (i.e. forecast period only) for [epinow()],
#'   [estimate_infections()] and [forecast_secondary()] objects, and no
#'   filter for [estimate_truncation()] objects (where horizons are
#'   non-positive by construction).
#' @param ... Additional arguments passed to
#'   [scoringutils::as_forecast_sample()]. `forecast_unit` is set
#'   automatically from the object class (`forecast_date`, `date`, `horizon`,
#'   plus `dataset` for [estimate_truncation()]) and cannot be overridden.
#'
#' @return A `forecast_sample` object as returned by
#'   [scoringutils::as_forecast_sample()]. Rows for which `observations` does
#'   not provide a value on the corresponding `date` are dropped.
#'
#' @seealso [get_predictions()] for the underlying sample extraction.
#' @name as_forecast_sample
#' @examplesIf interactive() && rlang::is_installed("scoringutils")
#' library(scoringutils)
#'
#' reported_cases <- example_confirmed[1:60]
#' fit <- estimate_infections(
#'   reported_cases,
#'   generation_time = gt_opts(example_generation_time),
#'   delays = delay_opts(example_incubation_period + example_reporting_delay),
#'   rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.2)),
#'   forecast = forecast_opts(horizon = 7)
#' )
#'
#' forecast_obj <- as_forecast_sample(fit, observations = example_confirmed)
#' score(forecast_obj)
NULL

#' @rdname as_forecast_sample
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.estimate_infections <- function(data, observations,
                                                   horizon = NULL, ...) {
  .build_forecast_sample(
    data, observations,
    observed_col = "confirm",
    forecast_unit = c("forecast_date", "date", "horizon"),
    horizon = horizon,
    default_horizon = function(h) h >= 0,
    ...
  )
}

#' @rdname as_forecast_sample
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.epinow <- function(data, observations,
                                      horizon = NULL, ...) {
  if (!is.null(data$error)) {
    cli::cli_abort(c(
      "Cannot convert a failed {.fn epinow} run to a {.cls forecast_sample}.",
      "i" = "The run failed with error: {data$error}"
    ))
  }
  .build_forecast_sample(
    data, observations,
    observed_col = "confirm",
    forecast_unit = c("forecast_date", "date", "horizon"),
    horizon = horizon,
    default_horizon = function(h) h >= 0,
    ...
  )
}

#' @rdname as_forecast_sample
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.forecast_secondary <- function(data, observations,
                                                  horizon = NULL, ...) {
  .build_forecast_sample(
    data, observations,
    observed_col = "secondary",
    forecast_unit = c("forecast_date", "date", "horizon"),
    horizon = horizon,
    default_horizon = function(h) h >= 0,
    ...
  )
}

#' @rdname as_forecast_sample
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.estimate_truncation <- function(data, observations,
                                                   horizon = NULL, ...) {
  .build_forecast_sample(
    data, observations,
    observed_col = "confirm",
    forecast_unit = c("dataset", "forecast_date", "date", "horizon"),
    horizon = horizon,
    default_horizon = function(h) rep(TRUE, length(h)),
    ...
  )
}

.build_forecast_sample <- function(data, observations, observed_col,
                                   forecast_unit, horizon, default_horizon,
                                   ...) {
  rlang::check_installed(
    "scoringutils",
    reason = "to convert EpiNow2 outputs to forecast_sample objects."
  )
  if (missing(observations)) {
    cli::cli_abort(
      "{.arg observations} must be supplied to score predictions."
    )
  }
  observations <- data.table::as.data.table(observations)
  required <- c("date", observed_col)
  missing_cols <- setdiff(required, names(observations))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg observations} is missing required column{?s} \\
      {.var {missing_cols}}."
    )
  }
  if (anyDuplicated(observations$date) > 0L) {
    cli::cli_abort(
      "{.arg observations} must contain unique {.var date} values."
    )
  }

  predictions <- get_predictions(data, format = "sample")
  keep <- if (is.null(horizon)) {
    default_horizon(predictions$horizon)
  } else {
    predictions$horizon %in% horizon
  }
  predictions <- predictions[keep]

  obs <- observations[, c("date", observed_col), with = FALSE]
  data.table::setnames(obs, observed_col, "observed")
  forecasts <- merge(predictions, obs, by = "date")

  dots <- list(...)
  dots$forecast_unit <- NULL

  do.call(
    scoringutils::as_forecast_sample,
    c(
      list(
        data = forecasts,
        forecast_unit = forecast_unit,
        observed = "observed",
        predicted = "predicted",
        sample_id = "sample"
      ),
      dots
    )
  )
}
