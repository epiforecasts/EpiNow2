#' Convert EpiNow2 model output to a `forecast_sample` object
#'
#' @description `r lifecycle::badge("experimental")`
#' Convert outputs of EpiNow2 fitting and forecasting functions to
#' `forecast_sample` objects from the `scoringutils` package for evaluating
#' predictive performance. Methods are provided for objects returned by
#' [epinow()], [estimate_infections()], [forecast_secondary()], and
#' [estimate_truncation()].
#'
#' These methods extract sample-level posterior predictions via
#' [get_predictions()] with `format = "sample"`, merge them with the supplied
#' observations on `date`, and pass the result to
#' [scoringutils::as_forecast_sample()].
#'
#' `scoringutils` is an optional dependency. Methods are registered via
#' delayed S3 method registration so that EpiNow2 can be loaded and checked
#' without `scoringutils` installed; the methods themselves require it.
#'
#' @param data Output of [epinow()], [estimate_infections()],
#'   [forecast_secondary()], or [estimate_truncation()].
#' @param observations A `<data.frame>` of observed values to score against.
#'   Must contain a `date` column. For [epinow()] and [estimate_infections()]
#'   objects must also contain a `confirm` column; for [forecast_secondary()]
#'   objects a `secondary` column; for [estimate_truncation()] objects a
#'   `confirm` column representing the latest, least-truncated observations.
#' @param ... Additional arguments passed to
#'   [scoringutils::as_forecast_sample()].
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
as_forecast_sample.estimate_infections <- function(data, observations, ...) {
  build_forecast_sample(data, observations, observed_col = "confirm", ...)
}

#' @rdname as_forecast_sample
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.epinow <- function(data, observations, ...) {
  if (!is.null(data$error)) {
    cli::cli_abort(c(
      "Cannot convert a failed {.fn epinow} run to a {.cls forecast_sample}.",
      "i" = "The run failed with error: {data$error}"
    ))
  }
  build_forecast_sample(data, observations, observed_col = "confirm", ...)
}

#' @rdname as_forecast_sample
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.forecast_secondary <- function(data, observations, ...) {
  build_forecast_sample(data, observations, observed_col = "secondary", ...)
}

#' @rdname as_forecast_sample
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.estimate_truncation <- function(data, observations, ...) {
  build_forecast_sample(data, observations, observed_col = "confirm", ...)
}

#' Build a `forecast_sample` from EpiNow2 sample predictions
#'
#' Internal helper that gates on `scoringutils` availability, extracts samples
#' via [get_predictions()], merges with observations, and forwards to
#' [scoringutils::as_forecast_sample()].
#'
#' @param data An EpiNow2 model fit or forecast object.
#' @param observations A `<data.frame>` with `date` and the column named in
#'   `observed_col`.
#' @param observed_col Name of the observed-value column in `observations`.
#' @param ... Additional arguments passed to
#'   [scoringutils::as_forecast_sample()].
#'
#' @return A `forecast_sample` object.
#' @keywords internal
build_forecast_sample <- function(data, observations, observed_col, ...) {
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

  predictions <- get_predictions(data, format = "sample")
  obs <- observations[, c("date", observed_col), with = FALSE]
  data.table::setnames(obs, observed_col, "observed")
  forecasts <- merge(predictions, obs, by = "date")

  scoringutils::as_forecast_sample(
    forecasts,
    observed = "observed",
    predicted = "predicted",
    sample_id = "sample",
    ...
  )
}
