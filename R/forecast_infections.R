#' Forecast Infections and the Time-Varying Reproduction Number
#'
#' @description `r lifecycle::badge("experimental")`
#' Provides optional tools for forecasting cases and Rt estimates using the timeseries methods
#' (via the `EpiSoon` package). It requires the `Episoon` package. Installation instructions for the EpiSoon package are
#' available [here](https://epiforecasts.io/EpiSoon/).
#' @param infections A data frame of cases by date of infection containing the following variables: date, mean, sd
#' @param rts  A data frame of Rt estimates by date of infection containing the following variables: date, mean, sd
#' @param ensemble_type Character string indicating the type of ensemble to use. By default this is
#' an unweighted ensemble ("mean") with no other types currently supported.
#' @param forecast_model An uninitialised forecast model function to be passed to `EpiSoon::forecast_rt`. Used
#' for forecasting future Rt and case co An example of the required structure is: `function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}`.
#' @param horizon Numeric, defaults to 14. The horizon over which to forecast Rts and cases.
#' @param samples Numeric, the number of forecast samples to take.
#' @param gt_mean Numeric, the mean of the gamma distributed generation time.
#' @param gt_sd Numeric, the standard deviation of the gamma distributed generation time.
#' @param gt_max Numeric, the maximum allowed value of the gamma distributed generation time.
#' @return A list of `data.tables`. The first entry ("samples") contains raw forecast samples and the second entry ("summarised") contains
#' summarised forecasts.
#' @export
#' @inheritParams calc_summary_measures
#' @importFrom data.table setDT := setorder setDTthreads
#' @importFrom purrr safely map_dbl
#' @importFrom truncnorm rtruncnorm
#' @examples
#' \donttest{
#' if (requireNamespace("EpiSoon")) {
#'   if (requireNamespace("forecastHybrid")) {
#'     # example case data
#'     reported_cases <- example_confirmed[1:40]
#'
#'     generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#'     incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#'     reporting_delay <- estimate_delay(rlnorm(100, log(6), 1), max_value = 15)
#'
#'     # estimate Rt and infections from data
#'     out <- estimate_infections(reported_cases,
#'       generation_time = generation_time,
#'       delays = delay_opts(incubation_period, reporting_delay),
#'       rt = rt_opts(prior = list(mean = 2, sd = 0.1))
#'     )
#'
#'     # forecast Rt and infections from estimates
#'     forecast <- forecast_infections(
#'       infections = out$summarised[variable == "infections"],
#'       rts = out$summarised[variable == "R"],
#'       gt_mean = out$summarised[variable == "gt_mean"]$mean,
#'       gt_sd = out$summarised[variable == "gt_sd"]$mean,
#'       gt_max = 30,
#'       forecast_model = function(y, ...) {
#'         EpiSoon::forecastHybrid_model(
#'           y = y[max(1, length(y) - 21):length(y)],
#'           model_params = list(models = "aefz", weights = "equal"),
#'           forecast_params = list(PI.combination = "mean"), ...
#'         )
#'       },
#'       horizon = 14,
#'       samples = 1000
#'     )
#'
#'     forecast$summarised
#'   }
#' }
#' }
forecast_infections <- function(infections, rts,
                                gt_mean, gt_sd, gt_max = 30,
                                ensemble_type = "mean", forecast_model,
                                CrIs = c(0.2, 0.5, 0.9),
                                horizon = 14, samples = 1000) {
  if (!requireNamespace("EpiSoon", quietly = TRUE)) {
    stop('The EpiSoon package is missing. Install it with:
         install.packages("drat"); drat:::add("epiforecasts"); install.packages("EpiSoon")')
  }

  # set to data.table if not ------------------------------------------------
  data.table::setDTthreads(1)
  infections <- data.table::setDT(infections)
  rts <- data.table::setDT(rts)

  # warnings ----------------------------------------------------------------
  if (missing(forecast_model)) {
    stop("A forecasting model has not been supplied so no forecast can be produced. See the documentation for examples.")
  }

  # set up a mean and sd forecast -------------------------------------------
  sample_forecast <- function(df, samples) {
    # safe forecast wrapper
    safe_forecast <- purrr::safely(EpiSoon::forecast_rt)

    # forecast Rts using the mean estimate
    rt_forecasts <-
      data.table::setDT(
        safe_forecast(
          rts = df[, .(date, rt = mean)],
          model = forecast_model,
          horizon = horizon,
          samples = samples
        )[[1]]
      )

    rt_sd <- df[date == max(date, na.rm = TRUE)]$sd
    rt_sd <- ifelse(rt_sd <= 0, 1e-3, rt_sd)

    # sample from assumed lognormal distribution
    rt_forecasts <- rt_forecasts[, rt := purrr::map_dbl(rt, ~ truncnorm::rtruncnorm(1,
      a = 0,
      mean = .,
      sd = rt_sd
    ))][
      ,
      .(sample, date, horizon, rt)
    ]
    return(rt_forecasts)
  }


  # forecast Rt -------------------------------------------------------------
  rt_forecast <- sample_forecast(rts, samples = samples)

  # define generation time pmf ----------------------------------------------
  # define generation pmf
  generate_pmf <- function(mean, sd, max_value) {
    params <- list(
      alpha = (mean / sd)^2,
      beta = mean / sd^2
    )

    # define with 0 day padding
    sample_fn <- function(n, ...) {
      c(0, EpiNow2::dist_skel(
        n = n,
        model = "gamma",
        params = params,
        max_value = max_value,
        ...
      ))
    }

    dist_pdf <- sample_fn(0:(max_value - 1), dist = TRUE, cum = FALSE)

    return(dist_pdf)
  }

  generation_pmf <- generate_pmf(gt_mean, gt_sd, max_value = gt_max)

  # forecast cases ----------------------------------------------------------
  # forecast cases from cases
  case_forecast <- sample_forecast(infections, samples = samples)[
    ,
    `:=`(cases = rt, forecast_type = "case")
  ][, rt := NULL]

  # forecast cases from rts and mean infections
  case_rt_forecast <-
    data.table::setDT(
      EpiSoon::forecast_cases(
        cases = infections[, .(date, cases = mean)],
        fit_samples = rt_forecast,
        rdist = rpois,
        serial_interval = generation_pmf
      )
    )

  # sample case forecast based on last observed infection standard deviation
  case_rt_forecast <- case_rt_forecast[, cases := purrr::map_dbl(
    cases,
    ~ as.integer(truncnorm::rtruncnorm(1, a = 0, mean = ., sd = infections$sd[nrow(infections)]))
  )][
    ,
    forecast_type := "rt"
  ]

  case_forecast <- data.table::rbindlist(list(
    case_forecast, case_rt_forecast
  ), use.names = TRUE)

  # ensemble forecast -------------------------------------------------------
  if (ensemble_type %in% "mean") {
    ensemble_forecast <- data.table::copy(case_forecast)[, .(
      cases = mean(cases, na.rm = TRUE),
      forecast_type = "ensemble"
    ),
    by = .(sample, date, horizon)
    ]
    case_forecast <- data.table::rbindlist(list(case_forecast, ensemble_forecast))
  }

  # combine forecasts -------------------------------------------------------
  forecast <- data.table::rbindlist(list(
    rt_forecast[, value := rt][, rt := NULL][, type := "rt"],
    case_forecast[, value := cases][, cases := NULL][, type := "case"]
  ), fill = TRUE)

  # summarise forecasts -----------------------------------------------------
  summarised_forecast <- calc_summary_measures(forecast,
    summarise_by = c("date", "type", "forecast_type"),
    order_by = c("type", "forecast_type", "date"),
    CrIs = CrIs
  )

  # combine output
  out <- list(samples = forecast, summarised = summarised_forecast)
  return(out)
}
