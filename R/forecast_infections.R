#' Estimate Infections and the Time-Varying Reproduction Number
#'
#' @param infections A data frame of cases by date of infection containing the following variables: date, mean, sd
#' @param rts  A data frame of Rt estimates by date of infection containing the following variables: date, mean, sd
#' @param ensemble_type Character string indicating the type of ensemble to use. By default this is 
#' an unweighted ensemble ("mean") with no other types currently supported.
#' @param forecast_model An uninitialised forecast model function to be passed to `EpiSoon::forecast_rt`. Used 
#' for forecasting future Rt and case co An example of the required structure is: `function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}`.
#' @param horizon Numeric, defaults to 14. The horizon over which to forecast Rts and cases.
#' @param samples Numeric, the number of forecast samples to take.
#' @return A list of `data.tables`. The first entry ("samples") contains raw forecast samples and the second entry ("summarised") contains
#' summarised forecasts.
#' @export
#' @importFrom data.table setDT := setorder setDTthreads
#' @importFrom purrr safely map2_dbl map_dbl
#' @importFrom EpiSoon forecast_rt
#' @importFrom HDInterval hdi
#' @examples
#' \dontrun{
#' library(EpiSoon)
#' library(forecastHybrid)
#' 
#' reported_cases <- NCoVUtils::get_ecdc_cases(countries = "Russia")
#' reported_cases <- NCoVUtils::format_ecdc_data(reported_cases)
#' reported_cases <- data.table::as.data.table(reported_cases)[, confirm := cases][, cases := NULL][1:90]
#'  
#' generation_time <- list(mean = EpiNow2::covid_generation_times_summary[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times_summary[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times_summary[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times_summary[1, ]$sd_sd,
#'                         max = 30)
#'                           
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- list(mean = log(5),
#'                         mean_sd = log(2),
#'                         sd = log(2),
#'                         sd_sd = log(1.5),
#'                         max = 30)
#'                         
#' rt_prior <- list(mean = 2.6, sd = 2)
#'   
#' 
#' ## Run model
#' out <- estimate_infections(reported_cases, family = "negbin",
#'                            generation_time = generation_time,
#'                            incubation_period = incubation_period,
#'                            reporting_delay = reporting_delay,
#'                            rt_prior = rt_prior,
#'                            model = model,
#'                            cores = 4, chains = 4,
#'                            estimate_rt = TRUE,
#'                            verbose = TRUE, return_fit = TRUE)
#'
#'
#' forecast <- forecast_infections(infections = out$summarised[variable == "infections"],
#'                                 rts = out$summarised[variable == "R"],
#'                                 gt_mean = out$summarised[variable == "gt_mean"]$mean,
#'                                 gt_sd = out$summarised[variable == "gt_sd"]$mean,
#'                                 forecast_model = function(y, ...){
#'                                 EpiSoon::forecastHybrid_model(y = y[max(1, length(y) - 21):length(y)],
#'                                 model_params = list(models = "aefz", weights = "equal"),
#'                                 forecast_params = list(PI.combination = "mean"), ...)},
#'                                 horizon = 14, 
#'                                 samples = 1000)
#'                                 
#' forecast
#' }                              
forecast_infections <- function(infections, rts, 
                                gt_mean, gt_sd,
                                ensemble_type = "mean", 
                                forecast_model, 
                                horizon = 14,
                                samples = 1000){ 
  

# Set to data.table if not ------------------------------------------------
data.table::setDTthreads(1)
  
infections <- data.table::setDT(infections)
rts <- data.table::setDT(rts)


# Warnings ----------------------------------------------------------------

if (missing(forecast_model)) {
  stop("A forecasting model has not been supplied so no forecast can be produced. See the documentation for examples.")
}

# Set up a mean and sd forecast -------------------------------------------
  
  sample_forecast <- function(df, samples) {
    
    ## Safe forecast wrapper
    safe_forecast <- purrr::safely(EpiSoon::forecast_rt)
    
    ## Forecast Rts using the mean estimate
    rt_forecasts <-
      data.table::setDT(
        safe_forecast(rts = df[, .(date, rt = mean)],
                      model = forecast_model,
                      horizon = horizon,
                      samples = samples)[[1]]
      )
    
    ## Forecast the variance using the same model structure
    sd_forecasts <-
      data.table::setDT(
        safe_forecast(rts = df[, .(date, rt = sd)],
                      model = forecast_model,
                      horizon = horizon,
                      samples = samples)[[1]]
      )[, sd_rt := rt][,rt := NULL]
    
    ## Join mean and sd forecasts
    rt_forecasts <- rt_forecasts[sd_forecasts, on = c("date", "sample", "horizon")]
    
    ## Sample from assumed lognormal distribution
    rt_forecasts <- rt_forecasts[sd_rt <= 0, sd_rt := 1e-4][,
                                 rt := purrr::map2_dbl(rt, sd_rt, ~ rlnorm(1, mean = log(.x), 
                                                                           sd = log(.y)))][,
                                 .(sample, date, horizon, rt)]
    
    return(rt_forecasts)
  }

  
# Forecast Rt -------------------------------------------------------------

  rt_forecast <- sample_forecast(rts, samples = samples)
  
# Define generation time pmf ----------------------------------------------
  
  
  ## Define generation pmf
  generate_pmf <- function(mean, sd, max_value) {
    params <- list(
      alpha = (mean/sd)^2,
      beta = mean/sd^2
    )
    
    ## Define with 0 day padding
    sample_fn <- function(n, ...) {
      c(0, EpiNow::dist_skel(n = n,
                             model = "gamma",
                             params = params,
                             max_value = max_value,
                             ...))
    }
    
    dist_pdf <- sample_fn(0:(max_value - 1), dist = TRUE, cum = FALSE)
    
    return(dist_pdf)
  }
  
  generation_pmf <- generate_pmf(gt_mean, gt_sd, max_value = gt_max)
  
# Forecast cases ----------------------------------------------------------

  ## Forecast cases from cases
  case_forecast <- sample_forecast(infections)[, forecast_type := "case"]
  
  ## Forecast cases from rts and mean infections
  case_rt_forecast <-
    data.table::setDT(
      EpiSoon::forecast_cases(
        cases = infections[, .(date, cases = mean)],
        fit_samples = rt_forecast,
        rdist = rpois,
        serial_interval = generation_pmf
      )
    )
  
  ## Sample case forecast based on last observed infection standard deviation
  case_rt_forecast <- case_rt_forecast[, cases := purrr::map_dbl(cases,
                 rlnorm(1, log(.), log(infections$sd[nrow(infections)])))][,
                                         forecast_type := "rt"]
  
  
  case_forecast <- data.table::rbindlist(list(
    case_forecast, case_rt_forecast))
  

# Ensemble forecast -------------------------------------------------------

  if (ensemble_type %in% "mean") {
    
    ensemble_forecast <- data.table::copy(case_forecast)[, .(cases = mean(cases, na.rm = TRUE),
                                                             forecast_type = "ensemble"),
                                                         by = .(sample, date)]
    
    case_forecast <- data.table::rbindlist(list(case_forecast, ensemble_forecast))
  }
  
  

# Combine forecasts -------------------------------------------------------

  forecast <- data.table::rbindlist(list(
    rt_forecast[, value := rt][, rt := NULL][, type := "rt"],
    case_forecast[, value := cases][, cases := NULL][, type := "type"]
  ), fill = TRUE)
  
  

# Summarise forecasts -----------------------------------------------------

  summarised_forecast <- data.table::copy(forecast)[, .(
    bottom  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.9)), ~ .[[1]])),
    top = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.9)), ~ .[[2]])),
    lower  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.5)), ~ .[[1]])),
    upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.5)), ~ .[[2]])),
    median = as.numeric(median(value, na.rm = TRUE)),
    mean = as.numeric(mean(value, na.rm = TRUE)),
    sd = as.numeric(sd(value, na.rm = TRUE))), by = .(date, variable)]
  
  ## Order summarised samples
  data.table::setorder(summarised_forecast, variable, date)  

  
  ## Combine output
  out <- list(samples = forecast, summarised = summarised_forecast)
  
  return(out)
  }