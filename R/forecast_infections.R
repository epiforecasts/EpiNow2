


forecast_infections <- function(infections, rts, case_only = TRUE,
                                ensemble_type = "mean", forecast_model = NULL, 
                                horizon = 14){ 
  

# Set up a mean and sd forecast -------------------------------------------
  
  sample_forecast <- function(df, samples) {
    
    ## Safe forecast wrapper
    safe_forecast <- purrr::safely(forecast_rt)
    
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
                 rlnorm(1, log(.), infections$sd[nrow(infections)]))][,
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