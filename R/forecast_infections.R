


forecast_infections <- function(infections, rts, case_only = TRUE,
                                ensemble_type = "mean", forecast_model = NULL, 
                                horizon = 14){ 
  

# Set up a mean and sd forecast -------------------------------------------
  
  sample_forecast <- function(df) {
    
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
                                 rt := purrr::map2_dbl(rt, sd_rt, ~ rlnorm(1, mean = .x, sd = .y))][,
                                 .(sample, date, horizon, rt)]
    
    return(rt_forecasts)
  }

  
# Forecast Rt -------------------------------------------------------------

  rt_forecast <- sample_forecast(rts)
  
  
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
  case_forecast <- sample_forecast(infections)
  
  ## Forecast cases from rts
  case_rt_forecast <-
    data.table::setDT(
      EpiSoon::forecast_cases(
        cases = infections[, .(date, )],
        fit_samples = rt_forecast,
        rdist = rpois,
        serial_interval = generation_pmf
      )
    )[,rt_type := "forecast"]  
  
  
  }