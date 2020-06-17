#' Estimate time-varying measures and forecast
#'
#' @param nowcast A nowcast as produced by `nowcast_pipeline`
#' @param rt_windows Numeric vector, windows over which to estimate time-varying R. The best performing window will be 
#' selected per serial interval sample by default (based on which window best forecasts current cases).
#' @param rate_window Numeric, the window to use to estimate the rate of spread.
#' @param verbose Logical, defaults to `TRUE`. Should progress messages be shown.
#' @inheritParams estimate_R0
#' @return A list of data frames containing reproduction number estimates, case forecasts, rate of growth estimates
#' both summarised and raw.
#' @export
#' @importFrom purrr safely map_dbl map pmap map_lgl
#' @importFrom HDInterval hdi
#' @importFrom future.apply future_lapply
#' @importFrom data.table setorder rbindlist copy setDTthreads
#' @examples 
#' ## Construct example distributions
#' ## reporting delay dist
#' delay_dist <- EpiNow::lognorm_dist_def(mean = 3, 
#'                                       mean_sd = 1,
#'                                       sd = 3,
#'                                       sd_sd = 1,
#'                                       max_value = 30,
#'                                       samples = 1)
#' 
#' ## incubation delay dist
#' incubation_dist <- delay_dist
#' 
#' ## Uses example case vector from EpiSoon
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)
#' cases <- cases[, `:=`(confirm = as.integer(cases), import_status = "local")]
#' 
#' ## Basic nowcast
#' nowcast <- nowcast_pipeline(reported_cases = cases, 
#'                             target_date = max(cases$date),
#'                             delay_defs = delay_dist,
#'                             incubation_defs = incubation_dist)
#'  
#' ## Estimate parameters                           
#' estimates <- epi_measures_pipeline(nowcast[type %in% "infection_upscaled"], 
#'                        generation_times = EpiNow::covid_generation_times,
#'                        rt_prior = list(mean_prior = 2.6, std_prior = 2))   
#'                        
#' estimates                                                             
epi_measures_pipeline <- function(nowcast = NULL,
                                  generation_times = NULL,
                                  min_est_date = NULL,
                                  gt_samples = 1, rt_samples = 5,
                                  rt_windows = 7, rate_window = 7,
                                  rt_prior = NULL, forecast_model = NULL,
                                  horizon = 0, verbose = TRUE) {
  
  ## Estimate time-varying R0
  process_R0 <- function(data,
                         generation_times,
                         rt_prior,
                         gt_samples, 
                         rt_samples, 
                         rt_windows,
                         min_est_date,
                         forecast_model,
                         horizon){
    data.table::setDTthreads(1)

    safe_R0 <- purrr::safely(EpiNow::estimate_R0)
    
    estimates <- safe_R0(cases = data,
                         generation_times = generation_times,
                         rt_prior = rt_prior,
                         gt_samples = gt_samples,
                         rt_samples = rt_samples,
                         windows = rt_windows,
                         min_est_date = min_est_date,
                         forecast_model = forecast_model,
                         horizon = horizon)[[1]]

    if (!is.null(estimates$rts)) {
      estimates$rts <-  estimates$rts[,
                `:=`(type = data$type[1], sample = as.numeric(data$sample[1]))]
    }

    if (!is.null(estimates$cases)) {
      estimates$cases <- estimates$cases[, `:=`(type = data$type[1],
                                                sample = as.numeric(data$sample[1]))]
    }

    return(estimates)
  }

  if (verbose) {
    message("Estimate time-varying R0")
  }

  estimates <-  future.apply::future_lapply(split(nowcast, by = c("type", "sample")),
                                 process_R0,
                                 generation_times = generation_times,
                                 rt_prior = rt_prior,
                                 gt_samples = gt_samples,
                                 rt_samples = rt_samples,
                                 rt_windows = rt_windows,
                                 min_est_date = min_est_date,
                                 forecast_model = forecast_model,
                                 horizon = horizon,
                                 future.scheduling = 10)

  ## Clean up NULL rt estimates and bind together
  R0_estimates <- data.table::rbindlist(
    purrr::map(estimates, ~ .$rts)
  )


  ## Generic HDI return function
  return_hdi <- function(vect = NULL, mass = NULL, index = NULL) {
    as.numeric(purrr::map_dbl(list(HDInterval::hdi(vect, credMass = mass)), ~ .[[index]]))
  }

  if (verbose) {
  message("Summarising time-varying R0")
  }

  R0_estimates_sum <- data.table::copy(R0_estimates)[, .(
    bottom  = return_hdi(R, 0.9, 1),
    top = return_hdi(R, 0.9, 2),
    lower  = return_hdi(R, 0.5, 1),
    upper = return_hdi(R, 0.5, 2),
    median = median(R, na.rm = TRUE),
    mean = mean(R, na.rm = TRUE),
    std = sd(R, na.rm = TRUE),
    prob_control = (sum(R < 1) / .N),
    mean_window = mean(window),
    sd_window = sd(window),
    mean_crps = mean(crps),
    sd_crps = sd(crps)),
    by = .(type, date, rt_type)
    ][, R0_range := purrr::pmap(
      list(mean, bottom, top, lower, upper),
      function(mean, bottom, top, lower, upper) {
        list(point = mean,
             lower = bottom,
             upper = top,
             mid_lower = lower,
             mid_upper = upper)
      }),]


  R0_estimates_sum <- data.table::setorder(R0_estimates_sum, date)

  if (verbose) {
    message("Summarising forecast cases")
  }

  cases_forecast <- purrr::map(estimates, ~ .$cases)

  rm(estimates)

  if (any(purrr::map_lgl(cases_forecast, ~ !is.null(.)))) {

    ## Clean up case forecasts
    cases_forecast <-  data.table::rbindlist(cases_forecast)

    ## Summarise case forecasts
    sum_cases_forecast <- data.table::copy(cases_forecast)[, .(
      bottom  = return_hdi(cases, 0.9, 1),
      top = return_hdi(cases, 0.9, 2),
      lower  = return_hdi(cases, 0.5, 1),
      upper = return_hdi(cases, 0.5, 2),
      median = as.numeric(median(cases, na.rm = TRUE)),
      mean = as.numeric(mean(cases, na.rm = TRUE)),
      std = as.numeric(sd(cases, na.rm = TRUE))),
      by = .(type, date, rt_type)
      ][, range := purrr::pmap(
        list(mean, bottom, top),
        function(mean, bottom, top) {
          list(point = mean,
               lower = bottom,
               upper = top)
        }),]

    sum_cases_forecast <- data.table::setorder(sum_cases_forecast, date)
  }

  ## Estimate time-varying little r
  if (verbose) {
    message("Estimate time-varying rate of growth")
  }

  if (!is.null(min_est_date)) {
    nowcast <- nowcast[date >= (min_est_date - lubridate::days(rate_window))]
  }

  ## Sum across cases and imports
  nowcast <- nowcast[, .(cases = sum(cases, na.rm = TRUE)),
                     by = c("type", "sample", "date")]
  nowcast <- na.omit(nowcast)


  ## Nest by type and sample then split by type only
  nowcast <- nowcast[, .(data = list(data.table::data.table(date, cases))),
                          by = c("type", "sample")]

  ## Make results storage
  little_r <- data.table::data.table(type = unique(nowcast$type))

  ## Break nowcast into list
  nowcast <- split(nowcast, by = "type")

  ## Estimate overall
  little_r <- little_r[, overall_little_r := future.apply::future_lapply(nowcast,
                                  function(est){EpiNow::estimate_r_in_window(est$data)},
                                  future.scheduling = 10,
                                  future.packages = c("EpiNow"))]

  ## Estimate time-varying
  little_r <- little_r[, time_varying_r := future.apply::future_lapply(nowcast,
                                                                       function(est){EpiNow::estimate_time_varying_r(est$data)},
                                                                       future.scheduling = 10,
                                                                       future.packages = c("EpiNow"))]


  out <- list(R0_estimates_sum, little_r, R0_estimates)
  names(out) <- c("R0", "rate_of_spread", "raw_R0")

  if (any(purrr::map_lgl(cases_forecast, ~ !is.null(.)))) {

    out$case_forecast <- sum_cases_forecast
    out$raw_case_forecast <- cases_forecast

  }
  
  return(out)
}

