#' Real-time Pipeline
#'
#' @description Combine fitting a delay distribution, constructing a set of
#' complete sampled linelists, nowcast cases by onset date, and estimate
#' the time-varying effective reproduction number and rate of spread.
#' @param linelist A dataframe of of cases (by row) containing the following variables:
#' `import_status` (values "local" and "imported"), `date_onset`, `date_confirm`, `report_delay`.
#' @param delay_cutoff_date Character string, in the form "2020-01-01". Cutoff date to use
#' to estimate the delay distribution.
#' @param earliest_allowed_onset A character string in the form of a date ("2020-01-01") indiciating the earliest
#' allowed onset.
#' @param approx_threshold Numeric, defaults to 10,000. Threshold of cases below which explicit sampling of onsets
#' always occurs.
#' @param generation_times A matrix with columns representing samples and rows representing the probability of the serial intervel being on
#' that day. Defaults to `EpiNow::covid_generation_times`.
#' @param min_forecast_cases Numeric, defaults to 200. The minimum number of cases required in the last 7 days
#' of data in order for a forecast to be run. This prevents spurious forecasts based on highly uncertain Rt estimates.
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @param dt_threads Numeric, the number of data.table threads to use. Set internally to avoid issue when running in parallel.
#' Defaults to 1 thread.
#' @param verbose Logical, defaults to `FALSE`. Should internal nowcasting progress messages be returned.
#' @return NULL
#' @export
#' @inheritParams epi_measures_pipeline
#' @inheritParams report_nowcast
#' @inheritParams plot_pipeline
#' @inheritParams nowcast_pipeline
#' @importFrom data.table as.data.table
#' @importFrom lubridate days
#' 
#' @examples
#' \dontrun{
#' ## Save everything to a temporary directory 
#' ## Change this to inspect locally
#' target_dir <- tempdir()
#' 
#' ## Construct example distributions
#' ## reporting delay dist
#' delay_dist <- suppressWarnings(
#'                EpiNow::get_dist_def(rexp(25, 1/10), 
#'                                     samples = 5, bootstraps = 1))
#' 
#' ## Uses example case vector from EpiSoon
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)
#' cases <- cases[, `:=`(confirm = as.integer(cases), import_status = "local")][,
#'                   cases := NULL]
#' 
#' ## Run basic nowcasting pipeline
#' rt_pipeline(cases = cases,
#'             delay_defs = delay_dist,
#'             target_date = max(cases$date),
#'             target_folder = target_dir)
#'             
#'             
#' ## Run with forecasting and approximate delay sampling
#' 
#' ## Requires additional packages:
#' library(EpiSoon)
#' library(forecastHybrid)
#' 
#' ## Runs the estimation pipeline as before but this time with a 14 day Rt and case forecast
#' ## Uses the {forecastHybrid} package to produce an unweighted
#' ## ensemble using the last 3 weeks of data
#' rt_pipeline(cases = cases,
#'             delay_defs = delay_dist,
#'             target_date = max(cases$date),
#'             approx_delay = TRUE,
#'             target_folder = target_dir,
#'             horizon = 14, 
#'             report_forecast = TRUE
#'             nowcast_lag = 8,
#'             forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
#'             y = y[max(1, length(y) - 21):length(y)],
#'             model_params = list(models = "aefz", weights = "equal"),
#'             forecast_params = list(PI.combination = "mean"), ...)})
#' 
#' }
rt_pipeline <- function(cases = NULL, linelist = NULL,
                        delay_defs = NULL, incubation_defs = NULL,
                        delay_cutoff_date = NULL, rt_samples = 5, rt_windows = 1:7, 
                        rate_window = 7, earliest_allowed_onset = NULL, merge_actual_onsets = TRUE, 
                        approx_delay = FALSE,  approx_threshold = 10000, max_delay = 120, 
                        generation_times = NULL, rt_prior = NULL, nowcast_lag = 8,
                        forecast_model = NULL, horizon = 0, report_forecast = FALSE,  
                        onset_modifier = NULL, min_forecast_cases = 200, 
                        target_folder = NULL, target_date = NULL, max_upscale = 5,
                        dt_threads = 1, verbose = FALSE) {
 
 
 # Convert input to DT -----------------------------------------------------
  suppressMessages(data.table::setDTthreads(threads = dt_threads))
  cases <- data.table::as.data.table(cases)
  if (!is.null(linelist)) {
    linelist <- data.table::as.data.table(linelist)
  }

# Make sure incubation and delays have the same number of samples ---------

  if (is.null(incubation_defs)) {
    if (verbose) {
      message("Using default incubation period based on of:", 
              EpiNow::covid_incubation_period[1, ]$as_reported)
    }
    incubation_defs <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
                                                mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
                                                sd = EpiNow::covid_incubation_period[1, ]$sd,
                                                sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
                                                max_value = 30, samples = nrow(delay_defs))
  }

 # Set up folders ----------------------------------------------------------

 latest_folder <- file.path(target_folder, "latest")
 target_folder <- file.path(target_folder, target_date)
 
  if (!dir.exists(target_folder)) {
    dir.create(target_folder, recursive = TRUE)
  }
 
 # Default input -----------------------------------------------------------

  if (is.null(generation_times)) {
    if (verbose) {
      message("Using default sample of generation times with mean (sd) of 3.6 (3.1)")
    }
    generation_times <- EpiNow::covid_generation_times
  }


  if (is.null(rt_prior)) {
    if (verbose) {
      message("Using default Rt prior of 2.6 (2)")
    }
    rt_prior <- list(
      mean_prior = 2.6,
      std_prior = 2)
  }
 
 
# Control errors by changing options --------------------------------------

 ##Define the minimum number of recent cases required for a forecast to be run
 if (!is.null(min_forecast_cases)) {
   current_cases <- data.table::copy(cases)[date <= max(date)][
     date >= (max(date) - lubridate::days(7))
   ][, .(cases = sum(confirm, na.rm = TRUE))]$cases

   
   ## If cases in the last week are fewer than this number then turn off forecasting.
   if (min_forecast_cases > current_cases) {
     horizon <- 0
     report_forecast <- FALSE
     forecast_model <- NULL
   }
 }
 

# Make sure the horizon is as specified from the target date --------------

 if (horizon != 0 & !is.null(forecast_model)) {
   horizon <- horizon + as.numeric(as.Date(target_date) - max(cases$date))
 } 

# Adaptive approximate delay ----------------------------------------------

 if (approx_delay) {
   total_cases <- data.table::copy(cases)[date <= max(date)][,
                        .(cases = sum(confirm, na.rm = TRUE))]$cases
   
   if (total_cases <= approx_threshold) {
     approx_delay <- FALSE
     if (verbose){
       message("Explicitly sampling delays as case count is below thresold (", approx_threshold, ")
               Use approx_thresold to alter this behaviour")
     }
   }
 }
 
 ## Define the min plotting (and estimate date as the first date that
 ## at least 5 local cases were reported
 min_plot_date <- data.table::copy(cases)[
   import_status %in% "local"][confirm >= 5][
     ,.(date = min(date, na.rm = TRUE))]$date
 
  # Format input ------------------------------------------------------------

 if (!is.null(linelist)) {
   ## Reformat linelist for use in nowcast_pipeline
   linelist <- linelist[, .(date_onset_symptoms = date_onset, 
                            date_confirmation = date_confirm,
                            delay_confirmation = report_delay,
                            import_status)]
 }else{
   merge_actual_onsets <- FALSE
 }
 
  # Run a nowcast -----------------------------------------------------------

  nowcast <- EpiNow::nowcast_pipeline(
    reported_cases = cases, 
    linelist = linelist,
    target_date = target_date, 
    earliest_allowed_onset = earliest_allowed_onset,
    merge_actual_onsets = merge_actual_onsets,
    nowcast_lag = nowcast_lag, 
    verbose = verbose,
    delay_defs = delay_defs,
    incubation_defs = incubation_defs,
    onset_modifier = onset_modifier, 
    approx_delay = approx_delay,
    max_delay = max_delay)
 
# Report nowcast estimates ------------------------------------------------
  EpiNow::report_nowcast(nowcast, cases,
                         target_folder = target_folder,
                         target = "infection_upscaled")
  
  saveRDS(nowcast,  paste0(target_folder, "/nowcast.rds"))
  saveRDS(delay_defs, paste0(target_folder, "/delays.rds"))
  saveRDS(incubation_defs, paste0(target_folder, "/incubation.rds"))
  
  # Estimate time-varying parameters ----------------------------------------
  epi_estimates <-
    EpiNow::epi_measures_pipeline(
          nowcast = nowcast[type == "infection_upscaled"][, type := "nowcast"],
          min_est_date = min_plot_date,
          generation_times = generation_times,
          rt_samples = rt_samples,
          rate_window = rate_window, rt_windows = rt_windows,
          rt_prior = rt_prior, forecast_model = forecast_model,
          horizon = horizon, verbose = verbose)

  saveRDS(epi_estimates,  paste0(target_folder, "/time_varying_params.rds"))
  saveRDS(epi_estimates$case_forecast, paste0(target_folder, "/case_forecast.rds"))
  saveRDS(epi_estimates$R0, paste0(target_folder, "/summarised_reff.rds"))
  saveRDS(epi_estimates$rate_of_spread, paste0(target_folder, "/summarised_littler.rds"))

  

# Report cases ------------------------------------------------------------

  cases_by_report <- report_cases(nowcast,
                                  case_forecast = epi_estimates$raw_case_forecast,
                                  delay_defs = delay_defs,
                                  incubation_defs = incubation_defs,
                                  type = "median")
  
  saveRDS(cases_by_report, paste0(target_folder, "/cases_by_report.rds"))
  
  ## Remove everything except folder and reporting arguments
  rm(list = setdiff(ls(), c("target_folder", "target_date", "min_plot_date",
                  "report_forecast", "latest_folder")))

# Report estimates --------------------------------------------------------

  EpiNow::report_reff(target_folder)  

  EpiNow::report_littler(target_folder)

 # Summarise  -------------------------------------------------------

  EpiNow::report_summary(target_folder)

 # Plot --------------------------------------------------------------------

   EpiNow::plot_pipeline(target_folder = target_folder,
                         target_date = target_date,
                         min_plot_date = min_plot_date,
                         report_forecast = report_forecast)

 # Copy all results to latest folder ---------------------------------------
  
  ## Save all results to a latest folder as well
  suppressWarnings(
    if (dir.exists(latest_folder)) {
      unlink(latest_folder)
    })
    
  suppressWarnings(
    dir.create(latest_folder)
  )

  suppressWarnings(
    file.copy(file.path(target_folder, "."),
              latest_folder, recursive = TRUE)
  )
  
  return(invisible(NULL))
}
