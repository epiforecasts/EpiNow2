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
#' that day. Defaults to `EpiNow2::covid_generation_times`.
#' @param min_forecast_cases Numeric, defaults to 200. The minimum number of cases required in the last 7 days
#' of data in order for a forecast to be run. This prevents spurious forecasts based on highly uncertain Rt estimates.
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @param dt_threads Numeric, the number of data.table threads to use. Set internally to avoid issue when running in parallel.
#' Defaults to 1 thread.
#' @param verbose Logical, defaults to `FALSE`. Should internal nowcasting progress messages be returned.
#' @return NULL
#' @export
#' @inheritParams report_nowcast
#' @inheritParams plot_pipeline
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
#'                EpiNow2::get_dist_def(rexp(25, 1/10), 
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
#'             target_folder = target_dir,
#'             horizon = 14, 
#'             report_forecast = TRUE
#'             forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
#'             y = y[max(1, length(y) - 21):length(y)],
#'             model_params = list(models = "aefz", weights = "equal"),
#'             forecast_params = list(PI.combination = "mean"), ...)})
#' 
#' }
rt_pipeline <- function(cases = NULL,
                        generation_times = NULL, rt_prior = NULL,
                        forecast_model = NULL, horizon = 0, report_forecast = FALSE,  
                        min_forecast_cases = 200, 
                        target_folder = NULL, target_date = NULL,
                        cores = 1, verbose = FALSE) {
 
 
 # Convert input to DT -----------------------------------------------------
  suppressMessages(data.table::setDTthreads(threads = cores))
  cases <- data.table::as.data.table(cases)

 # Set up folders ----------------------------------------------------------

 latest_folder <- file.path(target_folder, "latest")
 target_folder <- file.path(target_folder, target_date)
 
  if (!dir.exists(target_folder)) {
    dir.create(target_folder, recursive = TRUE)
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

 
 ## Define the min plotting (and estimate date as the first date that
 ## at least 5 local cases were reported
 min_plot_date <- data.table::copy(cases)[
   import_status %in% "local"][confirm >= 5][
     ,.(date = min(date, na.rm = TRUE))]$date


# Estimate infections and Reproduction no ---------------------------------

   estimates <- estimate_infections(reported_cases, family = "poisson",
                                    generation_time = generation_time,
                                    incubation_period = incubation_period,
                                    reporting_delay = reporting_delay,
                                    rt_prior = rt_prior,
                                    model = model,
                                    cores = 4, chains = 4,
                                    estimate_rt = TRUE,
                                    verbose = TRUE, return_fit = TRUE) 
 
# Report estimates --------------------------------------------------------

  saveRDS(estimates$samples,  paste0(target_folder, "/estimate_samples.rds"))
  saveRDS(estimates$samples,  paste0(target_folder, "/summarised_estimates.rds"))
  

# Forecast infections and reproduction number -----------------------------

  fore
# Report cases ------------------------------------------------------------

  cases_by_report <- report_cases(out$samples[variable %in% "infections"][, variable := NULL],
                                  case_forecast = epi_estimates$raw_case_forecast,
                                  delay_defs = delay_defs,
                                  incubation_defs = incubation_defs,
                                  type = "median")
  
  saveRDS(cases_by_report, paste0(target_folder, "/cases_by_report.rds"))
  
  ## Remove everything except folder and reporting arguments
  rm(list = setdiff(ls(), c("target_folder", "target_date", "min_plot_date",
                  "report_forecast", "latest_folder")))

# Report estimates --------------------------------------------------------

  EpiNow2::report_reff(target_folder)  

  EpiNow2::report_littler(target_folder)

 # Summarise  -------------------------------------------------------

  EpiNow2::report_summary(target_folder)

 # Plot --------------------------------------------------------------------

   EpiNow2::plot_pipeline(target_folder = target_folder,
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
