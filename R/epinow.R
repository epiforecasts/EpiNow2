#' Real-time Rt Estimation, Forecasting and Reporting
#'
#' @description Estimate Rt and cases by date of infection, forecast into the future, transform to date 
#' of report and then save summary measures and plots.
#' @param target_date Date, defaults to maximum found in the data if not specified.
#' @param target_folder Character string specifying where to save results (will create if not present).
#' @param return_estimates Logical, defaults to TRUE. Should estimates be returned.
#' @return A list of output from estimate_infections, forecast_infections,  report_cases, and report_summary.
#' @export
#' @inheritParams estimate_infections
#' @inheritParams forecast_infections
#' @importFrom data.table as.data.table
#' @importFrom lubridate days
#' 
#' @examples
#' \dontrun{
#' ## optional additional packages:
#' ## remove forecast_model argument to no longer require these
#' library(EpiSoon)
#' library(forecastHybrid)
#' 
#' ## Construct example distributions
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#'                           
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
#' ## Set max allowed delay to 60 days to truncate computation
#' reporting_delay$max <- 60
#' 
#' ## Uses example case vector from EpiSoon
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)
#' cases <- cases[, confirm := as.integer(cases)][,cases := NULL][1:40]
#' 
#' ## Report Rt along with forecasts
#' out <- epinow(reported_cases = cases, generation_time = generation_time,
#'               incubation_period = incubation_period, reporting_delay = reporting_delay,
#'               rt_prior = list(mean = 1, sd = 1),
#'               forecast_model = function(y, ...){
#'                    EpiSoon::forecastHybrid_model(
#'                      y = y[max(1, length(y) - 21):length(y)],
#'                      model_params = list(models = "aefz", weights = "equal"),
#'                      forecast_params = list(PI.combination = "mean"), ...)},
#'                samples = 1000, warmup = 500, cores = 2, chains = 2,
#'                verbose = TRUE, return_fit = TRUE)
#' 
#' out
#' }
epinow <- function(reported_cases, family = "negbin",
                   generation_time = generation_time,
                   incubation_period = incubation_period,
                   reporting_delay = reporting_delay,
                   rt_prior = rt_prior,
                   model = model,
                   cores = 2, chains = 2,
                   samples = 2000, warmup = 500,
                   estimate_rt = TRUE, return_fit = FALSE,
                   forecast_model, horizon = 14,
                   ensemble_type = "mean",
                   return_estimates = TRUE,
                   target_folder, target_date,
                   verbose = FALSE) {
 
 if (!return_estimates & missing(target_folder)) {
   stop("Either return estimates or save to a target folder")
 }
  
 # Convert input to DT -----------------------------------------------------
  suppressMessages(data.table::setDTthreads(threads = cores))
 
 # Set up folders ----------------------------------------------------------

  if (missing(target_date)) {
    target_date <- max(reported_cases$date)
  }
  
  if (!missing(target_folder)) {
    latest_folder <- file.path(target_folder, "latest")
    target_folder <- file.path(target_folder, target_date)
    
    if (!dir.exists(target_folder)) {
      dir.create(target_folder, recursive = TRUE)
    }
  }else{
    target_folder <- NULL
  }
  
# Make sure the horizon is as specified from the target date --------------

 if (horizon != 0 & !missing(forecast_model)) {
   horizon <- horizon + as.numeric(as.Date(target_date) - max(cases$date))
 } 


# Save input data ---------------------------------------------------------

if (!is.null(target_folder)) {
  latest_date <- reported_cases[confirm > 0][date == max(date)]$date
  
  saveRDS(latest_date, paste0(target_folder, "/latest_date.rds"))
  saveRDS(reported_cases, paste0(target_folder, "/reported_cases.rds"))
}  

# Estimate infections and Reproduction no ---------------------------------

   estimates <- estimate_infections(reported_cases = reported_cases,
                                    family = family,
                                    generation_time = generation_time,
                                    incubation_period = incubation_period,
                                    reporting_delay = reporting_delay,
                                    rt_prior = rt_prior,
                                    model = model,
                                    cores = cores, chains = chains,
                                    samples = ceiling(samples / chains),
                                    warmup = warmup,
                                    estimate_rt = estimate_rt,
                                    horizon = horizon,
                                    verbose = verbose, return_fit = return_fit) 
 
# Report estimates --------------------------------------------------------
  if (!is.null(target_folder)) {
    saveRDS(estimates$samples,  paste0(target_folder, "/estimate_samples.rds"))
    saveRDS(estimates$summarised,  paste0(target_folder, "/summarised_estimates.rds"))
    
    if (return_fit){
      saveRDS(estimates$fit, paste0(target_folder, "/model_fit.rds"))
    }
 }
# Forecast infections and reproduction number -----------------------------
if (!missing(forecast_model)) {
  forecast <- forecast_infections(infections = estimates$summarised[variable == "infections"][type != "forecast"][, type := NULL],
                                  rts = estimates$summarised[variable == "R"][type != "forecast"][, type := NULL],
                                  gt_mean = estimates$summarised[variable == "gt_mean"]$mean,
                                  gt_sd = estimates$summarised[variable == "gt_sd"]$mean,
                                  gt_max = generation_time$max,
                                  forecast_model = forecast_model,
                                  ensemble_type = ensemble_type,
                                  horizon = horizon,
                                  samples = samples)
}
# Report cases ------------------------------------------------------------
if (!missing(forecast_model) & !is.null(target_folder)) {
  saveRDS(forecast$samples,  paste0(target_folder, "/forecast_samples.rds"))
  saveRDS(forecast$summarised,  paste0(target_folder, "/summarised_forecast.rds"))
}
# Report forcasts ---------------------------------------------------------

if (missing(forecast_model)) {
  estimated_reported_cases <- report_cases(case_estimates = estimates$samples[variable == "infections"][type != "forecast"][,
                                                                              .(date, sample, cases = value)],
                                           reporting_delay = reporting_delay,
                                           incubation_period =  incubation_period,
                                           type = "sample")
}else{
  report_cases_with_forecast <- function(model) {
    reported_cases <- report_cases(case_estimates = estimates$samples[variable == "infections"][type != "forecast"][,
                                                                      .(date, sample, cases = value)],
                                   case_forecast = forecast$samples[type == "case" & 
                                                                    forecast_type == model][,
                                                                    .(date, sample, cases = value)],
                                   reporting_delay = reporting_delay,
                                   incubation_period =  incubation_period,
                                   type = "sample")
    return(reported_cases)
  }
  
  reported_cases_rt <- report_cases_with_forecast(model = "rt")
  reported_cases_cases <- report_cases_with_forecast(model = "case")
  reported_cases_ensemble <- report_cases_with_forecast(model = "ensemble")
  
  estimated_reported_cases <- list()
  
  estimated_reported_cases$samples <- data.table::rbindlist(list(
    reported_cases_rt$samples[,type := "rt"],
    reported_cases_cases$samples[,type := "case"],
    reported_cases_ensemble$samples[,type := "ensemble"],
    estimates$samples[variable == "reported_cases"][,
                      .(date, sample, cases = value, type = "gp_case")],
    estimates$samples[variable == "reported_cases_rt"][,
                      .(date, sample, cases = value, type = "gp_rt")]
  ), use.names = TRUE)
  
  estimated_reported_cases$summarised <- data.table::rbindlist(list(
    reported_cases_rt$summarised[,type := "rt"],
    reported_cases_cases$summarised[,type := "case"],
    reported_cases_ensemble$summarised[,type := "ensemble"],
    estimates$summarised[variable == "reported_cases"][, type := "gp_case"][,
                         variable := NULL][, strat := NULL],
    estimates$summarised[variable == "reported_cases_rt"][, type := "gp_rt"][,
                        variable := NULL][, strat := NULL]
  ), use.names = TRUE)
}
  
if (!is.null(target_folder)){
  saveRDS(estimated_reported_cases$samples, paste0(target_folder, "/estimated_reported_cases_samples.rds"))
  saveRDS(estimated_reported_cases$summarised, paste0(target_folder, "/summarised_estimated_reported_cases.rds"))
} 
   
# # Report estimates --------------------------------------------------------

   summary <- report_summary(
     summarised_estimates = estimates$summarised[!is.na(date)][type != "forecast"][date == max(date)],
     rt_samples = estimates$samples[variable == "R"][type != "forecast"][date == max(date), .(sample, value)])

   
   if(!is.null(target_folder)) {
     saveRDS(summary, paste0(target_folder, "/summary.rds"))
   }
   
#  # Plot --------------------------------------------------------------------
# 
#    EpiNow2::plot_pipeline(target_folder = target_folder,
#                          target_date = target_date,
#                          report_forecast = report_forecast)

 # Copy all results to latest folder ---------------------------------------
  if (!is.null(target_folder)) {  
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
  }
   
   if (return_estimates) {
     out <- list()
     out$estimates <- estimates
     
     if (!missing(forecast_model)) {
       out$forecast <- forecast
     }
     
     out$estimated_reported_cases <- estimated_reported_cases
     out$summary <- summary
     return(out)
   }else{
     return(invisible(NULL))
   }
}


#' Real-time Rt Estimation, Forecasting and Reporting by Region
#'
#' @description Estimates Rt by region. See the documentation for `epinow` for further information.
#' @param reported_cases A data frame of confirmed cases (confirm) by date (date), and region (`region`).
#' @param case_limit Numeric, the minimum number of cases in a region required for that region to be evaluated. Defaults to 20.
#' @param ... Pass additional arguments to `epinow`
#' @inheritParams epinow
#' @return 
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom data.table as.data.table setDT copy setorder
#' @importFrom purrr safely map
#' @examples
#'  \dontrun{
#' ## Requires additional packages:
#' library(EpiSoon)
#' library(forecastHybrid)
#' 
#' ## Construct example distributions
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#'                           
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- list(mean = log(10),
#'                         mean_sd = 0.8,
#'                         sd = log(2),
#'                         sd_sd = 0.1,
#'                         max = 30)
#'                         
#' ## Uses example case vector from EpiSoon
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)
#' cases <- cases[, `:=`(confirm = as.integer(cases))][,
#'                   cases := NULL]
#' 
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' ## Run basic nowcasting pipeline
#' out <- regional_epinow(reported_cases = cases,
#'                        generation_time = generation_time,
#'                        incubation_period = incubation_period,
#'                        reporting_delay = reporting_delay,
#'                        forecast_model = function(y, ...){
#'                          EpiSoon::forecastHybrid_model(
#'                             y = y[max(1, length(y) - 21):length(y)],
#'                             model_params = list(models = "aefz", weights = "equal"),
#'                             forecast_params = list(PI.combination = "mean"), ...)},
#'                        samples = 1000, warmup = 500, cores = 2, chains = 2,
#'                        verbose = TRUE)
#'}
regional_epinow <- function(reported_cases, 
                            target_folder, target_date,
                            case_limit = 20, cores = 1,
                            return_estimates = TRUE,
                            ...) {
   
  ## Set input to data.table
  cases <- data.table::as.data.table(cases)
  
  if (missing(target_date)) {
    target_date <- as.character(max(reported_cases$date))
  }
  
  if (missing(target_folder)) {
    target_folder <- NULL
  }
  
  message("Reporting estimates using data up to: ", target_date)
  
  
  ## Check for regions more than required cases
  eval_regions <- data.table::copy(reported_cases)[,.(confirm = sum(confirm, na.rm = TRUE)), 
                                                   by = c("region", "date")][
                                                     confirm >= case_limit]$region
  
  eval_regions <- unique(eval_regions)
  
  ## Exclude zero regions
  reported_cases <- reported_cases[!is.na(region)][region %in% eval_regions]
  
  message("Running the pipeline for: ",
          paste(eval_regions, collapse = ", "))
  
  ## regional pipelines
  regions <- unique(reported_cases$region)
  
  ## Function to run the pipeline in a region
  run_region <- function(target_region, 
                         reported_cases,
                         ...) { 
    message("Reporting estimates for: ", target_region)
    data.table::setDTthreads(threads = cores)
    
    if (!is.null(target_folder)) {
      target_folder <- file.path(target_folder, target_region)
    }
    
    regional_cases <- reported_cases[region %in% target_region][, region := NULL]
    
    out <- EpiNow2::epinow(
      reported_cases = regional_cases,
      target_folder = target_folder,
      target_date = target_date, 
      return_estimates = return_estimates,
      ...)
    
    if (return_estimates) {
      return(out)
    }else{
      return(invisible(NULL))
    }}
  
  safe_run_region <- purrr::safely(run_region)
  
  ## Run regions (make parallel using future::plan)
  out <- future.apply::future_lapply(regions, safe_run_region,
                                     reported_cases = reported_cases,
                                     ...,
                                     future.scheduling = Inf)
  
  
  if (return_estimates) {
    out <- purrr::map(out, ~ .$result)
    
    names(out) <- regions
    return(out)
  }else{
    return(invisible(NULL))
  }
}