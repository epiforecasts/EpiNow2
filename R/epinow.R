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
#' @importFrom data.table setDT
#' @importFrom lubridate days
#' 
#' @examples
#' \donttest{
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
#' ## Example case data
#' reported_cases <- EpiNow2::example_confirmed[1:40] 
#' 
#' ## Report Rt along with forecasts
#' out <- epinow(reported_cases = reported_cases, generation_time = generation_time,
#'               delays = list(incubation_period, reporting_delay),
#'               rt_prior = list(mean = 1, sd = 1),
#'               samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1), chains = 4,
#'               verbose = TRUE, return_fit = TRUE)
#' 
#' out
#' 
#' ## For optional forecasting
#' if(requireNamespace("EpiSoon")){
#'    if(requireNamespace("forecastHybrid")){
#'
#'    ## Report Rt along with forecasts
#'    out <- epinow(reported_cases = cases, generation_time = generation_time,
#'                  delays = list(incubation_period, reporting_delay),
#'                  rt_prior = list(mean = 1, sd = 1),
#'                  forecast_model = function(y, ...){
#'                    EpiSoon::forecastHybrid_model(
#'                      y = y[max(1, length(y) - 21):length(y)],
#'                      model_params = list(models = "aefz", weights = "equal"),
#'                      forecast_params = list(PI.combination = "mean"), ...)},
#'                  samples = 1000, warmup = 500, cores = ifelse(interactive(), 4, 1), chains = 4,
#'                  verbose = TRUE, return_fit = TRUE)
#' 
#' out
#'    }
#' }
#'
#' }
#'
epinow <- function(reported_cases, family = "negbin",
                   generation_time, delays,
                   gp = list(basis_prop = 0.3, boundary_scale = 2, 
                             lengthscale_mean = 0, lengthscale_sd = 2),
                   rt_prior = list(mean = 1, sd = 1), model,
                   prior_smoothing_window = 7, cores = 1, chains = 4,
                   samples = 1000, warmup = 200, adapt_delta = 0.99,  max_treedepth = 15,
                   estimate_rt = TRUE, estimate_week_eff = TRUE, estimate_breakpoints = FALSE,
                   burn_in = 0, stationary = FALSE, fixed = FALSE, return_fit = FALSE,
                   forecast_model, horizon = 7, ensemble_type = "mean", return_estimates = TRUE,
                   target_folder, target_date, verbose = TRUE, debug = FALSE) {

 if (!return_estimates & missing(target_folder)) {
   futile.logger::flog.fatal("Either return estimates or save to a target folder")
   stop("Either return estimates or save to a target folder")
 }
  
 # Convert input to DT -----------------------------------------------------
  suppressMessages(data.table::setDTthreads(threads = 1))
  reported_cases <- data.table::setDT(reported_cases)

 # Set up folders ----------------------------------------------------------

  if (missing(target_date)) {
    target_date <- max(reported_cases$date)
  }
  
  if (missing(target_folder)) {
    target_folder <- NULL
  }
  
  
  if (!is.null(target_folder)) {
    latest_folder <- file.path(target_folder, "latest")
    target_folder <- file.path(target_folder, target_date)
    
    if (!dir.exists(target_folder)) {
      dir.create(target_folder, recursive = TRUE)
    }
  }
# Make sure the horizon is as specified from the target date --------------

 if (horizon != 0) {
   horizon <- horizon + as.numeric(as.Date(target_date) - max(reported_cases$date))
 } 


# Save input data ---------------------------------------------------------

if (!is.null(target_folder)) {
  latest_date <- reported_cases[confirm > 0][date == max(date)]$date
  
  saveRDS(latest_date, paste0(target_folder, "/latest_date.rds"))
  saveRDS(reported_cases, paste0(target_folder, "/reported_cases.rds"))
}  

# Estimate infections and Reproduction no ---------------------------------

  if (missing(model)) {
    model <- NULL
  }
  
   estimates <- estimate_infections(reported_cases = reported_cases,
                                    family = family,
                                    generation_time = generation_time,
                                    delays = delays,
                                    gp =  gp,
                                    rt_prior = rt_prior,
                                    adapt_delta = adapt_delta,
                                    max_treedepth = max_treedepth,
                                    model = model,
                                    cores = cores, chains = chains,
                                    samples = samples,
                                    warmup = warmup,
                                    estimate_rt = estimate_rt,
                                    estimate_week_eff = estimate_week_eff,
                                    estimate_breakpoints = estimate_breakpoints,
                                    burn_in = burn_in, stationary = stationary, fixed = fixed,
                                    horizon = horizon,
                                    verbose = verbose, return_fit = return_fit,
                                    debug = debug) 
 
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
  estimated_reported_cases <- list()
  estimated_reported_cases$samples <- estimates$samples[variable == "reported_cases"][,
                                                        .(date, sample, cases = value, type = "gp_rt")]
  estimated_reported_cases$summarised <- estimates$summarised[variable == "reported_cases"][, 
                                                              type := "gp_rt"][, variable := NULL][, strat := NULL]
}else{
  report_cases_with_forecast <- function(model) {
    reported_cases <- report_cases(case_estimates = estimates$samples[variable == "infections"][type != "forecast"][,
                                                                      .(date, sample, cases = value)],
                                   case_forecast = forecast$samples[type == "case" & 
                                                                    forecast_type == model][,
                                                                    .(date, sample, cases = value)],
                                   delays = delays,
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
                      .(date, sample, cases = value, type = "gp_rt")]
  ), use.names = TRUE)
  
  estimated_reported_cases$summarised <- data.table::rbindlist(list(
    reported_cases_rt$summarised[,type := "rt"],
    reported_cases_cases$summarised[,type := "case"],
    reported_cases_ensemble$summarised[,type := "ensemble"],
    estimates$summarised[variable == "reported_cases"][, type := "gp_rt"][,
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

   plots <- report_plots(summarised_estimates = estimates$summarised,
                         reported = reported_cases, target_folder = target_folder)

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
     out$plots <- plots
     return(out)
   }else{
     return(invisible(NULL))
   }
}


#' Real-time Rt Estimation, Forecasting and Reporting by Region
#'
#' @description Estimates Rt by region. See the documentation for `epinow` for further information.
#' @param reported_cases A data frame of confirmed cases (confirm) by date (date), and region (`region`).
#' @param non_zero_points Numeric, the minimum number of time points with non-zero cases in a region required for
#' that region to be evaluated. Defaults to 2.
#' @param summary Logical, should summary measures be calculated.
#' @param all_regions_summary Logical, defaults to `TRUE`. Should summary plots for all regions be returned
#' rather than just regions of interest.
#' @param ... Pass additional arguments to `epinow`
#' @inheritParams epinow
#' @inheritParams regional_summary
#' @return A list of output stratified at the top level into regional output and across region output summary output
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom data.table as.data.table setDT copy setorder
#' @importFrom purrr safely map compact
#' @examples
#'  \donttest{
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
#'                         mean_sd = log(2),
#'                         sd = log(2),
#'                         sd_sd = log(1.1),
#'                         max = 30)
#'                         
#' ## Uses example case vector
#' cases <- EpiNow2::example_confirmed[1:40]
#' 
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' ## Run basic nowcasting pipeline
#' ## Here we reduce the accuracy of the GP approximation in order to reduce runtime
#' out <- regional_epinow(reported_cases = cases,
#'                        generation_time = generation_time,
#'                        delays = list(incubation_period, reporting_delay),
#'                        adapt_delta = 0.9,
#'                        samples = 2000, warmup = 200,
#'                        cores = ifelse(interactive(), 4, 1), chains = 4)
#'}
regional_epinow <- function(reported_cases, 
                            target_folder, target_date,
                            non_zero_points = 2, cores = 1,
                            summary = TRUE,
                            summary_dir,
                            region_scale = "Region",
                            all_regions_summary = TRUE,
                            return_estimates = TRUE,
                            ...) {
    
  ## Set input to data.table
  reported_cases <- data.table::setDT(reported_cases)
  
  if (missing(target_date)) {
    target_date <- as.character(max(reported_cases$date))
  }
  
  if (missing(target_folder)) {
    target_folder <- NULL
  }
  
  futile.logger::flog.info("Reporting estimates using data up to: %s", target_date)
  
  
  ## Check for regions more than required time points with cases
  eval_regions <- data.table::copy(reported_cases)[,.(confirm = confirm > 0), by = c("region", "date")][,
            .(confirm = sum(confirm, na.rm = TRUE)), by = "region"][confirm >= non_zero_points]$region
  
  eval_regions <- unique(eval_regions)
  
  ## Exclude zero regions
  reported_cases <- reported_cases[!is.na(region)][region %in% eval_regions]
  
  futile.logger::flog.info("Running the pipeline for: %s",
          paste(eval_regions, collapse = ", "))
  
  ## regional pipelines
  regions <- unique(reported_cases$region)
  
  ## Function to run the pipeline in a region
  run_region <- function(target_region, 
                         reported_cases,
                         cores = cores,
                         ...) { 
    futile.logger::flog.info("Reporting estimates for: %s", target_region)
    data.table::setDTthreads(threads = 1)

    if (!is.null(target_folder)) {
      target_folder <- file.path(target_folder, target_region)
    }
    
    regional_cases <- reported_cases[region %in% target_region][, region := NULL]
    
    out <- EpiNow2::epinow(
      reported_cases = regional_cases,
      target_folder = target_folder,
      target_date = target_date, 
      return_estimates = TRUE,
      cores = cores,
      ...)
     futile.logger::flog.info("Completed reporting estimates for: %s", target_region)

     return(out)
    }
  
  safe_run_region <- purrr::safely(run_region)
  
  ## Run regions (make parallel using future::plan)
  regional_out <- future.apply::future_lapply(regions, safe_run_region,
                                              reported_cases = reported_cases,
                                              cores = cores,
                                              ...,
                                              future.scheduling = Inf)
  
  regional_errors <- purrr::map(regional_out, ~ .$error)
  names(regional_errors) <- regions
  regional_errors <- purrr::compact(regional_errors)

  if (length(regional_errors) != 0) {
     message("Runtime errors caught: ")
     print(regional_errors)
    }

  regional_out <- purrr::map(regional_out, ~ .$result)
  names(regional_out) <- regions
  
  
  if (summary) {
    if (missing(summary_dir)) {
      summary_dir <- NULL
    }
    safe_summary <- purrr::safely(regional_summary)
    
    summary_out <- safe_summary(regional_output = regional_out,
                                summary_dir = summary_dir,
                                reported_cases = reported_cases,
                                region_scale = region_scale,
                                all_regions = all_regions_summary)

    if (!is.null(summary_out[[2]])) {
      message("Errors caught whilst generating summary statistics: ")
      print(summary_out[[2]])
      }

    summary_out <- summary_out[[1]]
  }
  
  if (return_estimates) {
    out <- list()
    out$regional <- regional_out
    
    if (summary) {
      out$summary <- summary_out
    }
    
    return(out)
  }else{
    return(invisible(NULL))
  }
}
