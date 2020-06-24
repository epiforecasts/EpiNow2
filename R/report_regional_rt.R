#' Real-time Rt Estimation, Forecasting and Reporting by Region
#'
#' @description Estimates Rt by region. See the documentation for `report_rt` for further information.
#' @param reported_cases A data frame of confirmed cases (confirm) by date (date), and region (`region`).
#' @param case_limit Numeric, the minimum number of cases in a region required for that region to be evaluated. Defaults to 20.
#' @param ... Pass additional arguments to `report_rt`
#' @inheritParams report_rt
#' @return 
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom data.table as.data.table setDT copy setorder
#' @importFrom purrr safely
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
#' out <- report_regional_rt(reported_cases = cases,
#'                           generation_time = generation_time,
#'                           incubation_period = incubation_period,
#'                           reporting_delay = reporting_delay,
#'                           forecast_model = function(y, ...){
#'                               EpiSoon::forecastHybrid_model(
#'                                  y = y[max(1, length(y) - 21):length(y)],
#'                                  model_params = list(models = "aefz", weights = "equal"),
#'                                  forecast_params = list(PI.combination = "mean"), ...)},
#'                                  samples = 1000, warmup = 500, cores = 2, chains = 2,
#'                           verbose = TRUE)
#'}
report_regional_rt <- function(reported_cases, 
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
    
    out <- EpiNow2::report_rt(
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
    names(out) <- regions
    return(out)
  }else{
    return(invisible(NULL))
  }
}