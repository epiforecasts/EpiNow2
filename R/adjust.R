#' Adjust from Case Counts by Infection Date to Date of Report
#'
#' @description Stochastic mapping from cases by date of infection to date of report via date of 
#' onset. Essentially reversal of `nowcast_pipeline`.
#' @param infections `data.table` containing a `date` variable and a numeric `cases` variable.
#' @param delay_def A single row data.table that defines the delay distribution (model, parameters and maximum delay for each model). 
#' See `lognorm_dist_def` for an example of the structure.
#' @param incubation_def A single row data.table that defines the incubation distribution (model, parameters and maximum delay for each model). 
#' See `lognorm_dist_def` for an example of the structure.
#' @param reporting_effect A numeric vector of length 7 that allows the scaling of reported cases
#' by the day on which they report (1 = Monday, 7 = Sunday). By default no scaling occurs.
#' @param reporting_model A function that takes a single numeric vector as an argument and returns a 
#' single numeric vector. Can be used to apply stochastic reporting effects. See the examples for details.
#' @param return_onset Logical, defaults to `FALSE`. Should cases by date of onset also be returned?
#' @return A `data.table` containing a `date` variable (date of report) and a `cases` variable. If `return_onset = TRUE` there will be 
#' a third variable `reference` which indicates what the date variable refers to. 
#' @export
#' @inheritParams sample_approx_dist
#' @importFrom data.table setorder data.table setDTthreads
#' @importFrom lubridate wday
#' @examples
#' 
#' ## Define example cases
#' cases <- data.table::as.data.table(EpiSoon::example_obs_cases) 
#' 
#' cases <- cases[, `:=`(confirm = as.integer(cases), import_status = "local")]
#' 
#' ## Define a single report delay distribution
#' delay_def <- EpiNow2::lognorm_dist_def(mean = 5, 
#'                                       mean_sd = 1,
#'                                       sd = 3,
#'                                       sd_sd = 1,
#'                                       max_value = 30,
#'                                       samples = 1,
#'                                       to_log = TRUE)
#'                                        
#' ## Define a single incubation period
#' incubation_def <- EpiNow2::lognorm_dist_def(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                                            mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                                            sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                                            sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                                            max_value = 30, samples = 1)
#'                                            
#' 
#' ## Perform a nowcast
#' nowcast <- nowcast_pipeline(reported_cases = cases, 
#'                             target_date = max(cases$date),
#'                             delay_defs = delay_def,
#'                             incubation_defs = incubation_def)
#'                             
#' 
#' infections <- nowcast[type %in% "infection_upscaled" & import_status %in% "local"]
#' infections <- infections[, `:=`(type = NULL, import_status = NULL)]
#' 
#' 
#' ## Simple mapping
#' report <- adjust_infection_to_report(infections, delay_def, incubation_def)   
#' 
#' print(report)   
#' 
#' ## Mapping with a weekly reporting effect
#' report_weekly <- adjust_infection_to_report(
#'                       infections, delay_def, incubation_def,
#'                       reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95))          
#'                              
#' print(report_weekly) 
#' 
#'## Map using a deterministic median shift for both delays
#'report_median <- adjust_infection_to_report(infections, delay_def, 
#'                                            incubation_def, type = "median")      
#'                                            
#'                                                    
#'                                                            
#' ## Map with a weekly reporting effect and stochastic reporting model
#' report_stochastic <- adjust_infection_to_report(
#'                       infections, delay_def, incubation_def,
#'                       reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95),
#'                       reporting_model = function(n) {
#'                       out <- suppressWarnings(rnbinom(length(n), as.integer(n), 0.5))
#'                       out <- ifelse(is.na(out), 0, out)
#'                       })          
#'                              
#' print(report_stochastic)         
adjust_infection_to_report <- function(infections, delay_def, incubation_def,
                                       reporting_effect, reporting_model,
                                       type = "sample",
                                       return_onset = FALSE,
                                       truncate_future = TRUE){
  
  data.table::setDTthreads(1)
  
  ## Define sample delay fn
  sample_delay_fn <- function(n, ...) {
    EpiNow2::dist_skel(n = n, 
                      model = delay_def$model[[1]], 
                      params = delay_def$params[[1]],
                      max_value = delay_def$max_value[[1]], 
                      ...)
  }
  
  ## Define an incubation fn
  sample_incubation_fn <- function(n, ...) {
    EpiNow2::dist_skel(n = n, 
                      model = incubation_def$model[[1]], 
                      params = incubation_def$params[[1]],
                      max_value = incubation_def$max_value[[1]], 
                      ...)
  }

  
  ## Infection to onset
  onset <- EpiNow2::sample_approx_dist(cases = infections, 
                                      dist_fn = sample_incubation_fn,
                                      max_value = incubation_def$max_value,
                                      direction = "forwards",
                                      type = type,
                                      truncate_future = FALSE)
  
  ## Onset to report
  report <- EpiNow2::sample_approx_dist(cases = onset, 
                                       dist_fn = sample_delay_fn,
                                       max_value = delay_def$max_value,
                                       direction = "forwards",
                                       type = type,
                                       truncate_future = FALSE)
  
  ## Add a weekly reporting effect if present
  if (!missing(reporting_effect)) {
    reporting_effect <- data.table::data.table(
      day = 1:7,
      effect = reporting_effect
    )
    
    report <- report[, day := lubridate::wday(date, week_start = 1)]
    report <- report[reporting_effect, on = "day"]
    report <- report[, cases := as.integer(cases * effect)][,
                       `:=`(effect = NULL, day = NULL)]
    
    report <- data.table::setorder(report, date)
  }
  
  if (!missing(reporting_model)) {
    report <- report[, cases := reporting_model(cases)]
  }
  
  ## Bind together onset and report
  if (return_onset) {
    report <- data.table::rbindlist(list(
      onset[, reference := "onset"],
      report[, reference := "report"]
    ))
  }
  
  ## Truncate reported cases by maximum infection date
  if (type %in% "sample" & truncate_future) {
    report <- report[date <= max(infections$date)]
  }
  
  
  return(report)
}
