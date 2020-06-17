#' Draw with an offset from a negative binomial distribution
#'
#' @description Samples size (the number of trials) of a binomial distribution
#'  copied from https://github.com/sbfnk/bpmodels/blob/master/R/utils.r
#' @param n Numeric, number of samples to draw
#' @param x Numeric, offset.
#' @param prob Numeric, probability of successful trial
#' @param max_upscale Numeric, maximum upscaling of cases allowed at each time point
#' @export
#' @examples
#' x <- c(0:10)
#' prob <- pgamma(1:11, 4, 2)
#'
#' rbinom_size(length(x), x, prob, max_upscale = 10)
#'
rbinom_size <- function(n, x, prob, max_upscale) {
  y <- ifelse(is.na(x), 0, x + stats::rnbinom(n, x + 1, prob))
  
  if (!missing(max_upscale)) {
    y <- ifelse(y > max_upscale * (x + 1),  max_upscale * (x + 1), y)
  }
  
  return(y)
}


#' Adjust Case Counts for Truncation
#'
#' @param cases Numeric vector of cases
#' @param cum_freq Numeric vector of cumulative frequencies
#' @param confidence_adjustment Numeric vector of frequencies used to adjust confidence
#' @param dates Character vector of dates
#' @param samples Numeric, number of samples to take
#' @return A `data.table` adjusted for truncation
#' @export
#' @inheritParams rbinom_size
#' @importFrom purrr map
#' @importFrom data.table data.table
adjust_for_truncation <- function(cases, cum_freq, dates, 
                                  confidence_adjustment = NULL,
                                  samples, max_upscale) {
  
  
  out <- purrr::map(seq_len(samples), function(sample) {
    
    ## Sample cases
    x_cases <- cases
    x_cases[length(cases):(length(cases) - (length(cum_freq) - 1))] <-
      EpiNow::rbinom_size(
        length(cum_freq),
        cases[length(cases):(length(cases) - (length(cum_freq) - 1))],
        cum_freq, max_upscale = max_upscale)
    
    ## Add confidence based on the cumulative frequency
    confidence <- rep(1, length(x_cases))
    
    conf_meas <- cum_freq
    
    if (!is.null(confidence_adjustment)){
      if (length(conf_meas) > length(confidence_adjustment)){
        confidence_adjustment <- c(confidence_adjustment,
                                   rep(1, (length(conf_meas) - length(confidence_adjustment))))
      }
      
      if (length(conf_meas) < length(confidence_adjustment)){
        conf_meas <- c(conf_meas, 
                       rep(1, length(confidence_adjustment) - length(conf_meas)))
      }
      conf_meas <- conf_meas * confidence_adjustment
    } 
    
    confidence[length(confidence):max(1, ((length(confidence) - (length(conf_meas) - 1))))] <- 
      conf_meas[1:min(length(confidence), length(conf_meas))]
    
    return(data.table::data.table(date = dates,
                                  cases = x_cases,
                                  confidence = confidence))
  })
  
  return(out)
}




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
#' delay_def <- EpiNow::lognorm_dist_def(mean = 5, 
#'                                       mean_sd = 1,
#'                                       sd = 3,
#'                                       sd_sd = 1,
#'                                       max_value = 30,
#'                                       samples = 1,
#'                                       to_log = TRUE)
#'                                        
#' ## Define a single incubation period
#' incubation_def <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
#'                                            mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
#'                                            sd = EpiNow::covid_incubation_period[1, ]$sd,
#'                                            sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
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
    EpiNow::dist_skel(n = n, 
                      model = delay_def$model[[1]], 
                      params = delay_def$params[[1]],
                      max_value = delay_def$max_value[[1]], 
                      ...)
  }
  
  ## Define an incubation fn
  sample_incubation_fn <- function(n, ...) {
    EpiNow::dist_skel(n = n, 
                      model = incubation_def$model[[1]], 
                      params = incubation_def$params[[1]],
                      max_value = incubation_def$max_value[[1]], 
                      ...)
  }

  
  ## Infection to onset
  onset <- EpiNow::sample_approx_dist(cases = infections, 
                                      dist_fn = sample_incubation_fn,
                                      max_value = incubation_def$max_value,
                                      direction = "forwards",
                                      type = type,
                                      truncate_future = FALSE)
  
  ## Onset to report
  report <- EpiNow::sample_approx_dist(cases = onset, 
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
