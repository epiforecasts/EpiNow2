#' Simulate Cases by Date of Infection, Onset and Report
#'
#' @param initial_cases Integer, initial number of cases.
#' @param initial_date Date, (i.e `as.Date("2020-02-01")`). Starting date of the simulation.
#' @param generation_interval Numeric vector describing the generation interval probability density
#' @inheritParams EpiSoon::predict_cases
#' @inheritParams adjust_infection_to_report
#' @return A dataframe containing three variables: `date`, `cases` and `reference`.
#' @export
#' @importFrom data.table data.table setDT rbindlist
#' @importFrom lubridate days
#' @importFrom EpiSoon predict_cases
#' @examples
#' ## Define an initial rt vector 
#' rts <- c(rep(2, 20), (2 - 1:15 * 0.1), rep(0.5, 10))
#' rts
#' ## Use the mean default generation interval for covid
#' generation_interval <- rowMeans(EpiNow::covid_generation_times)
#' 
#' ## Sample a report delay as a lognormal
#' delay_def <- EpiNow::lognorm_dist_def(mean = 5, mean_sd = 1,
#'                                       sd = 3, sd_sd = 1, max_value = 30,
#'                                       samples = 1, to_log = TRUE)
#'                                       
#' 
#' ## Sample a incubation period (again using the default for covid)
#' incubation_def <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
#'                                           mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
#'                                           sd = EpiNow::covid_incubation_period[1, ]$sd,
#'                                           sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
#'                                           max_value = 30, samples = 1)
#'
#' ## Simulate cases with a decrease in reporting at weekends and an increase on Monday                                     
#' simulated_cases <- simulate_cases(rts, initial_cases = 100 , initial_date = as.Date("2020-03-01"),
#'                     generation_interval = generation_interval, delay_def = delay_def, 
#'                    incubation_def = incubation_def, 
#'                    reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95))
#'                    
#'print(simulated_cases)
#'
#'
#'
#' ## Simulate cases with a weekly reporting effect and stochastic noise in reporting (beyond the delay)                                  
#' simulated_cases <- simulate_cases(rts, initial_cases = 100 , initial_date = as.Date("2020-03-01"),
#'                     generation_interval = generation_interval, delay_def = delay_def, 
#'                    incubation_def = incubation_def, 
#'                    reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95),
#'                    reporting_model = function(n) {
#'                       out <- suppressWarnings(rnbinom(length(n), as.integer(n), 0.5))
#'                       out <- ifelse(is.na(out), 0, out)
#'                       })
#'                    
#'print(simulated_cases)
simulate_cases <- function(rts, initial_cases, initial_date, generation_interval,
                           rdist = rpois, delay_def, incubation_def, reporting_effect, 
                           reporting_model, truncate_future = TRUE,
                           type = "sample") {
  
  
  ## Simulating cases initialising a data.table
  cases <- data.table::data.table(
    date = initial_date,
    cases = initial_cases)
  
  ## Structuring rts as a data.table with dates
  rts <- data.table::data.table(
    date = seq(initial_date, initial_date + lubridate::days(length(rts) - 1), by = "days"),
    rt = rts
  )
  
  ##  Return a dataframe of cases by date of infection
  simulated_cases <- data.table::setDT(
                     EpiSoon::predict_cases(cases = cases,
                                            rt = rts,
                                            serial_interval = generation_interval,
                                            rdist = rdist)
                     )
  
  ## Mapping with a weekly reporting effect
  report <- EpiNow::adjust_infection_to_report(simulated_cases,
                                               delay_def = delay_def,
                                               incubation_def = incubation_def, 
                                               reporting_effect = reporting_effect,
                                               reporting_model = reporting_model,
                                               type = type, return_onset = TRUE,
                                               truncate_future = truncate_future)
  
  
  ## Bind in simulated cases with reported cases
  report <- data.table::rbindlist(list(
    simulated_cases[, reference := "infection"],
    report
  ))
  
  ## Get the median interval to truncate initial cases
  median_interval <- sum(!(cumsum(generation_interval) > 0.5)) + 1
  
  ## Truncate initial data be length of the median generation interval
  report[date >= initial_date + lubridate::days(median_interval)]
  
  return(report)
}
