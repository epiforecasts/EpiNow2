#' Simulate Cases by Date of Infection, Onset and Report
#'
#' @param rts A dataframe of containing two variables `rt` and  `date` with
#' `rt` being numeric and `date` being a date.
#' @param initial_cases Integer, initial number of cases.
#' @param initial_date Date, (i.e `as.Date("2020-02-01")`). Starting date of the simulation.
#' @param generation_interval Numeric vector describing the generation interval probability density
#' @param rdist A function to be used to sample the number of cases. Must take two
#' arguments with the first specifying the number of samples and the second the mean. Defaults
#' to `rpois` if not supplied
#' @inheritParams adjust_infection_to_report
#' @return A dataframe containing three variables: `date`, `cases` and `reference`.
#' @export
#' @importFrom data.table data.table setDT rbindlist
#' @importFrom lubridate days
simulate_cases <- function(rts, initial_cases, initial_date, generation_interval,
                           rdist = rpois, delay_defs, reporting_effect, 
                           reporting_model, truncate_future = TRUE,
                           type = "sample") {
  
  if (!requireNamespace("EpiSoon", quietly = TRUE)) {
    stop('The EpiSoon package is missing. Install it with: 
         install.packages("drat"); drat:::add("epiforecasts"); install.packages("EpiSoon")')
  }
  
  # simulating cases initialising a data.table
  cases <- data.table::data.table(
    date = initial_date,
    cases = initial_cases)
  
  # structuring rts as a data.table with dates
  rts <- data.table::data.table(
    date = seq(initial_date, initial_date + lubridate::days(length(rts) - 1), by = "days"),
    rt = rts
  )
  
  #  return a dataframe of cases by date of infection
  simulated_cases <- data.table::setDT(
                     EpiSoon::predict_cases(cases = cases,
                                            rt = rts,
                                            serial_interval = generation_interval,
                                            rdist = rdist)
                     )
  
  # mapping with a weekly reporting effect
  report <- EpiNow2::adjust_infection_to_report(simulated_cases,
                                               delay_defs = delay_defs,
                                               reporting_effect = reporting_effect,
                                               reporting_model = reporting_model,
                                               type = type, truncate_future = truncate_future)
  
  # bind in simulated cases with reported cases
  report <- data.table::rbindlist(list(
    simulated_cases[, reference := "infection"],
    report
  ))
  
  # get the median interval to truncate initial cases
  median_interval <- sum(!(cumsum(generation_interval) > 0.5)) + 1
  
  # truncate initial data be length of the median generation interval
  report[date >= initial_date + lubridate::days(median_interval)]
  return(report)
}
