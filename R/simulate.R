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
#' @examples
#' \donttest{
#' if(requireNamespace("EpiSoon")){
#' 
#' library(EpiSoon)
#' 
#' # Define an initial rt vector 
#' rts <- c(rep(2, 20), (2 - 1:15 * 0.1), rep(0.5, 10))
#' rts
#' # Use the mean default generation interval for covid
#' # Generation time
#' generation_defs <- EpiNow2::gamma_dist_def(mean = generation_time$mean,
#'                                           mean_sd = generation_time$mean_sd,
#'                                           sd = generation_time$sd,
#'                                           sd_sd = generation_time$sd_sd,
#'                                           max_value = generation_time$max, samples = 1)
#'                                           
#' generate_pdf <- function(dist, max_value) {
#'    ## Define with 0 day padding
#'    sample_fn <- function(n, ...) {
#'      c(0, EpiNow2::dist_skel(n = n,
#'                  model = dist$model[[1]],
#'                  params = dist$params[[1]],
#'                  max_value = dist$max_value[[1]] - 1, 
#'                  ...))
#'                   }
#'  dist_pdf <- sample_fn(0:(max_value - 1), dist = TRUE, cum = FALSE)
#'  
#'  return(dist_pdf)}
#'  
#'  generation_pdf <- generate_pdf(generation_defs, max_value = generation_defs$max)
#' 
#' # Sample a report delay as a lognormal
#' delay_def <- EpiNow2::lognorm_dist_def(mean = 5, mean_sd = 1,
#'                                       sd = 3, sd_sd = 1, max_value = 30,
#'                                       samples = 1, to_log = TRUE)
#'                                       
#' 
#' # Sample a incubation period (again using the default for covid)
#' incubation_def <- EpiNow2::lognorm_dist_def(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                                           max_value = 30, samples = 1)
#'
#' # Simulate cases with a decrease in reporting at weekends and an increase on Monday                                     
#' simulated_cases <- simulate_cases(rts, initial_cases = 100 , initial_date = as.Date("2020-03-01"),
#'                     generation_interval = generation_pdf,
#'                     delay_defs = list(incubation_def, delay_def),
#'                     reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95))
#'                    
#'print(simulated_cases)
#'
#'
#'
#' # Simulate cases with a weekly reporting effect and 
#' # stochastic noise in reporting (beyond the delay)                                  
#' simulated_cases <- simulate_cases(rts, initial_cases = 100 ,
#'                                   initial_date = as.Date("2020-03-01"),
#'                                   generation_interval = generation_pdf, 
#'                                   delay_defs = list(incubation_def, delay_def)
#'                                   reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95),
#'                                   reporting_model = function(n) {
#'                                       out <- suppressWarnings(rnbinom(length(n), 
#'                                                              as.integer(n), 0.5))
#'                                       out <- ifelse(is.na(out), 0, out)
#'                                   })
#'                    
#'print(simulated_cases)
#'}
#'}
simulate_cases <- function(rts, initial_cases, initial_date, generation_interval,
                           rdist = rpois, delay_defs, reporting_effect, 
                           reporting_model, truncate_future = TRUE,
                           type = "sample") {
  
  if (!requireNamespace("EpiSoon", quietly = TRUE)) {
    stop('The EpiSoon package is missing. Install it with: 
         install.packages("drat"); drat:::add("epiforecasts"); install.packages("EpiSoon")')
  }
  
  # Simulating cases initialising a data.table
  cases <- data.table::data.table(
    date = initial_date,
    cases = initial_cases)
  
  # Structuring rts as a data.table with dates
  rts <- data.table::data.table(
    date = seq(initial_date, initial_date + lubridate::days(length(rts) - 1), by = "days"),
    rt = rts
  )
  
  #  Return a dataframe of cases by date of infection
  simulated_cases <- data.table::setDT(
                     EpiSoon::predict_cases(cases = cases,
                                            rt = rts,
                                            serial_interval = generation_interval,
                                            rdist = rdist)
                     )
  
  # Mapping with a weekly reporting effect
  report <- EpiNow2::adjust_infection_to_report(simulated_cases,
                                               delay_defs = delay_defs,
                                               reporting_effect = reporting_effect,
                                               reporting_model = reporting_model,
                                               type = type, truncate_future = truncate_future)
  
  
  # Bind in simulated cases with reported cases
  report <- data.table::rbindlist(list(
    simulated_cases[, reference := "infection"],
    report
  ))
  
  # Get the median interval to truncate initial cases
  median_interval <- sum(!(cumsum(generation_interval) > 0.5)) + 1
  
  # Truncate initial data be length of the median generation interval
  report[date >= initial_date + lubridate::days(median_interval)]
  
  return(report)
}
