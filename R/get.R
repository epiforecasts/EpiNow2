#' Get Folders with Nowcast Results
#'
#' @param results_dir A character string giving the directory in which results
#'  are stored (as produced by `regional_rt_pipeline`).
#'
#' @return A named character vector containing the results to plot.
#' @export
get_regions <- function(results_dir) {
  
  # Regions to include - based on folder names
  regions <- list.files(results_dir)
  
  ## Put into alphabetical order
  regions <- regions[order(regions)]
  
  names(regions) <- regions
  
  
  return(regions)
}



#' Get a Single Raw Result
#'
#' @param file Character string giving the result files name.
#' @param region Character string giving the region of interest.
#' @param date Target date (in the format `"yyyy-mm-dd`).
#' @param result_dir Character string giving the location of the target directory 
#'
#' @return An R object read in from the targeted .rds file
get_raw_result <- function(file, region, date, 
                           result_dir) {
  file_path <- file.path(result_dir, region, date, file)
  object <- readRDS(file_path)
  
  return(object)
}

#' Get Combined Regional Results
#'
#' @param results_dir A character string indicating the folder containing the `EpiNow2`
#' results to extract.
#' @param date A Character string (in the format "yyyy-mm-dd") indicating the date to extract
#' data for. Defaults to "latest" which finds the latest results available.
#' @param forecast Logical, defaults to `FALSE`. Should forecast results be returned.
#' @return A list of estimates, forecasts and estimated cases by date of report.
#' @export
#' @importFrom purrr map safely
#' @importFrom data.table rbindlist
#' @examples
#'
#'
#' \dontrun{
#' ## Assuming epiforecasts/covid is one repo higher
#' ## Summary results
#' out <- get_regional_results("../test", forecast = TRUE)
#' 
#' out
#' }
#' ## Code
#' get_regional_results
get_regional_results <- function(results_dir, date,
                                 forecast = FALSE) {
  
  ## Assign to latest likely date if not given
  if (missing(date)) {
    date <- "latest"
  }
  
  ## Find all regions
  regions <- list.files(results_dir)
  names(regions) <- regions
  
  load_data <- purrr::safely(EpiNow2::get_raw_result)
  

# Get estimates -----------------------------------------------------------

  get_estimates <- function(samples, summarised) {
    samples <- purrr::map(regions, ~ load_data("estimate_samples.rds", .,
                                               result_dir = results_dir,
                                               date = date)[[1]])
    
    samples <- data.table::rbindlist(samples, idcol = "region")
    
    
    ## Get incidence values and combine
    summarised <- purrr::map(regions, ~ load_data("summarised_estimates.rds", .,
                                                  result_dir = results_dir,
                                                  date = date)[[1]])
    
    summarised <- data.table::rbindlist(summarised, idcol = "region")
    
    out <- list()
    out$samples <- samples
    out$summarised <- summarised
    
    return(out)
  }

  
  out <- list()
  out$estimates <- get_estimates(samples = "forecast_samples.rds",
                                 summarised = "summarised_forecast.rds")
  if (forecast) {
    
   out$forecast <-  get_estimates(samples = "estimate_samples.rds",
                                  summarised = "summarised_estimates.rds")
   
   out$estimated_reported_cases <- get_estimates(samples = "estimated_reported_cases_samples.rds",
                                                 summarised = "summarised_estimated_reported_cases.rds")
  }
  
  
  return(out)
}
