#' Get Folders with Nowcast Results
#'
#' @param results_dir A character string giving the directory in which results
#'  are stored (as produced by `regional_rt_pipeline`).
#'
#' @return A named character vector containing the results to plot.
#' @export
#' @examples
#' \dontrun{
#' See ?regional_summary for code to produce test results
#' get_regions("../test")
#' }
get_regions <- function(results_dir) {
  
  # Regions to include - based on folder names
  regions <- list.files(results_dir, recursive = FALSE)
  
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
#' @export
#' @return An R object read in from the targeted .rds file
#' @examples
#' \dontrun{
#' # see ?regional_summary for code to produce test results
#' get_raw_result("summarised_estimates.rds", 
#'                region = "realland", date = "latest",
#'                result_dir = "../test")
#' }
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
#' # see ?regional_epinow for code to generate regional results to use with this example.
#' 
#' out$summary <- NULL
#' 
#' get_regional_results(out, forecast = TRUE)
#' 
#' }

get_regional_results <- function(regional_output,
                                 results_dir, date,
                                 forecast = FALSE) {
  
  if (missing(regional_output)) {
    regional_output <- NULL
  }
  
  if (is.null(regional_output)) {
    ## Assign to latest likely date if not given
    if (missing(date)) {
      date <- "latest"
    }
    
    ## Find all regions
    regions <- list.files(results_dir, recursive = FALSE)
    names(regions) <- regions
    
    load_data <- purrr::safely(EpiNow2::get_raw_result)
    
    
    # Get estimates -----------------------------------------------------------
    
    get_estimates <- function(samples, summarised) {
      samples <- purrr::map(regions, ~ load_data(samples, .,
                                                 result_dir = results_dir,
                                                 date = date)[[1]])
      
      samples <- data.table::rbindlist(samples, idcol = "region")
      
      
      ## Get incidence values and combine
      summarised <- purrr::map(regions, ~ load_data(summarised, .,
                                                    result_dir = results_dir,
                                                    date = date)[[1]])
      
      summarised <- data.table::rbindlist(summarised, idcol = "region")
      
      out <- list()
      out$samples <- samples
      out$summarised <- summarised
      
      return(out)
    }
    
    
    out <- list()
    out$estimates <- get_estimates(samples = "estimate_samples.rds",
                                   summarised = "summarised_estimates.rds")
    
    if (forecast) {
      
      out$forecast <- get_estimates(samples = "forecast_samples.rds",
                                    summarised = "summarised_forecast.rds")
      
      out$estimated_reported_cases <- get_estimates(samples = "estimated_reported_cases_samples.rds",
                                                    summarised = "summarised_estimated_reported_cases.rds")
    }
  }else{
    
    get_estimates <- function(data) {
      samples <- purrr::map(regional_output, ~ .[[data]]$samples)
      
      samples <- data.table::rbindlist(samples, idcol = "region")
      
      
      ## Get incidence values and combine
      summarised <- purrr::map(regional_output, ~ ~ .[[data]]$summarised)
      
      summarised <- data.table::rbindlist(summarised, idcol = "region")
      
      out <- list()
      out$samples <- samples
      out$summarised <- summarised
    }
    
    out <- list()
    out$estimates <- get_estimates("estimates")
    
    if (forecast) {
      
      out$forecast <- get_estimates("forecast")
      
      out$estimated_reported_cases <- get_estimates("estimated_reported_cases")
    }
  }

  return(out)
}
